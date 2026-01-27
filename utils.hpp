#pragma once

#include <string_view>

#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/Decl.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Tooling/Transformer/RangeSelector.h"
#include "clang/Tooling/Transformer/SourceCode.h"

enum class qualifier_mode { only_const, only_mutable, both };

namespace matchers {

using namespace ::clang::ast_matchers;

/**
 * matches any decl node that is declared inside the given namespace
 */
inline auto isInNamespace(std::string_view name) {
  return hasAncestor(namespaceDecl(hasName(name)));
}

/**
 * matches the type of a pointer (const or non-const or both depending on mode)
 * pointing to an instantiated class template with the given name.
 */
inline auto instantiatedClassTemplatePointerType(std::string_view name,
                                                 qualifier_mode mode) {
  auto dense_class_matcher = recordType(
      hasDeclaration(classTemplateSpecializationDecl(hasName(name))));
  auto pointer_matcher = [&] {
    switch (mode) {
    case qualifier_mode::only_const:
      return pointerType(pointee(isConstQualified(), dense_class_matcher));
    case qualifier_mode::only_mutable:
      return pointerType(
          pointee(unless(isConstQualified()), dense_class_matcher));
    case qualifier_mode::both:
    default:
      return pointerType(pointee(dense_class_matcher));
    }
  }();
  // we need to get the canonical type in case this is the return type of a
  // smart pointer's .get() function, which involves a SubstTemplateTypeParmType
  // wrapper
  return hasCanonicalType(pointer_matcher);
}

inline auto densePointerType(qualifier_mode mode) {
  return hasType(
      instantiatedClassTemplatePointerType("::gko::matrix::Dense", mode));
}

inline auto densePointerTypeLoc(qualifier_mode mode) {
  // due to https://github.com/llvm/llvm-project/issues/177251 we can't use
  // hasDescendant()
  auto templateSpecializationMatcher = templateSpecializationTypeLoc(
      hasTemplateArgumentLoc(0, hasTypeLoc(typeLoc().bind("vtype"))));
  auto constLoc = pointerTypeLoc(hasPointeeLoc(
      qualifiedTypeLoc(hasUnqualifiedLoc(templateSpecializationMatcher))));
  auto mutLoc = pointerTypeLoc(hasPointeeLoc(templateSpecializationMatcher));
  switch (mode) {
  case qualifier_mode::only_const:
    return hasTypeLoc(constLoc);
  case qualifier_mode::only_mutable:
    return hasTypeLoc(mutLoc);
  case qualifier_mode::both:
  default:
    return hasTypeLoc(anyOf(constLoc, mutLoc));
  }
}

inline auto executorType() {
  return hasUnqualifiedDesugaredType(
      recordType(hasDeclaration(classTemplateSpecializationDecl(
          isInStdNamespace(), hasName("shared_ptr"),
          hasTemplateArgument(
              0, templateArgument(refersToType(hasDeclaration(
                     cxxRecordDecl(anyOf(isDerivedFrom("::gko::Executor"),
                                         hasName("::gko::Executor")))))))))));
}

inline auto executorExpr() { return expr(hasType(executorType())); }

inline auto namedMemberExpr(std::string_view name, auto object_expr,
                            auto... other_matchers) {
  return memberExpr(hasDeclaration(namedDecl(hasName(name))),
                    hasObjectExpression(object_expr), other_matchers...);
}

inline auto smartPtrMemberExpr(std::string_view name, auto object_expr) {
  return namedMemberExpr(name, cxxOperatorCallExpr(hasArgument(0, object_expr)),
                         isArrow());
}

inline auto smartPtrGetExpr() {
  return materializeTemporaryExpr(
      has(cxxMemberCallExpr(callee(namedMemberExpr("get", expr())))));
}

AST_MATCHER(clang::CallExpr, isMakeFunction) {
  auto CD = Node.getCalleeDecl();
  if (!CD) {
    return false;
  }
  auto F = CD->getAsFunction();
  if (!F) {
    return false;
  }
  auto I = F->getIdentifier();
  if (!I) {
    // avoid assertion failure inside getName()
    return false;
  }
  return F->getName().starts_with("make_");
}

} // namespace matchers

// backporting https://github.com/llvm/llvm-project/pull/177442
namespace backported {

using clang::ASTNodeKind;
using clang::CharSourceRange;
using clang::CXXCtorInitializer;
using clang::DeclRefExpr;
using clang::DynTypedNode;
using clang::NamedDecl;
using clang::SourceLocation;
using clang::SourceRange;
using clang::TemplateSpecializationTypeLoc;
using clang::TypeLoc;
using clang::ast_matchers::MatchFinder;
using clang::transformer::RangeSelector;
using llvm::Error;
using llvm::Expected;
using llvm::StringError;
using llvm::StringRef;
using llvm::Twine;
using namespace clang::tooling;

static Error invalidArgumentError(Twine Message) {
  return llvm::make_error<StringError>(llvm::errc::invalid_argument, Message);
}

static Error typeError(StringRef ID, const ASTNodeKind &Kind) {
  return invalidArgumentError("mismatched type (node id=" + ID +
                              " kind=" + Kind.asStringRef() + ")");
}

static Error typeError(StringRef ID, const ASTNodeKind &Kind,
                       Twine ExpectedType) {
  return invalidArgumentError("mismatched type: expected one of " +
                              ExpectedType + " (node id=" + ID +
                              " kind=" + Kind.asStringRef() + ")");
}

static Error missingPropertyError(StringRef ID, Twine Description,
                                  StringRef Property) {
  return invalidArgumentError(Description + " requires property '" + Property +
                              "' (node id=" + ID + ")");
}

static Expected<DynTypedNode>
getNode(const clang::ast_matchers::BoundNodes &Nodes, StringRef ID) {
  auto &NodesMap = Nodes.getMap();
  auto It = NodesMap.find(ID);
  if (It == NodesMap.end())
    return invalidArgumentError("ID not bound: " + ID);
  return It->second;
}

inline RangeSelector name(std::string ID) {
  return [ID](const MatchFinder::MatchResult &Result)
             -> Expected<CharSourceRange> {
    Expected<DynTypedNode> N = getNode(Result.Nodes, ID);
    if (!N)
      return N.takeError();
    auto &Node = *N;
    if (const auto *D = Node.get<NamedDecl>()) {
      if (!D->getDeclName().isIdentifier())
        return missingPropertyError(ID, "name", "identifier");
      SourceLocation L = D->getLocation();
      // the name may be spelled in a macro
      L = Result.SourceManager->getSpellingLoc(L);
      auto R = CharSourceRange::getTokenRange(L, L);
      // Verify that the range covers exactly the name.
      // FIXME: extend this code to support cases like `operator +` or
      // `foo<int>` for which this range will be too short.  Doing so will
      // require subcasing `NamedDecl`, because it doesn't provide virtual
      // access to the \c DeclarationNameInfo.
      StringRef Text = getText(R, *Result.Context);
      if (Text != D->getName())
        return llvm::make_error<StringError>(
            llvm::errc::not_supported,
            "range selected by name(node id=" + ID + "): '" + Text +
                "' is different from decl name '" + D->getName() + "'");
      return R;
    }
    if (const auto *E = Node.get<DeclRefExpr>()) {
      if (!E->getNameInfo().getName().isIdentifier())
        return missingPropertyError(ID, "name", "identifier");
      SourceLocation L = E->getLocation();
      return CharSourceRange::getTokenRange(L, L);
    }
    if (const auto *I = Node.get<CXXCtorInitializer>()) {
      if (!I->isMemberInitializer() && I->isWritten())
        return missingPropertyError(ID, "name", "explicit member initializer");
      SourceLocation L = I->getMemberLocation();
      return CharSourceRange::getTokenRange(L, L);
    }
    if (const auto *T = Node.get<TypeLoc>()) {
      if (auto SpecLoc = T->getAs<TemplateSpecializationTypeLoc>();
          !SpecLoc.isNull())
        return CharSourceRange::getTokenRange(SpecLoc.getTemplateNameLoc());
      return CharSourceRange::getTokenRange(T->getSourceRange());
    }
    return typeError(ID, Node.getNodeKind(),
                     "DeclRefExpr, NamedDecl, CXXCtorInitializer, TypeLoc");
  };
}

inline RangeSelector spelled(RangeSelector S) {
  return
      [S](const MatchFinder::MatchResult &Result) -> Expected<CharSourceRange> {
        Expected<CharSourceRange> SRange = S(Result);
        if (!SRange)
          return SRange.takeError();
        const auto &SM = *Result.SourceManager;
        const auto B = SRange->getBegin();
        const auto E = SRange->getEnd();
        return CharSourceRange(
            SourceRange(SM.getSpellingLoc(B), SM.getSpellingLoc(E)),
            SRange->isTokenRange());
      };
}

} // namespace backported