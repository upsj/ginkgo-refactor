#include "utils.hpp"

#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "clang-tidy/utils/TransformerClangTidyCheck.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Transformer/RewriteRule.h" // makeRule
#include "clang/Tooling/Transformer/SourceCode.h"
#include "clang/Tooling/Transformer/Stencil.h" // cat
#include "clang/Tooling/Transformer/Transformer.h"
#include <iostream>

namespace {

using ::clang::CharSourceRange;
using ::clang::SourceRange;
using ::clang::StringRef;
using ::clang::tidy::ClangTidyCheckFactories;
using ::clang::tidy::ClangTidyContext;
using ::clang::tidy::ClangTidyModule;
using ::clang::tidy::utils::TransformerClangTidyCheck;
using ::clang::transformer::after;
using ::clang::transformer::applyFirst;
using ::clang::transformer::before;
using ::clang::transformer::cat;
using ::clang::transformer::changeTo;
using ::clang::transformer::enclose;
using ::clang::transformer::makeRule;
using ::clang::transformer::name;
using ::clang::transformer::node;
using ::clang::transformer::RootID;

using namespace ::clang::ast_matchers;
using namespace matchers;

auto inline isInKernelsNamespace() { return isInNamespace("::gko::kernels"); }

AST_POLYMORPHIC_MATCHER(
    isFunctionTemplateDefinition,
    AST_POLYMORPHIC_SUPPORTED_TYPES(clang::FunctionTemplateDecl)) {
  return Node.isThisDeclarationADefinition();
}

auto matchDenseParamInForwardDecl(qualifier_mode mode) {
  auto templateSpecializationMatcher = templateSpecializationTypeLoc(
      hasTemplateArgumentLoc(0, hasTypeLoc(typeLoc().bind("vtype"))));
  auto denseTypeMatcher =
      type(hasUnqualifiedDesugaredType(templateSpecializationType(
          hasDeclaration(namedDecl(hasName("::gko::matrix::Dense"))))));
  auto isInKernelFunctionFwdDecl = hasAncestor(functionTemplateDecl(
      isInKernelsNamespace(), unless(isFunctionTemplateDefinition())));
  auto constLoc = hasTypeLoc(pointerTypeLoc(hasPointeeLoc(
      qualifiedTypeLoc(hasUnqualifiedLoc(templateSpecializationMatcher)))));
  auto mutLoc =
      hasTypeLoc(pointerTypeLoc(hasPointeeLoc(templateSpecializationMatcher)));
  switch (mode) {
  case qualifier_mode::only_const:
    return parmVarDecl(
        constLoc,
        hasType(pointerType(pointee(isConstQualified(), denseTypeMatcher))),
        isInKernelFunctionFwdDecl);
  case qualifier_mode::only_mutable:
    return parmVarDecl(mutLoc,
                       hasType(pointerType(pointee(unless(isConstQualified()),
                                                   denseTypeMatcher))),
                       isInKernelFunctionFwdDecl);
  case qualifier_mode::both:
  default:
    return parmVarDecl(anyOf(constLoc, mutLoc),
                       hasType(pointerType(pointee(denseTypeMatcher))),
                       isInKernelFunctionFwdDecl);
  }
}

static llvm::Error invalidArgumentError(std::string Message) {
  return llvm::make_error<llvm::StringError>(llvm::errc::invalid_argument,
                                             Message);
}

clang::transformer::RangeSelector spelled(clang::transformer::RangeSelector S) {
  return [S](const clang::ast_matchers::MatchFinder::MatchResult &Result)
             -> llvm::Expected<clang::CharSourceRange> {
    llvm::Expected<clang::CharSourceRange> SRange = S(Result);
    if (!SRange)
      return SRange.takeError();
    // fast case: the range is already spelled
    const auto begin = SRange->getBegin();
    const auto end = SRange->getEnd();
    if (begin.isFileID() && end.isFileID()) {
      return *SRange;
    }

    const auto &SM = *Result.SourceManager;
    const auto spelledBegin = SM.getSpellingLoc(begin);
    const auto spelledEnd = SM.getSpellingLoc(end);
    auto result = clang::CharSourceRange(
        clang::SourceRange(spelledBegin, spelledEnd), SRange->isTokenRange());
    std::cout << "Spelled: "
              << clang::Lexer::getSourceText(result, SM,
                                             Result.Context->getLangOpts())
                     .str()
              << '\n';
    result.getBegin().dump(*Result.SourceManager);
    result.getEnd().dump(*Result.SourceManager);
    return result;
  };
}

static llvm::Expected<clang::DynTypedNode>
getNode(const clang::ast_matchers::BoundNodes &Nodes, StringRef ID) {
  auto &NodesMap = Nodes.getMap();
  auto It = NodesMap.find(ID);
  if (It == NodesMap.end())
    return invalidArgumentError("ID not bound: " + ID.str());
  return It->second;
}

clang::transformer::RangeSelector spelledName(std::string ID) {
  return [ID](const clang::ast_matchers::MatchFinder::MatchResult &Result)
             -> llvm::Expected<clang::CharSourceRange> {
    llvm::Expected<clang::DynTypedNode> N = getNode(Result.Nodes, ID);
    if (!N)
      return N.takeError();
    auto &Node = *N;
    if (const auto *D = Node.get<clang::NamedDecl>()) {
      if (!D->getDeclName().isIdentifier())
        return invalidArgumentError(ID + "name" + "identifier");
      clang::SourceLocation L = D->getLocation();
      auto spelledL = Result.SourceManager->getSpellingLoc(L);
      auto R = CharSourceRange::getTokenRange(spelledL, spelledL);
      // Verify that the range covers exactly the name.
      // FIXME: extend this code to support cases like `operator +` or
      // `foo<int>` for which this range will be too short.  Doing so will
      // require subcasing `NamedDecl`, because it doesn't provide virtual
      // access to the \c DeclarationNameInfo.
      StringRef Text = clang::tooling::getText(R, *Result.Context);
      if (Text != D->getName())
        return llvm::make_error<llvm::StringError>(
            llvm::errc::not_supported,
            "range selected by name(node id=" + ID + "): '" + Text +
                "' is different from decl name '" + D->getName() + "'");
      return R;
    }
    return invalidArgumentError(
        ID + "DeclRefExpr, NamedDecl, CXXCtorInitializer, TypeLoc");
  };
}

auto createRefactorDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_mutable)),
      {changeTo(spelled(node(RootID)),
                cat("gko::dense_view<", spelled(node("vtype")), "> ",
                    spelledName(RootID)))},
      cat("Rewrite gko::matrix::Dense<...>* to gko::dense_view<...> inside "
          "gko::kernels forward declarations"));
}

auto createRefactorConstDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_const)),
      {changeTo(spelled(node(RootID)),
                cat("gko::dense_view<const ", spelled(node("vtype")), "> ",
                    spelledName(RootID)))},
      cat("Rewrite const gko::matrix::Dense<...>* to gko::dense_view<const "
          "...> inside gko::kernels forward declarations"));
}

auto createRefactorKernelDeclsDenseToViewRule() {
  return applyFirst({
      createRefactorDenseParamRuleWithMacroSupport(),
      createRefactorConstDenseParamRuleWithMacroSupport(),
  });
}

// Boilerplate

class RefactorKernelDeclsDenseToViewCheck : public TransformerClangTidyCheck {
public:
  RefactorKernelDeclsDenseToViewCheck(StringRef Name, ClangTidyContext *Context)
      : TransformerClangTidyCheck(createRefactorKernelDeclsDenseToViewRule(),
                                  Name, Context) {}
};

class RefactorKernelDeclsDenseToViewModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<RefactorKernelDeclsDenseToViewCheck>(
        "gko-refactor-kernel-decls-dense-to-view");
  }
};

} // namespace

namespace clang::tidy {

// Register the module using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::RefactorKernelDeclsDenseToViewModule>
    refactorKernelDeclsDenseToViewInit(
        "gko-refactor-kernel-decls-dense-to-view",
        "Adds 'gko-refactor-kernel-decls-dense-to-view' checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the module.
volatile int anchor_for_refactor_dense_to_view_kernel_decls = 0;

} // namespace clang::tidy
