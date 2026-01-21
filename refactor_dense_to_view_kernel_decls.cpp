#include "utils.hpp"

#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "clang-tidy/utils/TransformerClangTidyCheck.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/AST/DeclTemplate.h"
#include "clang/AST/Type.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Lex/Lexer.h"
#include "clang/Tooling/Transformer/RewriteRule.h" // makeRule
#include "clang/Tooling/Transformer/Stencil.h"     // cat
#include <iostream>

namespace {

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
  auto build_matcher = [](auto... qualifierMatcher) {
    return parmVarDecl(
        hasTypeLoc(pointerTypeLoc(hasPointeeLoc(
            hasDescendant(templateSpecializationTypeLoc(hasTemplateArgumentLoc(
                0, hasTypeLoc(typeLoc().bind("vtype")))))))),
        hasType(pointerType(
            pointee(qualifierMatcher...,
                    type(hasUnqualifiedDesugaredType(
                        templateSpecializationType(hasDeclaration(
                            namedDecl(hasName("::gko::matrix::Dense"))))))))),
        hasAncestor(functionTemplateDecl(
            isInKernelsNamespace(), unless(isFunctionTemplateDefinition()))));
  };
  switch (mode) {
  case qualifier_mode::only_const:
    return build_matcher(isConstQualified());
  case qualifier_mode::only_mutable:
    return build_matcher(unless(isConstQualified()));
  case qualifier_mode::both:
  default:
    return build_matcher();
  }
}

// Returns the full set of expansion locations of `Loc` from bottom to top-most
// macro, if `Loc` is spelled in a macro argument. If `Loc` is spelled in the
// macro definition, returns an empty vector.
static llvm::SmallVector<clang::SourceLocation, 2>
getMacroArgumentExpansionLocs(clang::SourceLocation Loc,
                              const clang::SourceManager &SM) {
  assert(Loc.isMacroID() && "Location must be in a macro");
  llvm::SmallVector<clang::SourceLocation, 2> ArgLocs;
  while (Loc.isMacroID()) {
    const auto &Expansion = SM.getSLocEntry(SM.getFileID(Loc)).getExpansion();
    if (Expansion.isMacroArgExpansion()) {
      // Check the spelling location of the macro arg, in case the arg itself is
      // in a macro expansion.
      Loc = Expansion.getSpellingLoc();
      ArgLocs.push_back(Expansion.getExpansionLocStart());
    } else {
      return {};
    }
  }
  return ArgLocs;
}

static bool spelledInMacroDefinition(clang::CharSourceRange Range,
                                     const clang::SourceManager &SM) {
  if (Range.getBegin().isMacroID() && Range.getEnd().isMacroID()) {
    // Check whether the range is entirely within a single macro argument by
    // checking if they are in the same macro argument at every level.
    auto B = getMacroArgumentExpansionLocs(Range.getBegin(), SM);
    auto E = getMacroArgumentExpansionLocs(Range.getEnd(), SM);
    return B.empty() || B != E;
  }

  return Range.getBegin().isMacroID() || Range.getEnd().isMacroID();
}

clang::transformer::RangeSelector spelled(clang::transformer::RangeSelector S) {
  return [S](const clang::ast_matchers::MatchFinder::MatchResult &Result)
             -> llvm::Expected<clang::CharSourceRange> {
    llvm::Expected<clang::CharSourceRange> SRange = S(Result);
    if (!SRange)
      return SRange.takeError();

    std::cout << "Base: "
              << clang::Lexer::getSourceText(*SRange, *Result.SourceManager,
                                             Result.Context->getLangOpts())
                     .str()
              << '\n'
              << "Expanded: "
              << clang::Lexer::getSourceText(
                     Result.SourceManager->getExpansionRange(*SRange),
                     *Result.SourceManager, Result.Context->getLangOpts())
                     .str()
              << '\n';
    auto begin = SRange->getBegin();
    auto end = SRange->getEnd();
    auto [begin_file, begin_offset] =
        Result.SourceManager->getDecomposedSpellingLoc(begin);
    auto [end_file, end_offset] =
        Result.SourceManager->getDecomposedSpellingLoc(end);
    auto begin_sloc = Result.SourceManager->getSLocEntry(begin_file);
    auto end_sloc = Result.SourceManager->getSLocEntry(end_file);
    auto begin_spelled =
        begin_sloc.isExpansion()
            ? begin_sloc.getExpansion().getSpellingLoc().getLocWithOffset(
                  begin_offset)
            : begin;
    auto end_spelled =
        end_sloc.isExpansion()
            ? end_sloc.getExpansion().getSpellingLoc().getLocWithOffset(
                  end_offset)
            : end;
    std::cout << "Spelled: "
              << clang::Lexer::getSourceText(
                     clang::CharSourceRange(
                         clang::SourceRange(begin_spelled, end_spelled), true),
                     *Result.SourceManager, Result.Context->getLangOpts())
                     .str()
              << '\n';
    return Result.SourceManager->getExpansionRange(*SRange);
  };
}

auto createRefactorDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_mutable)),
      // make sure we only modify the things spelled inside macros,
      // leave the vtype unchanged
      {changeTo(spelled(enclose(before(node(RootID)), before(node("vtype")))),
                cat("gko::dense_view<")),
       changeTo(spelled(enclose(after(node("vtype")), after(node(RootID)))),
                cat("> ", name(RootID)))},
      cat("Rewrite gko::matrix::Dense<...>* to gko::dense_view<...> inside "
          "gko::kernels forward declarations"));
}

auto createRefactorConstDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_const)),
      // make sure we only modify the things spelled inside macros,
      // leave the vtype unchanged
      {changeTo(enclose(before(node(RootID)), before(node("vtype"))),
                cat("gko::dense_view<const ")),
       changeTo(enclose(after(node("vtype")), after(node(RootID))),
                cat("> ", name(RootID)))},
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
