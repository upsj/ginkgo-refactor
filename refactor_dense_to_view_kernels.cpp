#include "utils.hpp"

#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "clang-tidy/utils/TransformerClangTidyCheck.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Transformer/RewriteRule.h" // makeRule
#include "clang/Tooling/Transformer/Stencil.h"     // cat

namespace {

using ::clang::StringRef;
using ::clang::tidy::ClangTidyCheckFactories;
using ::clang::tidy::ClangTidyContext;
using ::clang::tidy::ClangTidyModule;
using ::clang::tidy::utils::TransformerClangTidyCheck;
using ::clang::transformer::applyFirst;
using ::clang::transformer::cat;
using ::clang::transformer::changeTo;
using ::clang::transformer::makeRule;
using ::clang::transformer::name;
using ::clang::transformer::node;

using namespace ::clang::ast_matchers;
using namespace matchers;

auto inline isInKernelsNamespace() { return isInNamespace("::gko::kernels"); }

auto inline matchDenseKernelParameter(qualifier_mode mode, bool match_typeLoc) {
  const auto loc_matcher = hasTypeLoc(pointerTypeLoc(
      hasPointeeLoc(typeLoc(hasDescendant(templateSpecializationTypeLoc(
          hasTemplateArgumentLoc(0, hasTypeLoc(typeLoc().bind("vtype")))))))));
  auto type_matcher = hasType(
      instantiatedClassTemplatePointerType("::gko::matrix::Dense", mode));
  return parmVarDecl(isInKernelsNamespace(), type_matcher, loc_matcher);
}

auto createRefactorDenseParamRuleWithMacroSupport() {}

auto createRefactorConstDenseParamRule() {
  // we need to replace the entire declaration here, because the pointerTypeLoc
  // doesn't capture the const, see
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchDenseKernelParameter(qualifier_mode::only_const, true))
          .bind("param"),
      changeTo(
          cat("matrix::dense_view<const ", node("vtype"), "> ", name("param"))),
      cat("Replacing const Dense<ValueType> by dense_view<const ValueType> "
          "inside gko::kernels namespace"));
  return rule;
}

auto createRefactorDenseParamRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchDenseKernelParameter(qualifier_mode::only_mutable, true)
                   .bind("param")),
      changeTo(cat("matrix::dense_view<", node("vtype"), "> ", name("param"))),
      cat("Replacing const Dense<ValueType> by dense_view<ValueType> inside "
          "gko::kernels namespace"));
  return rule;
}

auto createRefactorDenseAtRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               cxxMemberCallExpr(
                   callee(memberExpr(hasDeclaration(namedDecl(hasName("at"))))),
                   on(declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                      qualifier_mode::both, false)))
                          .bind("ref")),
                   hasArgument(0, expr().bind("arg1")),
                   hasArgument(1, expr().bind("arg2")))),
      changeTo(cat(node("ref"), "(", node("arg1"), ", ", node("arg2"), ")")),
      cat("Replacing Dense::at by dense_view::operator() inside gko::kernels "
          "namespace"));
  return rule;
}

auto createRefactorDenseGetSizeRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               cxxMemberCallExpr(
                   callee(memberExpr(
                       hasDeclaration(namedDecl(hasName("get_size"))))),
                   on(declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                      qualifier_mode::both, false)))
                          .bind("ref")))),
      changeTo(cat(node("ref"), ".size")),
      cat("Replacing Dense::get_size by dense_view::size inside gko::kernels "
          "namespace"));
  return rule;
}

auto createRefactorDenseGetStrideRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               cxxMemberCallExpr(
                   callee(memberExpr(
                       hasDeclaration(namedDecl(hasName("get_stride"))))),
                   on(declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                      qualifier_mode::both, false)))
                          .bind("ref")))),
      changeTo(cat(node("ref"), ".stride")),
      cat("Replacing Dense::get_stride by dense_view::stride inside "
          "gko::kernels namespace"));
  return rule;
}

auto createRefactorDenseGetValuesRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               cxxMemberCallExpr(
                   callee(memberExpr(hasDeclaration(namedDecl(anyOf(
                       hasName("get_values"), hasName("get_const_values")))))),
                   on(declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                      qualifier_mode::both, false)))
                          .bind("ref")))),
      changeTo(cat(node("ref"), ".data")),
      cat("Replacing Dense::get_(const_)values by dense_view::data inside "
          "gko::kernels namespace"));
  return rule;
}

auto createRefactorKernelsDenseToViewRule() {
  return applyFirst({
      createRefactorConstDenseParamRule(),
      createRefactorDenseParamRule(),
      createRefactorDenseAtRule(),
      createRefactorDenseGetSizeRule(),
      createRefactorDenseGetValuesRule(),
  });
}

// Boilerplate

class RefactorKernelsDenseToViewCheck : public TransformerClangTidyCheck {
public:
  RefactorKernelsDenseToViewCheck(StringRef Name, ClangTidyContext *Context)
      : TransformerClangTidyCheck(createRefactorKernelsDenseToViewRule(), Name,
                                  Context) {}
};

class RefactorKernelsDenseToViewModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<RefactorKernelsDenseToViewCheck>(
        "gko-refactor-kernels-dense-to-view");
  }
};

} // namespace

namespace clang::tidy {

// Register the module using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::RefactorKernelsDenseToViewModule>
    refactorKernelsDenseToViewInit(
        "gko-refactor-kernels-dense-to-view",
        "Adds 'gko-refactor-kernels-dense-to-view' checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the module.
volatile int anchor_for_refactor_dense_to_view_kernels = 0;

} // namespace clang::tidy
