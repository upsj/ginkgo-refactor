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

enum class qualifier_mode { only_const, only_mutable, both };

auto inline isInKernelsNamespace() {
  return hasAncestor(namespaceDecl(hasName("::gko::kernels")));
}

auto inline matchDependentDenseType(qualifier_mode mode) {
  const auto matrix_matcher = hasUnqualifiedDesugaredType(
      classTemplateSpecializationType("::gko::matrix::Dense"));
  switch (mode) {
  case qualifier_mode::only_const:
    return pointerType(pointee(isConstQualified(), matrix_matcher));
  case qualifier_mode::only_mutable:
    return pointerType(pointee(unless(isConstQualified()), matrix_matcher));
  default:
    return pointerType(pointee(matrix_matcher));
  }
}

auto inline matchDenseKernelParameter(const std::string &type_bind,
                                      const std::string &vtype_bind,
                                      qualifier_mode mode) {
  const auto loc_matcher = hasTypeLoc(
      pointerTypeLoc(hasPointeeLoc(typeLoc(hasDescendant(
                         templateSpecializationTypeLoc(hasTemplateArgumentLoc(
                             0, hasTypeLoc(typeLoc().bind(vtype_bind))))))))
          .bind(type_bind));
  auto type_matcher = hasType(matchDependentDenseType(mode));
  return parmVarDecl(isInKernelsNamespace(), type_matcher, loc_matcher);
}

auto createRefactorDenseParamRuleWithMacroSupport() {}

auto createRefactorConstDenseParamRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs, matchDenseKernelParameter(
                                   "type", "vtype", qualifier_mode::only_const))
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
               matchDenseKernelParameter("type", "vtype",
                                         qualifier_mode::only_mutable))
          .bind("param"),
      changeTo(cat("matrix::dense_view<", node("vtype"), "> ", name("param"))),
      cat("Replacing const Dense<ValueType> by dense_view<ValueType> inside "
          "gko::kernels namespace"));
  return rule;
}

auto createRefactorDenseAtRule() {
  auto rule = makeRule(
      traverse(
          clang::TK_AsIs,
          callExpr(
              has(expr(cxxDependentScopeMemberExpr(
                  hasMemberName("at"),
                  hasDescendant(
                      declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                      "type", "vtype", qualifier_mode::both)))
                          .bind("ref"))))),
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
               callExpr(has(expr(cxxDependentScopeMemberExpr(
                   hasMemberName("get_size"),
                   hasDescendant(
                       declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                       "type", "vtype", qualifier_mode::both)))
                           .bind("ref"))))))),
      changeTo(cat(node("ref"), ".size")),
      cat("Replacing Dense::get_size by dense_view::size inside gko::kernels "
          "namespace"));
  return rule;
}

auto createRefactorDenseGetStrideRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               callExpr(has(expr(cxxDependentScopeMemberExpr(
                   hasMemberName("get_stride"),
                   hasDescendant(
                       declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                       "type", "vtype", qualifier_mode::both)))
                           .bind("ref"))))))),
      changeTo(cat(node("ref"), ".stride")),
      cat("Replacing Dense::get_stride by dense_view::stride inside "
          "gko::kernels namespace"));
  return rule;
}

auto createRefactorKernelsDenseToViewRule() {
  return applyFirst({
      createRefactorConstDenseParamRule(),
      createRefactorDenseParamRule(),
      createRefactorDenseAtRule(),
      createRefactorDenseGetSizeRule(),
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
