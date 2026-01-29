#include "refactor_rules.hpp"
#include "utils.hpp"

#include "clang/AST/ASTTypeTraits.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Tooling/Transformer/RewriteRule.h" // makeRule
#include "clang/Tooling/Transformer/Stencil.h"     // cat

using ::clang::transformer::applyFirst;
using ::clang::transformer::cat;
using ::clang::transformer::changeTo;
using ::clang::transformer::makeRule;
using ::clang::transformer::name;
using ::clang::transformer::node;

using namespace ::clang::ast_matchers;
using namespace matchers;

namespace {

inline auto isInKernelsNamespace() { return isInNamespace("::gko::kernels"); }

inline auto matchDenseKernelParameter(qualifier_mode mode, bool match_typeLoc) {
  return parmVarDecl(isInKernelsNamespace(), densePointerType(mode),
                     densePointerTypeLoc(mode));
}

auto createRefactorConstDenseParamRule() {
  // we need to replace the entire declaration here, because the pointerTypeLoc
  // doesn't capture the const, see
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchDenseKernelParameter(qualifier_mode::only_const, true))
          .bind("param"),
      changeTo(cat("matrix::device_view::dense<const ", node("vtype"), "> ",
                   name("param"))),
      cat("Replacing const Dense<ValueType> by device_view::dense<const "
          "ValueType> "
          "inside gko::kernels namespace"));
  return rule;
}

auto createRefactorDenseParamRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchDenseKernelParameter(qualifier_mode::only_mutable, true)
                   .bind("param")),
      changeTo(cat("matrix::device_view::dense<", node("vtype"), "> ",
                   name("param"))),
      cat("Replacing const Dense<ValueType> by device_view::dense<ValueType> "
          "inside "
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
      cat("Replacing Dense::at by device_view::dense::operator() inside "
          "gko::kernels "
          "namespace"));
  return rule;
}

auto createRefactorDenseGetterRule(std::string_view old_name,
                                   std::string_view new_name) {
  auto rule = makeRule(
      traverse(
          clang::TK_AsIs,
          cxxMemberCallExpr(
              callee(memberExpr(hasDeclaration(namedDecl(hasName(old_name))))),
              on(declRefExpr(hasDeclaration(matchDenseKernelParameter(
                                 qualifier_mode::both, false)))
                     .bind("ref")))),
      changeTo(cat(node("ref"), ".", new_name)),
      cat("Replacing Dense::", old_name, " by device_view::dense::", new_name,
          " inside gko::kernels namespace"));
  return rule;
}

} // namespace

OurRewriteRule createRefactorKernelsDenseToViewRule() {
  return applyFirst({
      createRefactorConstDenseParamRule(),
      createRefactorDenseParamRule(),
      createRefactorDenseAtRule(),
      createRefactorDenseGetterRule("get_size", "size"),
      createRefactorDenseGetterRule("get_stride", "stride"),
      createRefactorDenseGetterRule("get_values", "data"),
      createRefactorDenseGetterRule("get_const_values", "data"),
  });
}
