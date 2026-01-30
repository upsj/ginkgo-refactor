#include "refactor_rules.hpp"
#include "utils.hpp"

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

using ::clang::CharSourceRange;
using ::clang::SourceRange;
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

namespace {

AST_POLYMORPHIC_MATCHER(
    isFunctionTemplateDefinition,
    AST_POLYMORPHIC_SUPPORTED_TYPES(clang::FunctionTemplateDecl)) {
  return Node.isThisDeclarationADefinition();
}

auto matchDenseParamInForwardDecl(qualifier_mode mode) {
  auto denseTypeMatcher =
      type(hasUnqualifiedDesugaredType(templateSpecializationType(
          hasDeclaration(namedDecl(hasName("::gko::matrix::Dense"))))));
  // there seems to be no easy matcher way of accessing the parmVarDecls of a
  // functionTemplateDecl, so we just use ancestor/descendant queries
  auto execParmVarDecl = parmVarDecl(hasType(executorType()));
  auto isInKernelFunctionFwdDecl = hasAncestor(functionTemplateDecl(
      hasDescendant(execParmVarDecl), isInKernelsNamespace(),
      unless(isFunctionTemplateDefinition())));
  auto typeLoc = densePointerTypeLoc(mode);
  auto matcher = [&](auto... qualifierMatcher) {
    return parmVarDecl(typeLoc, hasType(pointerType(pointee(denseTypeMatcher))),
                       isInKernelFunctionFwdDecl);
  };
  switch (mode) {
  case qualifier_mode::only_const:
    return matcher(isConstQualified());
  case qualifier_mode::only_mutable:
    return matcher(unless(isConstQualified()));
  case qualifier_mode::both:
  default:
    return matcher();
  }
}

auto createRefactorDenseParamRuleWithMacroSupport() {
  return makeRule(traverse(clang::TK_AsIs, matchDenseParamInForwardDecl(
                                               qualifier_mode::only_mutable)),
                  {changeTo(backported::spelled(node(RootID)),
                            cat("matrix::device_view::dense<",
                                backported::spelled(node("vtype")), "> ",
                                backported::name(RootID)))},
                  cat("Rewrite gko::matrix::Dense<...>* to "
                      "matrix::device_view::dense<...> inside "
                      "gko::kernels forward declarations"));
}

auto createRefactorConstDenseParamRuleWithMacroSupport() {
  return makeRule(traverse(clang::TK_AsIs, matchDenseParamInForwardDecl(
                                               qualifier_mode::only_const)),
                  {changeTo(backported::spelled(node(RootID)),
                            cat("matrix::device_view::dense<const ",
                                backported::spelled(node("vtype")), "> ",
                                backported::name(RootID)))},
                  cat("Rewrite const gko::matrix::Dense<...>* to "
                      "matrix::device_view::dense<const "
                      "...> inside gko::kernels forward declarations"));
}

} // namespace

OurRewriteRule createRefactorKernelDeclsDenseToViewRule() {
  return applyFirst({
      createRefactorDenseParamRuleWithMacroSupport(),
      createRefactorConstDenseParamRuleWithMacroSupport(),
  });
}
