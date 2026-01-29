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
using ::clang::transformer::node;

using namespace ::clang::ast_matchers;

using namespace matchers;

namespace {

inline auto isKernelCall() {
  return callExpr(anyOf(
      callExpr(isMakeFunction(),
               hasAncestor(cxxMemberCallExpr(
                   callee(smartPtrMemberExpr("run", executorExpr()))))),
      callExpr(hasDeclaration(functionDecl(
          isInKernelsNamespace(), hasParameter(0, hasType(executorType())))))));
}

inline auto matchRawPtrDenseKernelArgument(qualifier_mode mode) {
  return expr(densePointerType(mode), unless(smartPtrGetExpr()),
              hasParent(isKernelCall()))
      .bind("expr");
}

inline auto matchSmartPtrDenseKernelArgument(qualifier_mode mode) {
  return cxxMemberCallExpr(
             densePointerType(mode),
             callee(memberExpr(hasDeclaration(namedDecl(hasName("get"))),
                               hasObjectExpression(expr().bind("smart_ptr")))),
             hasAncestor(isKernelCall()))
      .bind("expr");
}

auto createRefactorSmartPtrDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchSmartPtrDenseKernelArgument(qualifier_mode::only_mutable)),
      changeTo(node("expr"), cat(node("smart_ptr"), "->get_device_view()")),
      cat("Replacing smart_ptr Dense arguments by device_view::dense"));
  return rule;
}

auto createRefactorSmartPtrConstDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchSmartPtrDenseKernelArgument(qualifier_mode::only_const)),
      changeTo(node("expr"),
               cat(node("smart_ptr"), "->get_const_device_view()")),
      cat("Replacing smart_ptr const Dense arguments by const "
          "device_view::dense"));
  return rule;
}

auto createRefactorRawPtrDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchRawPtrDenseKernelArgument(qualifier_mode::only_mutable)),
      changeTo(node("expr"), cat(node("expr"), "->get_device_view()")),
      cat("Replacing Dense arguments by device_view::dense"));
  return rule;
}

auto createRefactorRawPtrConstDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchRawPtrDenseKernelArgument(qualifier_mode::only_const)),
      changeTo(node("expr"), cat(node("expr"), "->get_const_device_view()")),
      cat("Replacing const Dense arguments by const device_view::dense"));
  return rule;
}

} // namespace

OurRewriteRule createRefactorCoreDenseToViewRule() {
  return applyFirst({
      // smart pointers before raw pointers, as the pattern is more specific
      createRefactorSmartPtrDenseKernelArgument(),
      createRefactorSmartPtrConstDenseKernelArgument(),
      createRefactorRawPtrDenseKernelArgument(),
      createRefactorRawPtrConstDenseKernelArgument(),
  });
}
