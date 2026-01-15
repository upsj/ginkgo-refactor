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

AST_MATCHER(clang::CallExpr, isMakeFunction) {
  return Node.getCalleeDecl()->getAsFunction()->getName().starts_with("make_");
}

enum class qualifier_mode { only_const, only_mutable, both };

auto inline matchMaterializedDenseType(qualifier_mode mode) {
  auto dense_class_matcher = recordType(hasDeclaration(
      classTemplateSpecializationDecl(hasName("::gko::matrix::Dense"))));
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

auto inline matchExecutorExpr() {
  return expr(hasType(elaboratedType(namesType(
      templateSpecializationType(hasDeclaration(classTemplateSpecializationDecl(
          isInStdNamespace(), hasName("shared_ptr"),
          hasTemplateArgument(
              0,
              templateArgument(refersToType(qualType(hasDeclaration(
                  cxxRecordDecl(anyOf(isDerivedFrom("::gko::Executor"),
                                      hasName("::gko::Executor")))))))))))))));
}

auto inline matchRawPtrDenseKernelArgument(qualifier_mode mode) {
  return cxxMemberCallExpr(
      callee(memberExpr(hasDeclaration(namedDecl(hasName("run"))),
                        hasObjectExpression(cxxOperatorCallExpr(
                            hasArgument(0, matchExecutorExpr()))))),
      hasArgument(0, callExpr(isMakeFunction(),
                              forEachArgumentWithParamType(
                                  expr().bind("expr"),
                                  referenceType(pointee(
                                      matchMaterializedDenseType(mode)))))));
}

auto inline matchSmartPtrDenseKernelArgument(qualifier_mode mode) {
  return cxxMemberCallExpr(
      callee(memberExpr(hasDeclaration(namedDecl(hasName("run"))),
                        hasObjectExpression(cxxOperatorCallExpr(
                            hasArgument(0, matchExecutorExpr()))))),
      hasArgument(
          0,
          callExpr(
              isMakeFunction(),
              forEachArgumentWithParamType(
                  materializeTemporaryExpr(
                      cxxMemberCallExpr(memberExpr(
                          hasDeclaration(namedDecl(hasName("get"))),
                          hasObjectExpression(expr().bind("smart_ptr")))))
                      .bind("expr"),
                  referenceType(pointee(matchMaterializedDenseType(mode)))))));
}

auto createRefactorSmartPtrDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchSmartPtrDenseKernelArgument(qualifier_mode::only_mutable)),
      changeTo(node("expr"), cat(node("smart_ptr"), "->get_device_view()")),
      cat("Replacing smart_ptr Dense arguments by dense_view"));
  return rule;
}

auto createRefactorSmartPtrConstDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchSmartPtrDenseKernelArgument(qualifier_mode::only_const)),
      changeTo(node("expr"),
               cat(node("smart_ptr"), "->get_const_device_view()")),
      cat("Replacing smart_ptr const Dense arguments by const dense_view"));
  return rule;
}

auto createRefactorRawPtrDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchRawPtrDenseKernelArgument(qualifier_mode::only_mutable)),
      changeTo(node("expr"), cat(node("expr"), "->get_device_view()")),
      cat("Replacing Dense arguments by dense_view"));
  return rule;
}

auto createRefactorRawPtrConstDenseKernelArgument() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs,
               matchRawPtrDenseKernelArgument(qualifier_mode::only_const)),
      changeTo(node("expr"), cat(node("expr"), "->get_const_device_view()")),
      cat("Replacing const Dense arguments by const dense_view"));
  return rule;
}

auto createRefactorCoreDenseToViewRule() {
  return applyFirst({
      // smart pointers before raw pointers, as the pattern is more specific
      createRefactorSmartPtrDenseKernelArgument(),
      createRefactorSmartPtrConstDenseKernelArgument(),
      createRefactorRawPtrDenseKernelArgument(),
      createRefactorRawPtrConstDenseKernelArgument(),
  });
}

// Boilerplate

class RefactorCoreDenseToViewCheck : public TransformerClangTidyCheck {
public:
  RefactorCoreDenseToViewCheck(StringRef Name, ClangTidyContext *Context)
      : TransformerClangTidyCheck(createRefactorCoreDenseToViewRule(), Name,
                                  Context) {}
};

class RefactorCoreDenseToViewModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<RefactorCoreDenseToViewCheck>(
        "gko-refactor-core-dense-to-view");
  }
};

} // namespace

namespace clang::tidy {

// Register the module using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::RefactorCoreDenseToViewModule>
    refactorCoreDenseToViewInit(
        "gko-refactor-core-dense-to-view",
        "Adds 'gko-refactor-core-dense-to-view' checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the module.
volatile int anchor_for_refactor_dense_to_view_core = 0;

} // namespace clang::tidy
