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
using ::clang::transformer::spelled;

using namespace ::clang::ast_matchers;
using namespace matchers;

auto inline isInKernelsNamespace() { return isInNamespace("::gko::kernels"); }

AST_POLYMORPHIC_MATCHER(
    isFunctionTemplateDefinition,
    AST_POLYMORPHIC_SUPPORTED_TYPES(clang::FunctionTemplateDecl)) {
  return Node.isThisDeclarationADefinition();
}

auto matchDenseParamInForwardDecl(qualifier_mode mode) {
  auto denseTypeMatcher =
      type(hasUnqualifiedDesugaredType(templateSpecializationType(
          hasDeclaration(namedDecl(hasName("::gko::matrix::Dense"))))));
  auto isInKernelFunctionFwdDecl = hasAncestor(functionTemplateDecl(
      isInKernelsNamespace(), unless(isFunctionTemplateDefinition())));
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

static llvm::Error invalidArgumentError(std::string Message) {
  return llvm::make_error<llvm::StringError>(llvm::errc::invalid_argument,
                                             Message);
}

auto createRefactorDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_mutable)),
      {changeTo(
          spelled(node(RootID)),
          cat("gko::dense_view<", spelled(node("vtype")), "> ", name(RootID)))},
      cat("Rewrite gko::matrix::Dense<...>* to gko::dense_view<...> inside "
          "gko::kernels forward declarations"));
}

auto createRefactorConstDenseParamRuleWithMacroSupport() {
  return makeRule(
      traverse(clang::TK_AsIs,
               matchDenseParamInForwardDecl(qualifier_mode::only_const)),
      {changeTo(spelled(node(RootID)),
                cat("gko::dense_view<const ", spelled(node("vtype")), "> ",
                    name(RootID)))},
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
