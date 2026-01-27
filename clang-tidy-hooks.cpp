#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/utils/TransformerClangTidyCheck.h"
#include "refactor_rules.hpp"

using ::clang::StringRef;
using ::clang::tidy::ClangTidyCheckFactories;
using ::clang::tidy::ClangTidyContext;
using ::clang::tidy::ClangTidyModule;
using ::clang::tidy::utils::TransformerClangTidyCheck;

class RefactorCheck : public TransformerClangTidyCheck {
public:
  RefactorCheck(StringRef Name, OurRewriteRule Rule, ClangTidyContext *Context)
      : TransformerClangTidyCheck(Rule, Name, Context) {}
};

class RefactorGinkgoModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    auto add_factory = [&](auto RuleGenerator, std::string_view Name) {
      CheckFactories.registerCheckFactory(Name, [=](llvm::StringRef Name,
                                                    ClangTidyContext *Context) {
        return std::make_unique<RefactorCheck>(Name, RuleGenerator(), Context);
      });
    };
    add_factory(createRefactorKernelsDenseToViewRule,
                "gko-refactor-kernels-dense-to-view");
    add_factory(createRefactorCoreDenseToViewRule,
                "gko-refactor-core-dense-to-view");
    add_factory(createRefactorKernelDeclsDenseToViewRule,
                "gko-refactor-kernel-decls-dense-to-view");
  }
};

namespace clang::tidy {

// Register the module using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::RefactorGinkgoModule>
    refactorGinkgoInit("gko-refactor",
                       "Adds Ginkgo refactoring checks checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the module.
volatile int anchor_for_gko_refactor_checks = 0;

} // namespace clang::tidy
