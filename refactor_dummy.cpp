#include "utils.hpp"

#include "clang-tidy/ClangTidyModule.h"
#include "clang-tidy/ClangTidyModuleRegistry.h"
#include "clang-tidy/utils/TransformerClangTidyCheck.h"
#include "clang/AST/ASTTypeTraits.h"
#include "clang/ASTMatchers/ASTMatchers.h"
#include "clang/Basic/SourceLocation.h"
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
using ::clang::transformer::applyFirst;
using ::clang::transformer::cat;
using ::clang::transformer::changeTo;
using ::clang::transformer::makeRule;
using ::clang::transformer::name;
using ::clang::transformer::node;

using namespace ::clang::ast_matchers;
using namespace matchers;

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

auto createRefactorDenseGetValuesRule() {
  auto rule = makeRule(
      traverse(clang::TK_AsIs, pointerTypeLoc()),
      changeTo(spelled(node(clang::transformer::RootID)), cat("dummy")),
      cat("Dummy replacement"));
  return rule;
}

auto createDummyRule() {
  return applyFirst({
      createRefactorDenseGetValuesRule(),
  });
}

// Boilerplate

class DummyCheck : public TransformerClangTidyCheck {
public:
  DummyCheck(StringRef Name, ClangTidyContext *Context)
      : TransformerClangTidyCheck(createDummyRule(), Name, Context) {}
};

class RefactorDummyModule : public ClangTidyModule {
public:
  void addCheckFactories(ClangTidyCheckFactories &CheckFactories) override {
    CheckFactories.registerCheck<DummyCheck>("dummy");
  }
};

} // namespace

namespace clang::tidy {

// Register the module using this statically initialized variable.
static ClangTidyModuleRegistry::Add<::RefactorDummyModule>
    refactorDummy("dummy", "Adds 'dummy' checks.");

// This anchor is used to force the linker to link in the generated object file
// and thus register the module.
volatile int anchor_for_refactor_dummy = 0;

} // namespace clang::tidy
