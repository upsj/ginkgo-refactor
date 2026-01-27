// Adapted from clang/unittest/Tooling/TransformerTest.cpp
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//

#include "clang/Tooling/Tooling.h"
#include "clang/Tooling/Transformer/RangeSelector.h"
#include "clang/Tooling/Transformer/RewriteRule.h"
#include "clang/Tooling/Transformer/Stencil.h"
#include "clang/Tooling/Transformer/Transformer.h"
#include "llvm/Support/Error.h"
#include "gtest/gtest.h"
#include <optional>

using namespace clang;
using namespace tooling;
using namespace ast_matchers;

using ::clang::transformer::addInclude;
using ::clang::transformer::applyFirst;
using ::clang::transformer::before;
using ::clang::transformer::cat;
using ::clang::transformer::changeTo;
using ::clang::transformer::editList;
using ::clang::transformer::makeRule;
using ::clang::transformer::member;
using ::clang::transformer::name;
using ::clang::transformer::node;
using ::clang::transformer::noEdits;
using ::clang::transformer::remove;
using ::clang::transformer::rewriteDescendants;
using ::clang::transformer::RewriteRule;
using ::clang::transformer::RewriteRuleWith;
using ::clang::transformer::statement;

static std::string format(StringRef Code) {
  const std::vector<Range> Ranges(1, Range(0, Code.size()));
  auto Style = format::getLLVMStyle();
  const auto Replacements = format::reformat(Style, Code, Ranges);
  auto Formatted = applyAllReplacements(Code, Replacements);
  if (!Formatted) {
    ADD_FAILURE() << "Could not format code: "
                  << llvm::toString(Formatted.takeError());
    return std::string();
  }
  return *Formatted;
}

static void compareSnippets(StringRef Expected,
                            const std::optional<std::string> &MaybeActual) {
  ASSERT_TRUE(MaybeActual) << "Rewrite failed. Expecting: " << Expected.str();
  auto Actual = *MaybeActual;
  std::string HL = "#include \"header.h\"\n";
  auto I = Actual.find(HL);
  if (I != std::string::npos)
    Actual.erase(I, HL.size());
  EXPECT_EQ(::format(Expected), ::format(Actual));
}

// FIXME: consider separating this class into its own file(s).
class ClangRefactoringTestBase : public testing::Test {
protected:
  void appendToHeader(StringRef S) { FileContents[0].second += S; }

  void addFile(StringRef Filename, StringRef Content) {
    FileContents.emplace_back(std::string(Filename), std::string(Content));
  }

  std::optional<std::string> rewrite(StringRef Input) {
    std::string Code = ("#include \"header.h\"\n" + Input).str();
    auto Factory = newFrontendActionFactory(&MatchFinder);
    if (!runToolOnCodeWithArgs(
            Factory->create(), Code, std::vector<std::string>(), "input.cc",
            "clang-tool", std::make_shared<PCHContainerOperations>(),
            FileContents)) {
      llvm::errs() << "Running tool failed.\n";
      return std::nullopt;
    }
    if (ErrorCount != 0) {
      llvm::errs() << "Generating changes failed.\n";
      return std::nullopt;
    }
    auto ChangedCode =
        applyAtomicChanges("input.cc", Code, Changes, ApplyChangesSpec());
    if (!ChangedCode) {
      llvm::errs() << "Applying changes failed: "
                   << llvm::toString(ChangedCode.takeError()) << "\n";
      return std::nullopt;
    }
    return *ChangedCode;
  }

  Transformer::ChangeSetConsumer consumer() {
    return [this](Expected<MutableArrayRef<AtomicChange>> C) {
      if (C) {
        Changes.insert(Changes.end(), std::make_move_iterator(C->begin()),
                       std::make_move_iterator(C->end()));
      } else {
        // FIXME: stash this error rather than printing.
        llvm::errs() << "Error generating changes: "
                     << llvm::toString(C.takeError()) << "\n";
        ++ErrorCount;
      }
    };
  }

  auto consumerWithStringMetadata() {
    return [this](Expected<TransformerResult<std::string>> C) {
      if (C) {
        Changes.insert(Changes.end(),
                       std::make_move_iterator(C->Changes.begin()),
                       std::make_move_iterator(C->Changes.end()));
        StringMetadata.push_back(std::move(C->Metadata));
      } else {
        // FIXME: stash this error rather than printing.
        llvm::errs() << "Error generating changes: "
                     << llvm::toString(C.takeError()) << "\n";
        ++ErrorCount;
      }
    };
  }

  void testRule(RewriteRule Rule, StringRef Input, StringRef Expected) {
    Transformers.push_back(
        std::make_unique<Transformer>(std::move(Rule), consumer()));
    Transformers.back()->registerMatchers(&MatchFinder);
    compareSnippets(Expected, rewrite(Input));
  }

  void testRule(RewriteRuleWith<std::string> Rule, StringRef Input,
                StringRef Expected) {
    Transformers.push_back(std::make_unique<Transformer>(
        std::move(Rule), consumerWithStringMetadata()));
    Transformers.back()->registerMatchers(&MatchFinder);
    compareSnippets(Expected, rewrite(Input));
  }

  void testRuleFailure(RewriteRule Rule, StringRef Input) {
    Transformers.push_back(
        std::make_unique<Transformer>(std::move(Rule), consumer()));
    Transformers.back()->registerMatchers(&MatchFinder);
    ASSERT_FALSE(rewrite(Input)) << "Expected failure to rewrite code";
  }

  void testRuleFailure(RewriteRuleWith<std::string> Rule, StringRef Input) {
    Transformers.push_back(std::make_unique<Transformer>(
        std::move(Rule), consumerWithStringMetadata()));
    Transformers.back()->registerMatchers(&MatchFinder);
    ASSERT_FALSE(rewrite(Input)) << "Expected failure to rewrite code";
  }

  // Transformers are referenced by MatchFinder.
  std::vector<std::unique_ptr<Transformer>> Transformers;
  clang::ast_matchers::MatchFinder MatchFinder;
  // Records whether any errors occurred in individual changes.
  int ErrorCount = 0;
  AtomicChanges Changes;
  std::vector<std::string> StringMetadata;

private:
  FileContentMappings FileContents = {{"header.h", ""}};
};
