#include "mock_header.hpp"
#include "refactor_rules.hpp"
#include "test_utils.hpp"

class RefactorKernelDeclsTest : public ClangRefactoringTestBase {
protected:
  RefactorKernelDeclsTest() { appendToHeader(gko_mock_header); }
};

TEST_F(RefactorKernelDeclsTest, DoesNothingOutsideKernelsNamespace) {
  auto Rule = createRefactorKernelDeclsDenseToViewRule();
  std::string Input = R"cc(
namespace gko {

template <typename ValueType>
void kernel(matrix::Dense<ValueType>* foo, const matrix::Dense<ValueType>* bar);

}
  )cc";

  testRule(Rule, Input, Input);
}

TEST_F(RefactorKernelDeclsTest, DoesNothingToOtherTypes) {
  auto Rule = createRefactorKernelDeclsDenseToViewRule();
  std::string Input = R"cc(
namespace gko {

template <typename T>
class Dense {};

template <typename ValueType>
void kernel(Dense<ValueType>* foo, const Dense<ValueType>* bar);

}
  )cc";

  testRule(Rule, Input, Input);
}

TEST_F(RefactorKernelDeclsTest, WorksInMacros) {
  auto Rule = createRefactorKernelDeclsDenseToViewRule();
  std::string Input = R"cc(
namespace gko::kernels {

#define FUNCTION_DECL(ValueType)      \
    void kernel(                         \
      matrix::Dense<ValueType>* foo,  \
      const matrix::Dense<ValueType>* bar)

template <typename ValueType>
FUNCTION_DECL(ValueType);

}
  )cc";
  std::string Expected = R"cc(
namespace gko::kernels {

#define FUNCTION_DECL(ValueType)                  \
    void kernel(                                     \
      matrix::device_view::dense<ValueType> foo,  \
      matrix::device_view::dense<const ValueType> bar)

template <typename ValueType>
FUNCTION_DECL(ValueType);

}
  )cc";

  testRule(Rule, Input, Expected);
}
