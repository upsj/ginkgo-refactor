#include "mock_header.hpp"
#include "refactor_rules.hpp"
#include "test_utils.hpp"

class RefactorKernelsTest : public ClangRefactoringTestBase {
protected:
  RefactorKernelsTest() { appendToHeader(gko_mock_header); }
};

TEST_F(RefactorKernelsTest, DoesNothingOutsideKernelsNamespace) {
  auto Rule = createRefactorKernelsDenseToViewRule();
  std::string Input = R"cc(
namespace gko {

template <typename ValueType>
void kernel(matrix::Dense<ValueType>* foo, const matrix::Dense<ValueType>* bar) {
    foo->get_size();
    bar->get_stride();
    foo->get_values();
    bar->get_const_values();
    foo->at(0, 1);
    bar->at(2, 3);
}

template void kernel<double>(matrix::Dense<double>* foo, const matrix::Dense<double>* bar);

}
  )cc";

  testRule(Rule, Input, Input);
}

TEST_F(RefactorKernelsTest, DoesNothingToOtherTypes) {
  auto Rule = createRefactorKernelsDenseToViewRule();
  std::string Input = R"cc(
namespace gko {

template <typename T>
class Dense {
public:
    int get_size() const;
};

template <typename ValueType>
void kernel(Dense<ValueType>* foo, const Dense<ValueType>* bar) {
    foo->get_size();
    bar->get_size();
}

template void kernel<double>(Dense<double>* foo, const Dense<double>* bar);

}
  )cc";

  testRule(Rule, Input, Input);
}

TEST_F(RefactorKernelsTest, DoesNothingInUninstantiatedCode) {
  auto Rule = createRefactorKernelsDenseToViewRule();
  std::string Input = R"cc(
namespace gko::kernels {    

template <typename ValueType>
void kernel(matrix::Dense<ValueType>* foo, const matrix::Dense<ValueType>* bar) {
    foo->get_size();
    bar->get_stride();
    foo->get_values();
    bar->get_const_values();
    foo->at(0, 1);
    bar->at(2, 3);
}

}
  )cc";

  testRule(Rule, Input, Input);
}

TEST_F(RefactorKernelsTest, WorksInInstantiatedCode) {
  auto Rule = createRefactorKernelsDenseToViewRule();
  std::string Input = R"cc(
namespace gko::kernels {    

template <typename ValueType>
void kernel(matrix::Dense<ValueType>* foo, const matrix::Dense<ValueType>* bar) {
    foo->get_size();
    bar->get_stride();
    foo->get_values();
    bar->get_const_values();
    foo->at(0, 1);
    bar->at(2, 3);
}

// this won't be refactored, but also doesn't need to be,
// because in almost all of Ginkgo's code, it happens inside macros
template void kernel<double>(matrix::Dense<double>* foo, const matrix::Dense<double>* bar);

}
  )cc";
  std::string Expected = R"cc(
namespace gko::kernels {    

template <typename ValueType>
void kernel(matrix::device_view::dense<ValueType> foo, matrix::device_view::dense<const ValueType> bar) {
    foo.size;
    bar.stride;
    foo.data;
    bar.data;
    foo(0, 1);
    bar(2, 3);
}

// this won't be refactored, but also doesn't need to be,
// because in almost all of Ginkgo's code, it happens inside macros
template void kernel<double>(matrix::Dense<double>* foo, const matrix::Dense<double>* bar);

}
  )cc";

  testRule(Rule, Input, Expected);
}