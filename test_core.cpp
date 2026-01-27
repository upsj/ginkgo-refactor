#include "mock_header.hpp"
#include "refactor_rules.hpp"
#include "test_utils.hpp"

class RefactorCoreTest : public ClangRefactoringTestBase {
protected:
  RefactorCoreTest() { appendToHeader(gko_mock_header); }
};

TEST_F(RefactorCoreTest, Works) {
  auto Rule = createRefactorCoreDenseToViewRule();
  std::string Input = R"cc(
namespace gko {

Operation make_kernel(gko::matrix::Dense<double>*, gko::matrix::Dense<double>*, const gko::matrix::Dense<double>*, const gko::matrix::Dense<double>*);

void foo() {
    std::shared_ptr<gko::ReferenceExecutor> exec;
    std::shared_ptr<gko::matrix::Dense<double>> mtx;
    gko::matrix::Dense<double>* mtx2;
    std::shared_ptr<const gko::matrix::Dense<double>> mtx3;
    const gko::matrix::Dense<double>* mtx4;
    exec->run(make_kernel(mtx.get(), mtx2, mtx3.get(), mtx4));
}

}
  )cc";
  std::string Expected = R"cc(
namespace gko {

Operation make_kernel(gko::matrix::Dense<double>*, gko::matrix::Dense<double>*, const gko::matrix::Dense<double>*, const gko::matrix::Dense<double>*);

void foo() {
    std::shared_ptr<gko::ReferenceExecutor> exec;
    std::shared_ptr<gko::matrix::Dense<double>> mtx;
    gko::matrix::Dense<double>* mtx2;
    std::shared_ptr<const gko::matrix::Dense<double>> mtx3;
    const gko::matrix::Dense<double>* mtx4;
    exec->run(make_kernel(mtx->get_device_view(), mtx2->get_device_view(), mtx3->get_const_device_view(), mtx4->get_const_device_view()));
}

}
  )cc";

  testRule(Rule, Input, Expected);
}
