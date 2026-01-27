cmake_minimum_required(VERSION 3.18)
project(GinkgoRefactor CXX)
find_package(LLVM 23.0 REQUIRED CONFIG)
find_package(Clang 23.0 REQUIRED CONFIG)
find_package(GTest REQUIRED CONFIG)
add_library(RefactorDenseToViewRules
    OBJECT
    refactor_dense_to_view_core.cpp
    refactor_dense_to_view_kernels.cpp
    refactor_dense_to_view_kernel_decls.cpp)
set_property(TARGET RefactorDenseToViewRules PROPERTY POSITION_INDEPENDENT_CODE ON)
target_compile_options(RefactorDenseToViewRules PUBLIC -fno-rtti)
target_compile_features(RefactorDenseToViewRules PUBLIC cxx_std_20)
target_include_directories(RefactorDenseToViewRules
    PUBLIC
        ${CLANG_INCLUDE_DIRS}
        ${LLVM_INCLUDE_DIRS}
)

# clang-tidy plugin
add_library(RefactorDenseToView MODULE clang-tidy-hooks.cpp)
target_link_libraries(RefactorDenseToView PRIVATE RefactorDenseToViewRules)

# tests
add_executable(TestRefactor test_core.cpp test_kernels.cpp test_kernel_decls.cpp)
target_link_libraries(TestRefactor
    PRIVATE
        RefactorDenseToViewRules
        GTest::gtest_main
        # this list is from clang/unittests/Tooling/CMakeLists.txt
        clangAST
        clangASTMatchers
        clangBasic
        clangDependencyScanning
        clangDriver
        clangFormat
        clangFrontend
        clangLex
        clangRewrite
        clangSerialization
        clangTooling
        clangToolingCore
        clangToolingInclusions
        clangToolingInclusionsStdlib
        clangToolingRefactoring
        clangTransformer
        LLVMFrontendOpenMP
        LLVMSupport
        LLVMTargetParser
    )
enable_testing()
add_test(TestRefactor TestRefactor)

