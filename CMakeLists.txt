cmake_minimum_required(VERSION 3.18)
project(GinkgoRefactor CXX)
find_package(LLVM 23.0 REQUIRED CONFIG)
find_package(Clang 23.0 REQUIRED CONFIG)
add_library(RefactorDenseToView MODULE refactor_dummy.cpp refactor_dense_to_view_core.cpp refactor_dense_to_view_kernels.cpp refactor_dense_to_view_kernel_decls.cpp)
target_compile_options(RefactorDenseToView PRIVATE -fno-rtti)
target_compile_features(RefactorDenseToView PRIVATE cxx_std_20)
target_include_directories(RefactorDenseToView
    PRIVATE
        ${CLANG_INCLUDE_DIRS}
        ${LLVM_INCLUDE_DIRS}
)
