cmake_minimum_required(VERSION 3.18)
project(GinkgoRefactor CXX)
find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)
add_library(RefactorDenseToView MODULE refactor_dense_to_view_core.cpp refactor_dense_to_view_kernels.cpp)
target_compile_options(RefactorDenseToView PRIVATE -fno-rtti)
target_include_directories(RefactorDenseToView
    PRIVATE
        ${CLANG_INCLUDE_DIRS}
        ${LLVM_INCLUDE_DIRS}
)
