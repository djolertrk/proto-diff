cmake_minimum_required(VERSION 3.13.4)

project(proto-diff 
     VERSION 1.0
     LANGUAGES CXX C
)

set(CMAKE_CXX_STANDARD 17)

set(PROTO_DIFF_SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
set(PROTO_DIFF_BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

find_package(LLVM REQUIRED CONFIG)
find_package(Clang REQUIRED CONFIG)

message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
include(AddLLVM)

message(STATUS "Using ClangConfig.cmake in: ${CLANG_DIR}")
list(APPEND CMAKE_MODULE_PATH ${LLVM_CMAKE_DIR} ${CLANG_CMAKE_DIR})
include(AddClang)

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(LLVM_LINK_COMPONENTS
  Support
  )

add_compile_options(-fno-rtti)

add_llvm_tool(proto-diff
  proto-diff.cpp
  )

set_target_properties(proto-diff
    PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/bin"
)

target_link_libraries(proto-diff
  PRIVATE
    clangTooling
    clangFrontend
    clangAST
    clangASTMatchers
    clangBasic
    clangLex
)
