cmake_minimum_required(VERSION 3.22)
project(emmc)

find_package(LLVM REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
separate_arguments(LLVM_DEFINITIONS_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
add_definitions(${LLVM_DEFINITIONS_LIST})

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_C_FLAGS "-Wall")

add_executable(emmc main.cpp token.h lexer.h cpp_helpers.h ast.h parser.h logging.h)

llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(emmc ${llvm_libs})