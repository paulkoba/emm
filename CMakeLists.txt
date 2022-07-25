cmake_minimum_required(VERSION 3.22)
project(emmc)

set(CMAKE_CXX_STANDARD 23)
set (CMAKE_C_FLAGS "-Wall")
add_executable(emmc main.cpp token.h lexer.h cpp_helpers.h ast.h parser.h logging.h)
