cmake_minimum_required(VERSION 3.22)
project(emmc)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

set(CMAKE_C_COMPILER clang)
set(CMAKE_CXX_COMPILER clang++)

add_compile_options(-g)

link_libraries()
execute_process(COMMAND llvm-config --libs OUTPUT_VARIABLE LIBS)
execute_process(COMMAND llvm-config --system-libs OUTPUT_VARIABLE SYS_LIBS)
execute_process(COMMAND llvm-config --ldflags OUTPUT_VARIABLE LDF)

string(STRIP ${LIBS} LIBS)
string(STRIP ${SYS_LIBS} SYS_LIBS)
string(STRIP ${LDF} LDF)

link_libraries(${LIBS} ${SYS_LIBS} ${LDF})

execute_process(COMMAND llvm-config --cxxflags OUTPUT_VARIABLE CMAKE_CXX_FLAGS)
string(STRIP ${CMAKE_CXX_FLAGS} CMAKE_CXX_FLAGS)

add_executable(emmc main.cpp Basic/CPPHelpers.h AST/AST.h Basic/Logging.h Types.h OptimizationPasses.h Compilation.h Compilation.h Type.h Value.h TypeRegistry.h Function.h Variable.h Mangling.h AST/BaseASTNode.h AST/CallExprAST.h Parse/Parser.h Lex/Lexer.h Lex/TokenTypes.h Basic/EnumMagic.h Lex/Token.h)