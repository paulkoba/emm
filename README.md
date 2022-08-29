# E--

This repository contains a compiler for E-- &ndash; a toy programming language. The compiler is written in C++ and uses LLVM for code generation. It supports structures, operator overloading, string literals, extern C functions, pointer logic, signed, unsigned & floating point arithmetic with a proper type casting system, and many more features.

Disclaimer: E-- was not designed for any real use, and was purely written as a learning opportunity.

# Build process

```
cmake CMakeLists.txt
make
```

# Usage example

To compile, link, and run an example application you can use the following commands:

```
./emmc examples/string_implementation.emm -o output.o
gcc output.o -o output
./output
```

# Compilation parameters

| Option name                      | Description                                                   |
|----------------------------------|---------------------------------------------------------------|
| -o                               | Set output file destination                                   |
| -O0, -O, -O1, -O2, -O3, -Os, -Oz | Set an optimization level in a way similar to C/C++ compilers |
| -emit-llvm                       | Emit LLVM IR to a separate file                               |
| -generate-ast                    | Emit AST in a .dot graph format to a separate file            |
