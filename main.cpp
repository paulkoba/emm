#include <iostream>
#include <fstream>
#include <sstream>

#include "cpp_helpers.h"
#include "lexer.h"

int main() {
    std::ifstream input("../examples/example.emm");
    std::stringstream buffer;
    buffer << input.rdbuf();

    std::cout << lex(buffer.str()) << std::endl;

    return 0;
}
