#include <iostream>
#include <fstream>
#include <sstream>

#include "cpp_helpers.h"
#include "lexer.h"
#include "parser.h"

int main() {
    std::ifstream input("../examples/example.emm");
    std::stringstream buffer;
    buffer << input.rdbuf();
    auto tmp = lex(buffer.str());
    std::cout << tmp << std::endl << std::endl;
    auto r = parseEverything(tmp);

    std::ofstream outputAST("output.dot");
    outputAST << "digraph { \n";
    for(const auto& el : r) {
        outputAST << el->generateDOTHeader();
    }
    for(const auto& el : r) {
        outputAST << el->generateDOT();
    }
    outputAST << "} \n";

    return 0;
}
