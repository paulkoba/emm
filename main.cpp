#include <iostream>
#include <fstream>
#include <sstream>

#include "cpp_helpers.h"
#include "lexer.h"
#include "parser.h"

int main() {
    std::ifstream input("../examples/codegen.emm");
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

    auto llvmContext = std::make_unique<llvm::LLVMContext>();
    module = std::make_unique<llvm::Module>("some_module", *llvmContext);
    llvm::IRBuilder<> builder(*llvmContext);

    for(const auto& e : r) {
        e->codegen(builder);
    }

    module->print(llvm::errs(), nullptr);
    std::cout << "Done" << std::endl;
    return 0;
}
