#include <iostream>
#include <fstream>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

#include "cpp_helpers.h"
#include "lexer.h"
#include "parser.h"
#include "optimization_passes.h"
#include "compilation.h"

int main() {
    std::ifstream input("../examples/codegen.emm");
    std::stringstream buffer;
    buffer << input.rdbuf();
    auto tmp = lex(buffer.str());
    std::cout << tmp << std::endl << std::endl;
    auto llvmContext = std::make_unique<llvm::LLVMContext>();
    auto r = parseFile(tmp, std::make_unique<llvm::Module>("some_module", *llvmContext));

    r->populateParents();

    std::ofstream outputAST("output.dot");
    outputAST << "digraph { \n";
    outputAST << r->generateDOTHeader();
    outputAST << r->generateDOT();
    outputAST << "} \n";

    llvm::IRBuilder<> builder(*llvmContext);

    r->codegen(builder);

    r->getModule()->print(llvm::errs(), nullptr);
    optimizeModule(*r->getModule());
    compile(r->getModule());
    return 0;
}
