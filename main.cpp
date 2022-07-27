#include <iostream>
#include <fstream>
#include <sstream>

#include "cpp_helpers.h"
#include "lexer.h"
#include "parser.h"

#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"

#include "optimization_passes.h"
#include "compilation.h"

int main() {
    std::ifstream input("../examples/codegen.emm");
    std::stringstream buffer;
    buffer << input.rdbuf();
    auto tmp = lex(buffer.str());
    std::cout << tmp << std::endl << std::endl;
    auto r = parseEverything(tmp);

    for(const auto& el : r) {
        el->populateParents();
    }

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
    optimizeModule(*module);
    compile(module.get());
    return 0;
}
