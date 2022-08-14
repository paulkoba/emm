#include <fstream>
#include <iostream>

#include "Compilation.h"
#include "CPPHelpers.h"
#include "Lexer.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"
#include "OptimizationPasses.h"
#include "Parser.h"

int main() {
	std::ifstream input("../examples/operator_overloading.emm");
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

	outputAST.close();

	llvm::IRBuilder<> builder(*llvmContext);

	typeRegistry = std::make_unique<TypeRegistry>(builder);
	r->codegen(builder);

	r->getModule()->print(llvm::errs(), nullptr);
	optimizeModule(*r->getModule());
	compile(r->getModule());
	return 0;
}
