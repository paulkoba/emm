#include <fstream>
#include <iostream>

#include "Basic/CPPHelpers.h"
#include "Compilation.h"
#include "Lex/TokenTypes.h"
#include "OptimizationPasses.h"
#include "Parse/Parser.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/Host.h"
#include "llvm/Support/raw_ostream.h"

int main() {
	Lexer lexer("../examples/strings.emm");
	auto llvmContext = std::make_unique<llvm::LLVMContext>();
	Parser parser(&lexer, std::make_unique<llvm::Module>("some_module", *llvmContext));

	auto r = parser.parse();

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


	optimizeModule(*r->getModule());
    r->getModule()->print(llvm::errs(), nullptr);
	compile(r->getModule());

	return 0;
}
