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

int main(int argc, char** argv) {
    std::vector<std::string> arguments(argv + 1, argv + argc);

    std::string inputFileLocation;
    std::string outputFileLocation;
    llvm::OptimizationLevel optLevel = llvm::OptimizationLevel::O0;
    bool expectOutputFileLocation = false;
    bool emitLLVM = false;
    bool shouldContinue = true;
    bool generateASTTree = false;

    if(arguments.empty()) {
        compilationError("No command line arguments supplied to the compiler");
        shouldContinue = false;
    }

    // TODO: This will need to be done efficiently, now all the parameters are kinda hardcoded
    for(const auto& el : arguments) {
        if(el.size() >= 2 && el[0] == '-' && expectOutputFileLocation) {
            compilationError("Expected output file location, got \"" + el + "\"");
            shouldContinue = false;
            break;
        }

        if(el == "-emit-llvm") {
            emitLLVM = true;
            continue;
        }

        if(el == "-generate-ast") {
            generateASTTree = true;
            continue;
        }

        if(el == "-O") {
            optLevel = llvm::OptimizationLevel::O1;
            continue;
        }

        if(el.size() == 3 && el[0] == '-') {
            if(el == "-O0") {
                optLevel = llvm::OptimizationLevel::O0;
            } else if(el == "-O1") {
                optLevel = llvm::OptimizationLevel::O1;
            } else if(el == "-O2") {
                optLevel = llvm::OptimizationLevel::O2;
            } else if(el == "-O3") {
                optLevel = llvm::OptimizationLevel::O3;
            } else if(el == "-Oz") {
                optLevel = llvm::OptimizationLevel::Oz;
            } else if(el == "-Os") {
                optLevel = llvm::OptimizationLevel::Os;
            } else {
                compilationError("Unknown compilation flag \"" + el + "\"");
                shouldContinue = false;
                break;
            }

            continue;
        }

        if(el == "-o") {
            expectOutputFileLocation = true;
            continue;
        }

        if(!el.empty() && el[0] != '-') {
            if(expectOutputFileLocation) {
                outputFileLocation = el;
                expectOutputFileLocation = false;
                continue;
            } else {
                if(inputFileLocation.empty()) {
                    inputFileLocation = el;
                    continue;
                } else {
                    compilationError("Multiple file compilation not yet supported.");
                    shouldContinue = false;
                    break;
                }
            }
        } else {
            compilationError("Unknown compilation flag \"" + el + "\"");
            shouldContinue = false;
            break;
        }
    }

    if(shouldContinue && expectOutputFileLocation) {
        compilationError("Expected output file location.");
        shouldContinue = false;
    }

    if(!shouldContinue) {
        compilationError("Couldn't parse compilation arguments. Halting...");
        return 0;
    }

	Lexer lexer(inputFileLocation.c_str());
	auto llvmContext = std::make_unique<llvm::LLVMContext>();
	Parser parser(&lexer, std::make_unique<llvm::Module>("some_module", *llvmContext));

	auto r = parser.parse();

	r->populateParents();

    if(generateASTTree) {
        std::ofstream outputAST("output.dot");
        outputAST << "digraph { \n";
        outputAST << r->generateDOTHeader();
        outputAST << r->generateDOT();
        outputAST << "} \n";

        outputAST.close();
    }

	llvm::IRBuilder<> builder(*llvmContext);

	typeRegistry = std::make_unique<TypeRegistry>(builder);
	r->codegen(builder);

	optimizeModule(*r->getModule(), optLevel);

    if(emitLLVM) {
        std::error_code ec;
        llvm::raw_fd_ostream dest("output.ll", ec, llvm::sys::fs::OF_None);
        r->getModule()->print(dest, nullptr);
        dest.flush();
    }

	compile(r->getModule(), outputFileLocation);

	return 0;
}
