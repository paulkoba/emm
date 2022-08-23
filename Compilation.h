//
// Created by fail on 7/27/22.
//

#ifndef EMMC_COMPILATION_H
#define EMMC_COMPILATION_H

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

int compile(llvm::Module* module) {
	// Initialize the target registry etc.
	llvm::InitializeAllTargetInfos();
	llvm::InitializeAllTargets();
	llvm::InitializeAllTargetMCs();
	llvm::InitializeAllAsmParsers();
	llvm::InitializeAllAsmPrinters();

	auto targetTriple = llvm::sys::getDefaultTargetTriple();
	module->setTargetTriple(targetTriple);

	std::string Error;
	auto target = llvm::TargetRegistry::lookupTarget(targetTriple, Error);

	// Print an error and exit if we couldn't find the requested target.
	// This generally occurs if we've forgotten to initialise the
	// TargetRegistry, or we have a bogus target triple.
	if (!target) {
		llvm::errs() << Error;
		return 1;
	}

	auto cpu = "generic";
	auto features = "";

	llvm::TargetOptions opt;
	auto rm = llvm::Optional<llvm::Reloc::Model>();
	auto theTargetMachine = target->createTargetMachine(targetTriple, cpu, features, opt, rm);

	module->setDataLayout(theTargetMachine->createDataLayout());

	auto filename = "output.o";
	std::error_code ec;
	llvm::raw_fd_ostream dest(filename, ec, llvm::sys::fs::OF_None);

	if (ec) {
		llvm::errs() << "Could not open file: " << ec.message();
		return 1;
	}

	llvm::legacy::PassManager pass;
	auto fileType = llvm::CGFT_ObjectFile;

	if (theTargetMachine->addPassesToEmitFile(pass, dest, nullptr, fileType)) {
		llvm::errs() << "theTargetMachine can't emit a file of this type";
		return 1;
	}

	pass.run(*module);
	dest.flush();

	return 0;
}

#endif	// EMMC_COMPILATION_H
