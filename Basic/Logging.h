//
// Created by fail on 7/25/22.
//

#ifndef EMMC_LOGGING_H
#define EMMC_LOGGING_H

#include <csignal>
#include <iostream>

// TODO: This doesn't work

// Those raise sigsegv for now because it makes things easier to debug
// Will need to be removed once compiler is mostly done

template <typename T>
void compilationError(int64_t line, T arg) {
	llvm::errs() << "Compilation error at line " << line << ": " << arg << "\n";
}

template <typename T>
void compilationError(T arg) {
	llvm::errs() << arg << "\n";
}

template <typename T>
void compilationWarning(int64_t line, T arg) {
	llvm::errs() << "Compilation error at line " << line << ": " << arg << "\n";
}

template <typename T>
void compilationWarning(T arg) {
	llvm::errs() << arg << "\n";
}

#endif	// EMMC_LOGGING_H
