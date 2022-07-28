//
// Created by fail on 7/25/22.
//

#ifndef EMMC_LOGGING_H
#define EMMC_LOGGING_H

#include <iostream>
#include <stacktrace>

// TODO: This doesn't work

template <typename T>
void compilationError(int64_t line, T arg) {
	std::cout << "Compilation error at line " << line << ": " << arg
			  << std::endl;
}

template <typename T>
void compilationError(T arg) {
	std::cout << arg << std::endl;
}

#endif	// EMMC_LOGGING_H
