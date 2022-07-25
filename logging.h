//
// Created by fail on 7/25/22.
//

#ifndef EMMC_LOGGING_H
#define EMMC_LOGGING_H

#include <iostream>

// TODO: This doesn't work

template<typename ...Args>
void compilationError(int64_t line, Args&& ...args) {
    std::cout << "Compilation error at line " << line << ": " << (... << args) << std::endl;
}

template<typename ...Args>
void compilationError(Args&& ...args) {
    (std::cout << ... << args) << std::endl;
}

#endif //EMMC_LOGGING_H
