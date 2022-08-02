//
// Created by fail on 8/2/22.
//

#ifndef EMMC_VARIABLE_H
#define EMMC_VARIABLE_H

#include "value.h"

class Variable {
public:
    std::string name;
    Value value;

public:
    Variable(std::string name, Value value)
            : name(std::move(name)), value(value) {}

    Variable() = default;
};

#endif //EMMC_VARIABLE_H
