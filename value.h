//
// Created by fail on 8/1/22.
//

#ifndef EMMC_VALUE_H
#define EMMC_VALUE_H

#include "type.h"

class Value {
    llvm::Value* value = nullptr;
    Type* type = nullptr;

public:
    Value(llvm::Value* value, Type* type) : value(value), type(type) {}
    Value() = default;

    [[nodiscard]] bool operator!() const { return value == nullptr; }

    [[nodiscard]] llvm::Value* getValue() const { return value; }
    [[nodiscard]] Type* getType() const { return type; }
};

#endif //EMMC_VALUE_H
