//
// Created by fail on 8/14/22.
//

#ifndef EMMC_MANGLING_H
#define EMMC_MANGLING_H

#include <vector>
#include <string>

#include "Value.h"
#include "Type.h"
#include "TypeRegistry.h"
#include "Token.h"

std::string mangle(const std::string &name, const std::vector<std::string> &types) {
    std::string mangledName = "_E" + std::to_string(name.size()) + name;

    for(auto &type : types) {
        mangledName += getTypeRegistry()->getType(type)->getMangledName();
    }

    return mangledName;
}

std::string getBinaryOpName(Value& lhs, Value& rhs, TokenType op) {
    auto t1 = lhs.getType()->getName();
    auto t2 = rhs.getType()->getName();
    auto functionName = functionNameFromTokenType(op);
    return mangle(functionName, {t1, t2});
}

#endif //EMMC_MANGLING_H
