//
// Created by fail on 8/14/22.
//

#ifndef EMMC_MANGLING_H
#define EMMC_MANGLING_H

#include <string>
#include <vector>

#include "Lex/Token.h"
#include "Type.h"
#include "TypeRegistry.h"
#include "Value.h"

// TODO: This will need to be completely reworked when we have a proper type system

std::string mangle(const std::string &name, const std::vector<std::string> &types) {
	std::string mangledName = "_E" + std::to_string(name.size()) + name;

	for (auto &type : types) {
		mangledName += getTypeRegistry()->getType(type)->getMangledName();
	}

	return mangledName;
}

std::string manglingCombine(const std::string& name, const std::string& structName) {
    return "_S" + std::to_string(structName.size()) + structName + "_" + name;
}

std::string mangle(const std::string &name, const std::string& structName, const std::vector<std::string> &types) {
    return mangle(manglingCombine(name, structName), types);
}

std::string getBinaryOpName(Value &lhs, Value &rhs, TokenType::TokenType op) {
	auto t1 = lhs.getType()->getName();
	auto t2 = rhs.getType()->getName();
	auto functionName = functionNameFromTokenType(op);
	return mangle(functionName, {t1, t2});
}

#endif	// EMMC_MANGLING_H
