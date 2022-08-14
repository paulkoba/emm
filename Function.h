//
// Created by fail on 8/2/22.
//

#ifndef EMMC_FUNCTION_H
#define EMMC_FUNCTION_H

#include "Type.h"
#include "Variable.h"

class Function {
	llvm::Function* function = nullptr;
	Type* returnType = nullptr;
	std::vector<Variable> parameters;

   public:
	Function(llvm::Function* function, Type* returnType) : function(function), returnType(returnType) {}
	Function() = default;

	[[nodiscard]] llvm::Function* getFunction() const { return function; }
	[[nodiscard]] Type* getReturnType() const { return returnType; }
};

// TODO: This will need to be done as part of ModuleAST once I get to supporting several modules.
std::unordered_map<llvm::Function*, std::unique_ptr<Function>> functionMap;

static Function* getFunction(llvm::Function* function) { return functionMap[function].get(); }

#endif	// EMMC_FUNCTION_H
