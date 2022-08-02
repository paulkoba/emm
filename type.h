//
// Created by fail on 8/1/22.
//

#ifndef EMMC_TYPE_H
#define EMMC_TYPE_H

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include <utility>

class Type {
	llvm::Type* base = nullptr;	 // Most of the functionality is directly derived from this
	std::string name;

	bool useBuiltinOperators = false;
	bool signedBuiltinOperators = false;

   public:
	Type(llvm::Type* base, std::string name) : base(base), name(std::move(name)) {}
	Type(llvm::Type* base, std::string name, bool useBuiltinOperators)
		: base(base), name(std::move(name)), useBuiltinOperators(useBuiltinOperators) {}
	Type(llvm::Type* base, std::string name, bool useBuiltinOperators, bool signedBuiltinOperators)
		: base(base),
		  name(std::move(name)),
		  useBuiltinOperators(useBuiltinOperators),
		  signedBuiltinOperators(signedBuiltinOperators) {}

	[[nodiscard]] llvm::Type* getBase() { return base; }
	[[nodiscard]] const std::string& getName() const { return name; }
	[[nodiscard]] bool usesBuiltinOperators() const { return useBuiltinOperators; }
	[[nodiscard]] bool usesSignedBuiltinOperators() const { return signedBuiltinOperators; }
};

#endif	// EMMC_TYPE_H
