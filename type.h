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

#include "logging.h"

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

    virtual ~Type() = default;

	[[nodiscard]] llvm::Type* getBase() { return base; }
	[[nodiscard]] const std::string& getName() const { return name; }
	[[nodiscard]] bool usesBuiltinOperators() const { return useBuiltinOperators; }
	[[nodiscard]] bool usesSignedBuiltinOperators() const { return signedBuiltinOperators; }

    [[nodiscard]] virtual std::size_t getMemberOffset(const std::string& memberName) {
        compilationError("Cannot get member offset of type " + name);
        return 0;
    }

    [[nodiscard]] virtual Type* getMemberType(const std::string& memberName) {
        compilationError("Cannot get member type of type " + memberName);
        return nullptr;
    }
};

class StructType : public Type {
    std::map<std::string, std::size_t> memberOffsets;
    std::map<std::string, Type*> memberTypes;

    friend class StructAST;
public:
    StructType(llvm::Type* base, std::string name, const std::vector<std::pair<std::string, std::string>>& members, const std::vector<Type*>& types)
        : Type(base, std::move(name), false, false) {
        for (std::size_t i = 0; i < members.size(); i++) {
            memberOffsets[members[i].first] = i;
            memberTypes[members[i].first] = types[i];
        }
    }

    [[nodiscard]] std::size_t getMemberOffset(const std::string& memberName) override {
        return memberOffsets[memberName];
    }

    [[nodiscard]] Type* getMemberType(const std::string& memberName) override {
        return memberTypes[memberName];
    }
};

#endif	// EMMC_TYPE_H
