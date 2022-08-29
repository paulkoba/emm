//
// Created by fail on 8/14/22.
//

#ifndef EMMC_CALLEXPRAST_H
#define EMMC_CALLEXPRAST_H

#include "../Mangling.h"
#include "BaseASTNode.h"

class CallExprAST : public BaseASTNode {
	std::string name;
	std::vector<std::unique_ptr<BaseASTNode>> args;
	bool shouldMangle = false;
	friend class StructCallExprAST;

   public:
	CallExprAST(std::string name, std::vector<std::unique_ptr<BaseASTNode>> args, bool shouldMangle = false)
		: name(std::move(name)), args(std::move(args)), shouldMangle(shouldMangle) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		for (const auto &arg : args) {
			output += arg->generateDOTHeader();
		}
		return output + std::to_string((int64_t)this) + " [label=\"CallExprAST\"]\n";
	}

	std::string generateDOT() override {
		std::string output;
		for (const auto &arg : args) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)arg.get()) + "\n";
			output += arg->generateDOT();
		}
		return output;
	}

	void populateParents() override {
		for (const auto &arg : args) {
			arg->parent = this;
			arg->populateParents();
		}
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		std::vector<llvm::Value *> oArgs;
		std::vector<std::string> oArgsTypes;

		for (const auto &arg : args) {
			auto r = arg->codegen(builder);
			oArgs.push_back(r.getValue());
			oArgsTypes.push_back(r.getType()->getName());
		}

		if (shouldMangle) {
			name = mangle(name, oArgsTypes);
		}

		auto func = getModule()->getFunction(name);
		if (!func) {
			compilationError("Function " + name + " not found");
			return {};
		}

		return {builder.CreateCall(func, oArgs), getFunction(func)->getReturnType()};
	}
};

class StructCallExprAST : public CallExprAST {
   public:
	StructCallExprAST(std::string name, std::vector<std::unique_ptr<BaseASTNode>> args)
		: CallExprAST(std::move(name), std::move(args), true) {}

	Value codegen(llvm::IRBuilder<> &builder) override {
		bool skip = true;
		std::vector<llvm::Value *> oArgs;
		std::vector<std::string> oArgsTypes;

		for (const auto &arg : args) {
			if (skip) {
				skip = false;
				auto r = arg->codegenPtr(builder);
				oArgs.push_back(r.getValue());
				oArgsTypes.push_back(getTypeRegistry()->getPointerType(r.getType())->getName());
				continue;
			}
			auto r = arg->codegen(builder);
			oArgs.push_back(r.getValue());
			oArgsTypes.push_back(r.getType()->getName());
		}

		std::string mangledName = name;
		if (shouldMangle) {
			mangledName = mangle(name, oArgsTypes);
		}

		auto func = getModule()->getFunction(mangledName);
		if (!func) {
			compilationError("Function " + mangledName + " not found");
			return {};
		}

		return {builder.CreateCall(func, oArgs), getFunction(func)->getReturnType()};
	}
};

#endif	// EMMC_CALLEXPRAST_H
