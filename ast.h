//
// Created by fail on 7/25/22.
//

#ifndef EMMC_AST_H
#define EMMC_AST_H

#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "token.h"
#include "types.h"

class Variable {
   public:
	std::string name;
	std::string type;
	llvm::Value *value = nullptr;

   public:
	Variable(std::string name, std::string type, llvm::Value *value)
		: name(std::move(name)), type(std::move(type)), value(value) {}

	Variable() = default;
};

class BaseASTNode {
   public:
	BaseASTNode *parent = nullptr;

   public:
	virtual ~BaseASTNode() = default;

	[[nodiscard]] virtual std::string generateDOTHeader() const {
		return std::to_string((int64_t)this) + " [label=\"BaseASTNode\"]\n";
	}

	virtual std::string generateDOT() { return ""; }

	virtual llvm::Value *codegen(llvm::IRBuilder<> &builder) { return nullptr; }

	virtual void populateParents() {
		// Do nothing
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	virtual void createVariableInNearestScope(const std::string &name, const std::string &type, llvm::Value *value) {
		if (parent) {
			parent->createVariableInNearestScope(name, type, value);
		}
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	virtual Variable *getVariableFromNearestScope(const std::string &name) {
		if (parent) {
			return parent->getVariableFromNearestScope(name);
		}

		return nullptr;
	}

	virtual bool isReturnStatement() { return false; }

	// NOLINTNEXTLINE(misc-no-recursion)
	virtual llvm::Module *getModule() {
		if (parent) {
			return parent->getModule();
		} else {
			return nullptr;
		}
	}
};

class I64AST : public BaseASTNode {
	int64_t value;

   public:
	explicit I64AST(int64_t value) : value(value) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"I64 " + std::to_string(value) + "\"]\n";
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		return llvm::ConstantInt::get(builder.getContext(), llvm::APInt(64, value, true));
	}
};

class VariableAST : public BaseASTNode {
	std::string name;
	std::string type;

   public:
	VariableAST(std::string name, std::string type) : name(std::move(name)), type(std::move(type)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"Variable " + name + ":" + type + "\"]\n";
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		auto var = getVariableFromNearestScope(name);

		if (var) {
			return var->value;
		}

		return nullptr;
	}
};

class StringAST : public BaseASTNode {
	std::string value;

   public:
	explicit StringAST(std::string value) : value(std::move(value)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"String\"]\n";
	}
};

class BinaryExprAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> lhs, rhs;
	TokenType op;

   public:
	BinaryExprAST(std::unique_ptr<BaseASTNode> lhs, std::unique_ptr<BaseASTNode> rhs, TokenType op)
		: lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		if (lhs) output += lhs->generateDOTHeader();
		if (rhs) output += rhs->generateDOTHeader();
		return output + std::to_string((int64_t)this) + " [label=\"BinaryExprAST " + tokenTypeToString(op) + "\"]\n";
	}

	std::string generateDOT() override {
		std::string output;
		if (lhs) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)lhs.get()) + "\n";
			output += lhs->generateDOT();
		}
		if (rhs) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)rhs.get()) + "\n";
			output += rhs->generateDOT();
		}
		return output;
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		auto lhsValue = lhs->codegen(builder);
		auto rhsValue = rhs->codegen(builder);
		if (!lhsValue || !rhsValue) return nullptr;

		// TODO: Properly handle different types
		auto r = buildBuiltinIntegerBinOp(builder, "i64", "i64", lhsValue, rhsValue, op);
		return r;
	}

	void populateParents() override {
		if (lhs) {
			lhs->parent = this;
			lhs->populateParents();
		}
		if (rhs) {
			rhs->parent = this;
			rhs->populateParents();
		}
	}
};

class UnaryOpAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> operand;
	std::string op;

   public:
	UnaryOpAST(std::unique_ptr<BaseASTNode> operand, std::string op) : operand(std::move(operand)), op(std::move(op)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		if (operand) output += operand->generateDOTHeader();
		return output + std::to_string((int64_t)this) + " [label=\"UnaryOpAST" + op + "\"]\n";
	}

	std::string generateDOT() override {
		std::string output;
		if (operand) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)operand.get()) + "\n";
			operand->generateDOT();
		}
		return output;
	}

	void populateParents() override {
		if (operand) {
			operand->parent = this;
			operand->populateParents();
		}
	}
};

class CallExprAST : public BaseASTNode {
	std::string name;
	std::vector<std::unique_ptr<BaseASTNode>> args;

   public:
	CallExprAST(std::string name, std::vector<std::unique_ptr<BaseASTNode>> args)
		: name(std::move(name)), args(std::move(args)) {}

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

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		auto func = getModule()->getFunction(name);
		if (!func) {
			std::cerr << "Error: Function " << name << " not found" << std::endl;
			return nullptr;
		}

		std::vector<llvm::Value *> oArgs;
		for (const auto &arg : args) {
			oArgs.push_back(arg->codegen(builder));
		}
		return builder.CreateCall(func, oArgs);
	}
};

class PrototypeAST : public BaseASTNode {
	std::string name;
	std::string returnType;
	std::vector<std::pair<std::string, std::string>> args;	// Name:Type Pairs
	friend class FunctionAST;

   public:
	PrototypeAST(std::string name, std::string returnType, std::vector<std::pair<std::string, std::string>> args)
		: name(std::move(name)), returnType(std::move(returnType)), args(std::move(args)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"PrototypeAST " + name + "; Args: ";

		for (const auto &arg : args) {
			output += " " + arg.first + ":" + arg.second;
		}

		output += "\"]\n";

		return output;
	}

	llvm::Function *codegen(llvm::IRBuilder<> &builder) override {
		std::vector<llvm::Type *> argTypes;
		// TODO: Proper types instead of always using 64 bit integers
		for (const auto &arg : args) {
			argTypes.push_back(llvm::Type::getInt64Ty(builder.getContext()));
		}

		llvm::FunctionType *functionType =
			llvm::FunctionType::get(llvm::Type::getInt64Ty(builder.getContext()), argTypes, false);
		llvm::Function *function =
			llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, getModule());

		std::size_t i = 0;
		for (auto &arg : function->args()) {
			arg.setName(args[i].first);
			i++;
		}

		return function;
	}
};

class ScopeAST : public BaseASTNode {
	std::vector<std::unique_ptr<BaseASTNode>> statements;
	std::map<std::string, Variable> variables;

	friend class FunctionAST;

	friend class IfAST;

   public:
	explicit ScopeAST(std::vector<std::unique_ptr<BaseASTNode>> statements) : statements(std::move(statements)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"ScopeAST\"]\n";
		for (const auto &statement : statements) {
			output += statement->generateDOTHeader();
		}
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		for (const auto &statement : statements) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)statement.get()) + "\n";
			output += statement->generateDOT();
		}
		return output;
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		llvm::Value *lastValue = nullptr;
		for (const auto &statement : statements) {
			lastValue = statement->codegen(builder);
		}
		return lastValue;
	}

	void populateParents() override {
		for (const auto &statement : statements) {
			statement->parent = this;
			statement->populateParents();
		}
	}

	void createVariableInNearestScope(const std::string &name, const std::string &type, llvm::Value *value) override {
		variables[name] = Variable(name, type, value);
	}

	Variable *getVariableFromNearestScope(const std::string &name) override {
		if (variables.find(name) != variables.end()) return &variables[name];

		if (parent) {
			return parent->getVariableFromNearestScope(name);
		}

		return nullptr;
	}
};

class FunctionAST : public BaseASTNode {
	std::unique_ptr<PrototypeAST> proto;
	std::unique_ptr<ScopeAST> body;

   public:
	FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ScopeAST> body)
		: proto(std::move(proto)), body(std::move(body)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"FunctionAST\"]\n";
		if (proto) output += proto->generateDOTHeader();
		if (body) output += body->generateDOTHeader();

		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (proto) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)proto.get()) + "\n";
			output += proto->generateDOT();
		}
		if (body) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)body.get()) + "\n";
			output += body->generateDOT();
		}
		return output;
	}

	llvm::Function *codegen(llvm::IRBuilder<> &builder) override {
		llvm::Function *function = getModule()->getFunction(proto->name);
		if (!function) {
			function = proto->codegen(builder);
		}
		if (!function) {
			return nullptr;
		}
		if (!function->empty()) {
			compilationError("Function already defined");
			return nullptr;
		}

		llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(builder.getContext(), "entry", function);
		builder.SetInsertPoint(entryBlock);

		for (auto &arg : function->args()) {
			body->createVariableInNearestScope((std::string)arg.getName(), "i64", &arg);
		}

		if (body) {
			body->codegen(builder);
		}

		return nullptr;
	}

	void populateParents() override {
		if (proto) {
			proto->parent = this;
			proto->populateParents();
		}
		if (body) {
			body->parent = this;
			body->populateParents();
		}
	}
};

class IfAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> condition;
	std::unique_ptr<ScopeAST> trueBranch;
	std::unique_ptr<ScopeAST> falseBranch;
   public:
	IfAST(std::unique_ptr<BaseASTNode> condition, std::unique_ptr<ScopeAST> trueBranch,
		  std::unique_ptr<ScopeAST> falseBranch)
		: condition(std::move(condition)), trueBranch(std::move(trueBranch)), falseBranch(std::move(falseBranch)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"IfAST\"]\n";
		if (condition) output += condition->generateDOTHeader();
		if (trueBranch) output += trueBranch->generateDOTHeader();
		if (falseBranch) output += falseBranch->generateDOTHeader();

		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (condition) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)condition.get()) + "\n";
			output += condition->generateDOT();
		}
		if (trueBranch) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)trueBranch.get()) + "\n";
			output += trueBranch->generateDOT();
		}
		if (falseBranch) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)falseBranch.get()) + "\n";
			output += falseBranch->generateDOT();
		}
		return output;
	}

	void populateParents() override {
		if (condition) {
			condition->parent = this;
			condition->populateParents();
		}
		if (trueBranch) {
			trueBranch->parent = this;
			trueBranch->populateParents();
		}
		if (falseBranch) {
			falseBranch->parent = this;
			falseBranch->populateParents();
		}
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		llvm::Value *conditionValue = condition->codegen(builder);
		if (!conditionValue) {
			return nullptr;
		}

		bool trueBranchReturn = !trueBranch->statements.empty() && trueBranch->statements.back()->isReturnStatement();
		bool falseBranchReturn = falseBranch && !falseBranch->statements.empty() && falseBranch->statements.back()->isReturnStatement();

		llvm::Value *conditionBool =
			builder.CreateICmpNE(conditionValue, llvm::ConstantInt::get(conditionValue->getType(), 0));
		llvm::Function *function = builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(builder.getContext(), "true", function);
		llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(builder.getContext(), "false");
		llvm::BasicBlock *exitBlock = nullptr;
		if (!falseBranchReturn || !trueBranchReturn) exitBlock = llvm::BasicBlock::Create(builder.getContext(), "exit");
		builder.CreateCondBr(conditionBool, trueBlock, falseBlock);
		builder.SetInsertPoint(trueBlock);
		if (trueBranch) {
			trueBranch->codegen(builder);
		}
		if (!trueBranchReturn) builder.CreateBr(exitBlock);
		function->getBasicBlockList().push_back(falseBlock);
		builder.SetInsertPoint(falseBlock);
		if (falseBranch) {
			falseBranch->codegen(builder);
		}
		if (!falseBranch || !falseBranchReturn) builder.CreateBr(exitBlock);

		if (exitBlock) {
			function->getBasicBlockList().push_back(exitBlock);
			builder.SetInsertPoint(exitBlock);
		}

		return nullptr;
	}
};

class ReturnAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> value;

   public:
	explicit ReturnAST(std::unique_ptr<BaseASTNode> value) : value(std::move(value)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"ReturnAST\"]\n";
		if (value) output += value->generateDOTHeader();
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (value) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)value.get()) + "\n";
			output += value->generateDOT();
		}
		return output;
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		if (value) {
			llvm::Value *val = value->codegen(builder);
			builder.CreateRet(val);
			return val;
		}
		return nullptr;
	}

	void populateParents() override {
		if (value) {
			value->parent = this;
			value->populateParents();
		}
	}

	bool isReturnStatement() override { return true; }
};

class LetAST : public BaseASTNode {
	std::string name;
	std::string type;
	std::unique_ptr<BaseASTNode> value;

   public:
	LetAST(std::string name, std::string type, std::unique_ptr<BaseASTNode> value)
		: name(std::move(name)), type(std::move(type)), value(std::move(value)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"LetAST " + name + ":" + type + "\"]\n";
		if (value) output += value->generateDOTHeader();
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (value) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)value.get()) + "\n";
			output += value->generateDOT();
		}
		return output;
	}
};

class ModuleAST : public BaseASTNode {
	std::vector<std::unique_ptr<BaseASTNode>> expressions;
	std::unique_ptr<llvm::Module> module = nullptr;

   public:
	explicit ModuleAST(std::vector<std::unique_ptr<BaseASTNode>> expressions, std::unique_ptr<llvm::Module> module)
		: expressions(std::move(expressions)), module(std::move(module)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"ModuleAST\"]\n";
		for (const auto &expression : expressions) {
			output += expression->generateDOTHeader();
		}
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		for (const auto &expression : expressions) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)expression.get()) + "\n";
			output += expression->generateDOT();
		}
		return output;
	}

	llvm::Value *codegen(llvm::IRBuilder<> &builder) override {
		for (const auto &expression : expressions) {
			expression->codegen(builder);
		}
		return nullptr;
	}

	void populateParents() override {
		for (const auto &expression : expressions) {
			expression->parent = this;
			expression->populateParents();
		}
	}

	llvm::Module *getModule() override { return module.get(); }
};

#endif	// EMMC_AST_H
