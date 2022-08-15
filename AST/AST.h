//
// Created by fail on 7/25/22.
//

#ifndef EMMC_AST_H
#define EMMC_AST_H

#include "../Types.h"

class SignedIntAST : public BaseASTNode {
	int64_t value;
	int64_t bitWidth;

   public:
	explicit SignedIntAST(int64_t value, int64_t bitWidth) : value(value), bitWidth(bitWidth) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"" + std::to_string(value) + "i" + std::to_string(bitWidth) +
			   "\"]\n";
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		return {llvm::ConstantInt::get(builder.getContext(), llvm::APInt(bitWidth, value, true)),
				getTypeRegistry()->getType("i" + std::to_string(bitWidth))};
	}
};

class I64AST : public SignedIntAST {
   public:
	explicit I64AST(int64_t value) : SignedIntAST(value, 64) {}
};

class UnsignedIntAST : public BaseASTNode {
	uint64_t value;
	uint64_t bitWidth;

   public:
	explicit UnsignedIntAST(uint64_t value, uint64_t bitWidth) : value(value), bitWidth(bitWidth) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"" + std::to_string(value) + "u" + std::to_string(bitWidth) +
			   "\"]\n";
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		return {llvm::ConstantInt::get(builder.getContext(), llvm::APInt(bitWidth, value, false)),
				getTypeRegistry()->getType("u" + std::to_string(bitWidth))};
	}
};

class FloatingPointAST : public BaseASTNode {
	double value;
	int64_t bitWidth;

   public:
	explicit FloatingPointAST(double value, int64_t bitWidth) : value(value), bitWidth(bitWidth) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"" + std::to_string(value) + "f" + std::to_string(bitWidth) +
			   "\"]\n";
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		return {llvm::ConstantFP::get(builder.getContext(), llvm::APFloat(value)),
				getTypeRegistry()->getType("f" + std::to_string(bitWidth))};
	}
};

class F64AST : public FloatingPointAST {
   public:
	explicit F64AST(double value) : FloatingPointAST(value, 64) {}
};

class VariableAST : public BaseASTNode {
	std::string name;
	std::string type;

   public:
	VariableAST(std::string name, std::string type) : name(std::move(name)), type(std::move(type)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		return std::to_string((int64_t)this) + " [label=\"Variable " + name + ":" + type + "\"]\n";
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		auto var = getVariableFromNearestScope(name);

		if (var) {
			auto llvmType = var->value.getType();
			if (!llvmType) {
				compilationError("Variable " + name + " has no type");
				return {};
			}

			return {builder.CreateLoad(llvmType->getBase(), var->value.getValue()), var->value.getType()};
		}

		compilationError("Variable " + name + " not found");
		return {};
	}

	Value codegenAssignment(llvm::IRBuilder<> &builder, Value rhs) override {
		auto lhs = codegen(builder);
		if (!lhs) return {};
		// FIXME: I have no idea how it works
		auto result =
			builder.CreateStore(rhs.getValue(), llvm::dyn_cast<llvm::LoadInst>(lhs.getValue())->getPointerOperand());
		return {result, lhs.getType()};
	}

	Value codegenPtr(llvm::IRBuilder<> &builder) override {
		auto var = getVariableFromNearestScope(name);
		if (!var) {
			compilationError("Variable " + name + " not found");
			return {};
		}
		return {var->value.getValue(), var->value.getType()};
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
    TokenType::TokenType op;

   public:
	BinaryExprAST(std::unique_ptr<BaseASTNode> lhs, std::unique_ptr<BaseASTNode> rhs, TokenType::TokenType op)
		: lhs(std::move(lhs)), rhs(std::move(rhs)), op(op) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		if (lhs) output += lhs->generateDOTHeader();
		if (rhs) output += rhs->generateDOTHeader();
		return output + std::to_string((int64_t)this) + " [label=\"BinaryExprAST " + TokenType::toString(op) +
			   "\"]\n";
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		if (op != TokenType::TokenType::ASSIGN) {
			auto lhsValue = lhs->codegen(builder);
			auto rhsValue = rhs->codegen(builder);

			if (!lhsValue || !rhsValue) return {};

			// TODO: Check if operator actually exists. Now it just segfaults if it doesn't
			if (!lhsValue.getType()->usesBuiltinOperators() || !rhsValue.getType()->usesBuiltinOperators()) {
				// Get ptr to lhs
				auto lhsPtr = lhsValue.getValue();
				auto alloc = builder.CreateAlloca(lhsValue.getType()->getBase());
				builder.CreateStore(lhsPtr, alloc);

				auto r = buildBinaryOp(builder, {alloc, getTypeRegistry()->getPointerType(lhsValue.getType())},
									   rhsValue, op, this);

				return r;
			} else {
				auto r = buildBinaryOp(builder, lhsValue, rhsValue, op);

				return r;
			}
		} else {
			auto rhsValue = rhs->codegen(builder);

			if (!rhsValue) return {};

			return lhs->codegenAssignment(builder, rhsValue);
		}
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
    TokenType::TokenType op;

   public:
	UnaryOpAST(std::unique_ptr<BaseASTNode> operand, TokenType::TokenType op) : operand(std::move(operand)), op(op) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		if (operand) output += operand->generateDOTHeader();
		return output + std::to_string((int64_t)this) + " [label=\"UnaryOpAST " + TokenType::toString(op) + "\"]\n";
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		auto operandValue = operand->codegen(builder);
		if (!operandValue) return {};

		auto r = buildUnaryOp(builder, operandValue, op);
		return r;
	}
};

class AsAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> lhs;
	std::string type;  // We can not use Type* here because during AST generation real types will not be known yet.

   public:
	AsAST(std::unique_ptr<BaseASTNode> lhs, std::string type) : lhs(std::move(lhs)), type(std::move(type)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output;
		if (lhs) output += lhs->generateDOTHeader();
		return output + std::to_string((int64_t)this) + " [label=\"AsAST " + type + "\"]\n";
	}

	std::string generateDOT() override {
		std::string output;
		if (lhs) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)lhs.get()) + "\n";
			output += lhs->generateDOT();
		}
		return output;
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		auto lhsValue = lhs->codegen(builder);
		if (!lhsValue) return {};

		return createCast(builder, lhsValue, typeRegistry->getType(type));
	}

	void populateParents() override {
		if (lhs) {
			lhs->parent = this;
			lhs->populateParents();
		}
	}
};

class PrototypeAST : public BaseASTNode {
	std::string name;
	std::string returnType;
	std::vector<std::pair<std::string, std::string>> args;	// Name:Type Pairs
	bool shouldMangle;
	friend class FunctionAST;

   public:
	PrototypeAST(std::string name, std::string returnType, std::vector<std::pair<std::string, std::string>> args,
				 bool shouldMangle = false)
		: name(std::move(name)), returnType(std::move(returnType)), args(std::move(args)), shouldMangle(shouldMangle) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"PrototypeAST " + name + "; Args: ";

		for (const auto &arg : args) {
			output += " " + arg.first + ":" + arg.second;
		}

		output += "\"]\n";

		return output;
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		std::vector<llvm::Type *> argTypes;
		std::vector<std::string> argTypesNames;

		auto registry = getTypeRegistry();
		auto rtype = registry->getType(returnType);

		for (const auto &arg : args) {
			argTypes.push_back(registry->getType(arg.second)->getBase());
			argTypesNames.push_back(arg.second);
		}

		if (shouldMangle) {
			name = mangle(name, argTypesNames);
		}

		llvm::FunctionType *functionType = llvm::FunctionType::get(rtype->getBase(), argTypes, false);
		llvm::Function *function =
			llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, name, getModule());

		std::size_t i = 0;
		for (auto &arg : function->args()) {
			arg.setName(args[i].first);
			i++;
		}

		functionMap[function] = std::make_unique<Function>(function, rtype);

		return {function, nullptr};
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		Value lastValue;
		for (const auto &statement : statements) {
			lastValue = statement->codegen(builder);
			if (statement->alwaysReturns()) break;
		}
		return lastValue;
	}

	void populateParents() override {
		for (const auto &statement : statements) {
			statement->parent = this;
			statement->populateParents();
		}
	}

	void createVariableInNearestScope(Variable variable) override { variables[variable.name] = variable; }

	Variable *getVariableFromNearestScope(const std::string &name) override {
		if (variables.find(name) != variables.end()) return &variables[name];

		if (parent) {
			return parent->getVariableFromNearestScope(name);
		}

		return nullptr;
	}

	bool alwaysReturns() override {
		for (const auto &statement : statements) {
			if (statement->alwaysReturns()) return true;
		}
		return false;
	}

	void logVariables() override {
		std::cerr << "--- Scope \n";
		for (const auto &variable : variables) {
			std::cerr << variable.first << " " << variable.second.value.getType()->getName() << std::endl;
		}

		if (parent) {
			parent->logVariables();
		}
	}
};

class FunctionAST : public BaseASTNode {
	std::unique_ptr<PrototypeAST> proto;
	std::unique_ptr<ScopeAST> body;

	std::string structName;
	Value returnValue;
	llvm::BasicBlock *returnBlock = nullptr;

   public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ScopeAST> body)
            : proto(std::move(proto)), body(std::move(body)), structName("") {}

	FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ScopeAST> body, std::string structName)
		: proto(std::move(proto)), body(std::move(body)), structName(std::move(structName)) {}

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

	Value codegen(llvm::IRBuilder<> &builder) override {
		llvm::Function *function = getModule()->getFunction(proto->name);
		if (!function) {
			function = static_cast<llvm::Function *>(proto->codegen(builder).getValue());
		}
		if (!function) {
			return {};
		}
		if (!function->empty()) {
			compilationError("Function already defined");
			return {};
		}

		llvm::BasicBlock *entryBlock = llvm::BasicBlock::Create(builder.getContext(), "entry", function);

		returnBlock = llvm::BasicBlock::Create(builder.getContext(), "return", function);
		builder.SetInsertPoint(entryBlock);
		std::size_t idx = 0;

		bool skip = true;

		for (auto &arg : function->args()) {
			if (skip && !structName.empty()) {
				body->createVariableInNearestScope(
					Variable{"self", Value{&arg, getTypeRegistry()->getPointedType(
													 getTypeRegistry()->getType(proto->args[idx].second))}});
				skip = false;
				++idx;
				continue;
			}

			llvm::Value *stored = builder.CreateAlloca(arg.getType());
			builder.CreateStore(&arg, stored);
			body->createVariableInNearestScope(
				Variable{proto->args[idx].first, Value{stored, getTypeRegistry()->getType(proto->args[idx].second)}});

			++idx;
		}

		// Allocate space for return value
		if (proto->returnType != "void") {
			returnValue = {builder.CreateAlloca(getTypeRegistry()->getType(proto->returnType)->getBase()),
						   getTypeRegistry()->getType(proto->returnType)};
		}

		if (body) {
			body->codegen(builder);

			if (!body->alwaysReturns()) {
				builder.CreateBr(returnBlock);
			}
		}

		// Return value
		builder.SetInsertPoint(returnBlock);

		if (proto->returnType != "void") {
			builder.CreateRet(builder.CreateLoad(returnValue.getType()->getBase(), returnValue.getValue()));
		} else {
			builder.CreateRetVoid();
		}

		return {};
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

	// NOLINTNEXTLINE(misc-no-recursion)
	llvm::Value *getReturnValue() override { return returnValue.getValue(); }

	// NOLINTNEXTLINE(misc-no-recursion)
	llvm::BasicBlock *getReturnBlock() override { return returnBlock; }
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		Value conditionValue = condition->codegen(builder);
		if (!conditionValue) {
			return {};
		}

		bool trueBranchReturn = trueBranch && trueBranch->alwaysReturns();
		bool falseBranchReturn = falseBranch && falseBranch->alwaysReturns();
		llvm::Value *conditionBool = builder.CreateICmpNE(
			conditionValue.getValue(), llvm::ConstantInt::get(conditionValue.getType()->getBase(), 0));
		llvm::Function *function = builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *trueBlock = llvm::BasicBlock::Create(builder.getContext(), "true", function);
		llvm::BasicBlock *falseBlock = llvm::BasicBlock::Create(builder.getContext(), "false");
		llvm::BasicBlock *exitBlock = nullptr;
		if (!trueBranchReturn || !falseBranchReturn) exitBlock = llvm::BasicBlock::Create(builder.getContext(), "exit");
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
		if (!falseBranchReturn) builder.CreateBr(exitBlock);

		if (!trueBranchReturn || !falseBranchReturn) {
			function->getBasicBlockList().push_back(exitBlock);
			builder.SetInsertPoint(exitBlock);
		}
		return {};
	}

	bool alwaysReturns() override { return trueBranch->alwaysReturns() && falseBranch && falseBranch->alwaysReturns(); }
};

class WhileAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> condition;
	std::unique_ptr<ScopeAST> body;

   public:
	WhileAST(std::unique_ptr<BaseASTNode> condition, std::unique_ptr<ScopeAST> body)
		: condition(std::move(condition)), body(std::move(body)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"WhileAST\"]\n";
		if (condition) output += condition->generateDOTHeader();
		if (body) output += body->generateDOTHeader();
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (condition) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)condition.get()) + "\n";
			output += condition->generateDOT();
		}
		if (body) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)body.get()) + "\n";
			output += body->generateDOT();
		}
		return output;
	}

	void populateParents() override {
		if (condition) {
			condition->parent = this;
			condition->populateParents();
		}
		if (body) {
			body->parent = this;
			body->populateParents();
		}
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		llvm::Function *function = builder.GetInsertBlock()->getParent();
		llvm::BasicBlock *whileConditionBlock =
			llvm::BasicBlock::Create(builder.getContext(), "whileCondition", function);
		llvm::BasicBlock *whileBodyBlock = llvm::BasicBlock::Create(builder.getContext(), "whileBody", function);
		llvm::BasicBlock *exitBlock = llvm::BasicBlock::Create(builder.getContext(), "exit");
		builder.CreateBr(whileConditionBlock);
		builder.SetInsertPoint(whileConditionBlock);
		Value conditionValue = condition->codegen(builder);
		if (!conditionValue) {
			compilationError("Condition of while statement is not a valid expression");
			return {};
		}
		llvm::Value *conditionBool = builder.CreateICmpNE(
			conditionValue.getValue(), llvm::ConstantInt::get(conditionValue.getType()->getBase(), 0));

		builder.CreateCondBr(conditionBool, whileBodyBlock, exitBlock);
		builder.SetInsertPoint(whileBodyBlock);
		body->codegen(builder);
		builder.CreateBr(whileConditionBlock);

		function->getBasicBlockList().push_back(exitBlock);
		builder.SetInsertPoint(exitBlock);

		return {};
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		if (value) {
			Value val = value->codegen(builder);
			// Check that the return type matches the function return type
			// TODO: This should check that Value types are the same, not llvm:Value*
			if (val.getType()->getBase() != builder.GetInsertBlock()->getParent()->getReturnType()) {
				compilationError("Return type does not match function return type");
				return {};
			}

			auto returnValue = getReturnValue();
			builder.CreateStore(val.getValue(), returnValue);
			builder.CreateBr(getReturnBlock());

			return val;
		}
		return {};
	}

	void populateParents() override {
		if (value) {
			value->parent = this;
			value->populateParents();
		}
	}

	bool alwaysReturns() override { return true; }
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		Value result = value->codegen(builder);
		llvm::Type *llvmType = nullptr;
		if (type.empty()) {
			llvmType = result.getType()->getBase();
			type = result.getType()->getName();
		} else {
			llvmType = result.getType()->getBase();
		}

		if (!llvmType) {
			compilationError("Unknown type " + type);
			return {};
		}

		// Allocate space on stack for the variable
		llvm::Value *alloca = builder.CreateAlloca(llvmType);
		// Store the value in the alloca
		builder.CreateStore(result.getValue(), alloca);
		// Add the alloca to the symbol table
		createVariableInNearestScope(Variable{name, {alloca, getTypeRegistry()->getType(type)}});

		return {};
	}

	void populateParents() override {
		if (value) {
			value->parent = this;
			value->populateParents();
		}
	}
};

class StructAST : public BaseASTNode {
	std::string name;
	std::vector<std::pair<std::string, std::string>> members;
	std::unique_ptr<FunctionAST> constructor;

   public:
	explicit StructAST(std::string name, std::vector<std::pair<std::string, std::string>> members)
		: name(std::move(name)), members(std::move(members)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"StructAST " + name + "\n";
		for (auto &member : members) {
			output += member.first + ":" + member.second + "\n";
		}
		output += "\"]\n";
		return output;
	}

	[[nodiscard]] std::string generateDOT() override { return ""; }

	Value codegen(llvm::IRBuilder<> &builder) override {
		// Create array of member types
		std::vector<llvm::Type *> memberTypes;
		std::vector<Type *> internalTypes;
		for (auto &member : members) {
			// Get type from string
			auto type = getTypeRegistry()->getType(member.second);
			memberTypes.push_back(type->getBase());
			internalTypes.push_back(type);
		}

		// Create the struct type
		llvm::StructType *structType = llvm::StructType::create(builder.getContext(), memberTypes, name);

		// Add the struct type to the type registry
		auto internalStructType = new StructType(structType, name, members, internalTypes);
		getTypeRegistry()->registerType(name, internalStructType);

		auto scope = std::vector<std::unique_ptr<BaseASTNode>>();
		scope.push_back(
			static_cast<std::unique_ptr<BaseASTNode>>(std::make_unique<ReturnAST>(std::make_unique<HelperASTNode>(
				Value{internalStructType->getDefaultValue(builder), getTypeRegistry()->getType(name)}))));

		// Generate and register default constructor for the struct
		// TODO: This will need to be completely redone once i support operator overloading
		constructor = std::make_unique<FunctionAST>(
			std::make_unique<PrototypeAST>(name, name, std::vector<std::pair<std::string, std::string>>{}),
			std::make_unique<ScopeAST>(std::move(scope)));
		constructor->parent = this;
		constructor->populateParents();
		constructor->codegen(builder);

		return {};
	}
};

class MemberAST : public BaseASTNode {
	std::string name;
	std::unique_ptr<BaseASTNode> value;

   public:
	MemberAST(std::string name, std::unique_ptr<BaseASTNode> value) : name(std::move(name)), value(std::move(value)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"MemberAST " + name + "\"]\n";
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		Value result = value->codegen(builder);

		if (result.getType()->usesBuiltinOperators()) {
			compilationError("codegen: Cannot use member operator on type " + result.getType()->getName());
			return {};
		}

		auto offset = result.getType()->getMemberOffset(name);

		// Extract element
		auto extracted = builder.CreateExtractValue(result.getValue(), offset);

		return {extracted, result.getType()->getMemberType(name)};
	}

	// TODO: This is not an intended way to do this, and may generate suboptimal code
	Value codegenAssignment(llvm::IRBuilder<> &builder, Value val) override {
		Value result = value->codegen(builder);

		if (result.getType()->usesBuiltinOperators()) {
			compilationError("codegenAssignment: Cannot use member operator on type " + result.getType()->getName());
			return {};
		}

		auto offset = result.getType()->getMemberOffset(name);

		// Insert element
		auto inserted = builder.CreateInsertValue(result.getValue(), val.getValue(), offset);

		// Store the value in the alloca
		return value->codegenAssignment(builder, {inserted, result.getType()->getMemberType(name)});
	}

	Value codegenPtr(llvm::IRBuilder<> &builder) override {
		Value result = value->codegen(builder);

		if (result.getType()->usesBuiltinOperators()) {
			compilationError("codegenPtr: Cannot use member operator on type " + result.getType()->getName());
			return {};
		}

		auto offset = result.getType()->getMemberOffset(name);

		// Get pointer to the member
		auto ptr = builder.CreateStructGEP(result.getType()->getBase(), result.getValue(), offset);

		return {ptr, result.getType()->getMemberType(name)};
	}

	void populateParents() override {
		if (value) {
			value->parent = this;
			value->populateParents();
		}
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

	Value codegen(llvm::IRBuilder<> &builder) override {
		for (const auto &expression : expressions) {
			expression->codegen(builder);
		}
		return {};
	}

	void populateParents() override {
		for (const auto &expression : expressions) {
			expression->parent = this;
			expression->populateParents();
		}
	}

	llvm::Module *getModule() override { return module.get(); }
};

class IndexAST : public BaseASTNode {
	std::unique_ptr<BaseASTNode> array;
	std::unique_ptr<BaseASTNode> index;

   private:
	Value getElementPtr(llvm::IRBuilder<> &builder) {
		Value arrayVal = array->codegen(builder);
		Value IndexVal = index->codegen(builder);

		if (!arrayVal || !IndexVal) {
			return {};
		}

		if (!arrayVal.getType()->isPointer()) {
			compilationError("Cannot IndexVal non-pointer type " + arrayVal.getType()->getName());
			return {};
		}

		if (!IndexVal.getType()->usesBuiltinOperators()) {
			compilationError("Cannot IndexVal with non-integer types " + IndexVal.getType()->getName());
			return {};
		}

		auto elementType = getTypeRegistry()->getPointedType(arrayVal.getType());

		// Get the element
		auto extracted = builder.CreateGEP(elementType->getBase(), arrayVal.getValue(), IndexVal.getValue());
		return {extracted, elementType};
	}

   public:
	IndexAST(std::unique_ptr<BaseASTNode> array, std::unique_ptr<BaseASTNode> index)
		: array(std::move(array)), index(std::move(index)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"IndexAST\"]\n";
		if (array) output += array->generateDOTHeader();
		if (index) output += index->generateDOTHeader();
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		if (array) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)array.get()) + "\n";
			output += array->generateDOT();
		}
		if (index) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)index.get()) + "\n";
			output += index->generateDOT();
		}
		return output;
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		auto ptr = getElementPtr(builder);

		return {builder.CreateLoad(ptr.getType()->getBase(), ptr.getValue()), ptr.getType()};
	}

	Value codegenAssignment(llvm::IRBuilder<> &builder, Value val) override {
		auto ptr = getElementPtr(builder);
		builder.CreateStore(val.getValue(), ptr.getValue());
		return {};
	}

	void populateParents() override {
		if (array) {
			array->parent = this;
			array->populateParents();
		}
		if (index) {
			index->parent = this;
			index->populateParents();
		}
	}
};

class ImplAST : public BaseASTNode {
	std::vector<std::unique_ptr<BaseASTNode>> functions;
	std::string name;

   public:
	ImplAST(std::vector<std::unique_ptr<BaseASTNode>> functions, std::string name)
		: functions(std::move(functions)), name(std::move(name)) {}

	[[nodiscard]] std::string generateDOTHeader() const override {
		std::string output = std::to_string((int64_t)this) + " [label=\"ImplAST\"]\n";
		for (const auto &function : functions) {
			output += function->generateDOTHeader();
		}
		return output;
	}

	[[nodiscard]] std::string generateDOT() override {
		std::string output;
		for (const auto &function : functions) {
			output += std::to_string((int64_t)this) + " -> " + std::to_string((int64_t)function.get()) + "\n";
			output += function->generateDOT();
		}
		return output;
	}

	Value codegen(llvm::IRBuilder<> &builder) override {
		for (const auto &function : functions) {
			function->codegen(builder);
		}
		return {};
	}

	void populateParents() override {
		for (const auto &function : functions) {
			function->parent = this;
			function->populateParents();
		}
	}
};

std::unique_ptr<BaseASTNode> fromLiteral(const std::string &integer, const std::string &type) {
	// TODO: There is definitely a better way to do this
	if (type == "i8") {
		return std::make_unique<SignedIntAST>(std::stoi(integer), 8);
	} else if (type == "i16") {
		return std::make_unique<SignedIntAST>(std::stoi(integer), 16);
	} else if (type == "i32") {
		return std::make_unique<SignedIntAST>(std::stoi(integer), 32);
	} else if (type == "i64") {
		return std::make_unique<SignedIntAST>(std::stoi(integer), 64);
	} else if (type == "u8") {
		return std::make_unique<UnsignedIntAST>(std::stoi(integer), 8);
	} else if (type == "u16") {
		return std::make_unique<UnsignedIntAST>(std::stoi(integer), 16);
	} else if (type == "u32") {
		return std::make_unique<UnsignedIntAST>(std::stoi(integer), 32);
	} else if (type == "u64") {
		return std::make_unique<UnsignedIntAST>(std::stoi(integer), 64);
	} else if (type == "f32") {
		return std::make_unique<FloatingPointAST>(std::stod(integer), 32);
	} else if (type == "f64") {
		return std::make_unique<FloatingPointAST>(std::stod(integer), 64);
	} else {
		compilationError("Unknown literal " + type);
		return nullptr;
	}
}

#endif	// EMMC_AST_H
