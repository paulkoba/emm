//
// Created by fail on 7/27/22.
//

#ifndef EMMC_TYPES_H
#define EMMC_TYPES_H

#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include <string>

#include "AST/CallExprAST.h"
#include "Basic/Logging.h"
#include "Lex/Token.h"
#include "Mangling.h"
#include "TypeRegistry.h"
#include "Value.h"

static Value buildBuiltinIntegerBinaryOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs, TokenType::TokenType op) {
	llvm::Value* result = nullptr;

	switch (op) {
		case TokenType::PLUS:
			result = builder.CreateAdd(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::MINUS:
			result = builder.CreateSub(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::PRODUCT:
			result = builder.CreateMul(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::DIVISION:
			if (rhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateSDiv(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateUDiv(lhs.getValue(), rhs.getValue());
			}

			return {result, lhs.getType()};
		case TokenType::EQUALS:
			return {builder.CreateICmpEQ(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::NOT_EQUALS:
			return {builder.CreateICmpNE(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::LESS:
			if (lhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateICmpSLT(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateICmpULT(lhs.getValue(), rhs.getValue());
			}

			return {result, getTypeRegistry()->getType("bool")};
		case TokenType::LESS_OR_EQUAL:
			if (lhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateICmpSLE(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateICmpULE(lhs.getValue(), rhs.getValue());
			}

			return {result, getTypeRegistry()->getType("bool")};
		case TokenType::GREATER:
			if (lhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateICmpSGT(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateICmpUGT(lhs.getValue(), rhs.getValue());
			}

			return {result, getTypeRegistry()->getType("bool")};
		case TokenType::GREATER_OR_EQUAL:
			if (lhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateICmpSGE(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateICmpUGE(lhs.getValue(), rhs.getValue());
			}

			return {result, getTypeRegistry()->getType("bool")};
		case TokenType::MODULO:
			if (lhs.getType()->usesSignedBuiltinOperators()) {
				result = builder.CreateSRem(lhs.getValue(), rhs.getValue());
			} else {
				result = builder.CreateURem(lhs.getValue(), rhs.getValue());
			}
			return {result, lhs.getType()};
		case TokenType::ASSIGN:
			compilationWarning(
				"Tried to generate assignment using buildBuiltinIntegerBinaryOp, which is deprecated. This is a "
				"compiler bug");
			result = builder.CreateStore(rhs.getValue(),
										 llvm::dyn_cast<llvm::LoadInst>(lhs.getValue())->getPointerOperand());
			return {result, lhs.getType()};
		default:
			compilationError("buildBuiltinIntegerBinaryOp: Not yet implemented.");
			return {nullptr, nullptr};
	}
}

static Value buildBuiltinFloatingPointBinaryOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs,
											   TokenType::TokenType op) {
	llvm::Value* result = nullptr;

	switch (op) {
		case TokenType::PLUS:
			result = builder.CreateFAdd(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::MINUS:
			result = builder.CreateFSub(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::PRODUCT:
			result = builder.CreateFMul(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::DIVISION:
			result = builder.CreateFDiv(lhs.getValue(), rhs.getValue());
			return {result, lhs.getType()};
		case TokenType::EQUALS:
			return {builder.CreateFCmpOEQ(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::NOT_EQUALS:
			return {builder.CreateFCmpONE(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::LESS:
			return {builder.CreateFCmpOLT(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::LESS_OR_EQUAL:
			return {builder.CreateFCmpOLE(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::GREATER:
			return {builder.CreateFCmpOGT(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::GREATER_OR_EQUAL:
			return {builder.CreateFCmpOGE(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
		case TokenType::MODULO:
			compilationError("buildBuiltinFloatingPointBinaryOp: Modulo operator not implemented.");
			return {nullptr, nullptr};
		case TokenType::ASSIGN:
			compilationWarning(
				"Tried to generate assignment using buildBuiltinFloatingPointBinaryOp, which is deprecated. This is a "
				"compiler bug");
			return {builder.CreateStore(rhs.getValue(),
										llvm::dyn_cast<llvm::LoadInst>(lhs.getValue())->getPointerOperand()),
					lhs.getType()};
		default:
			compilationError("buildBuiltinFloatingPointBinaryOp: Not yet implemented.");
			return {nullptr, nullptr};
	}
}

// TODO: This will need to be redone to use AST Nodes themselves instead of llvm::Value*
Value buildBuiltinBinaryOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs, TokenType::TokenType op) {
	if (lhs.getType()->getName() != rhs.getType()->getName()) {
		compilationError("Cannot perform binary operation on types " + lhs.getType()->getName() + " and " +
						 rhs.getType()->getName());
		return {};
	}

	if (!lhs.getType()->getBase()->isFloatingPointTy()) {
		return buildBuiltinIntegerBinaryOp(builder, lhs, rhs, op);
	} else {
		return buildBuiltinFloatingPointBinaryOp(builder, lhs, rhs, op);
	}
}

Value buildBuiltinIntegerUnaryOp(llvm::IRBuilder<>& builder, Value lhs, TokenType::TokenType op) {
	llvm::Value* result = nullptr;
	switch (op) {
		case TokenType::PLUS:
			return {lhs.getValue(), lhs.getType()};
		case TokenType::MINUS:
			result = builder.CreateNeg(lhs.getValue());
			return {result, lhs.getType()};
		case TokenType::NOT:
			result = builder.CreateNot(lhs.getValue());
			return {result, lhs.getType()};
		case TokenType::BITWISE_AND:  // Reference
			result = llvm::dyn_cast<llvm::LoadInst>(lhs.getValue())->getPointerOperand();
			return {result, getTypeRegistry()->getPointerType(lhs.getType())};
		case TokenType::PRODUCT: {	// Dereference operator
			auto pointed = getTypeRegistry()->getPointedType(lhs.getType());
			if (!pointed) {
				compilationError("Cannot dereference a non-pointer type");
				return {};
			}

			result = builder.CreateLoad(pointed->getBase(), lhs.getValue());
			return {result, getTypeRegistry()->getPointedType(lhs.getType())};
		}
		default:
			compilationError("buildBuiltinIntegerUnaryOp: Not yet implemented.");
			return {nullptr, nullptr};
	}
}

Value buildBuiltinFloatingPointUnaryOp(llvm::IRBuilder<>& builder, Value lhs, TokenType::TokenType op) {
	llvm::Value* result = nullptr;
	switch (op) {
		case TokenType::PLUS:
			return {lhs.getValue(), lhs.getType()};
		case TokenType::MINUS:
			result = builder.CreateFNeg(lhs.getValue());
			return {result, lhs.getType()};
		default:
			compilationError("buildBuiltinFloatingPointUnaryOp: Not yet implemented.");
			return {nullptr, nullptr};
	}
}

Value buildBuiltinUnaryOp(llvm::IRBuilder<>& builder, Value operand, TokenType::TokenType op) {
	if (!operand.getType()->getBase()->isFloatingPointTy()) {
		return buildBuiltinIntegerUnaryOp(builder, operand, op);
	} else {
		return buildBuiltinFloatingPointUnaryOp(builder, operand, op);
	}
}

Value buildBinaryOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs, TokenType::TokenType op,
					BaseASTNode* parent = nullptr) {
	if (lhs.getType()->usesBuiltinOperators() && rhs.getType()->usesBuiltinOperators()) {
		return buildBuiltinBinaryOp(builder, lhs, rhs, op);
	} else {
		auto binaryOpName = getBinaryOpName(lhs, rhs, op);
		auto vec = std::vector<std::unique_ptr<BaseASTNode>>();
		vec.push_back(std::make_unique<HelperASTNode>(lhs));
		vec.push_back(std::make_unique<HelperASTNode>(rhs));
		auto structCallExpr = std::make_unique<CallExprAST>(binaryOpName, std::move(vec));
		structCallExpr->parent = parent;
		return structCallExpr->codegen(builder);
	}
}

Value buildUnaryOp(llvm::IRBuilder<>& builder, Value lhs, TokenType::TokenType op) {
	if (lhs.getType()->usesBuiltinOperators()) {
		return buildBuiltinUnaryOp(builder, lhs, op);
	} else {
		compilationError("buildBinaryOp: Not yet implemented.");
		return {nullptr, nullptr};
	}
}

Value createCast(llvm::IRBuilder<>& builder, Value value, Type* type) {
	if (value.getType() == type) {
		return value;
	} else if (value.getType()->isPointer() && type->isPointer()) {
		return {builder.CreatePointerCast(value.getValue(), type->getBase()), type};
	} else if (value.getType()->usesBuiltinOperators()) {
		if (!value.getType()->getBase()->isFloatingPointTy() && !type->getBase()->isFloatingPointTy()) {
			if (value.getType()->usesSignedBuiltinOperators()) {
				return {builder.CreateSExtOrTrunc(value.getValue(), type->getBase()), type};
			} else {
				return {builder.CreateZExtOrTrunc(value.getValue(), type->getBase()), type};
			}
		} else if (value.getType()->getBase()->isFloatingPointTy() && type->getBase()->isFloatingPointTy()) {
			if (value.getType()->getBase()->getIntegerBitWidth() > type->getBase()->getIntegerBitWidth()) {
				return {builder.CreateFPTrunc(value.getValue(), type->getBase()), type};
			} else {
				return {builder.CreateFPExt(value.getValue(), type->getBase()), type};
			}
		} else if (value.getType()->getBase()->isFloatingPointTy() && !type->getBase()->isFloatingPointTy()) {
			if (type->usesSignedBuiltinOperators()) {
				return {builder.CreateFPToSI(value.getValue(), type->getBase()), type};
			} else {
				return {builder.CreateFPToUI(value.getValue(), type->getBase()), type};
			}
		} else {
			if (value.getType()->usesSignedBuiltinOperators()) {
				return {builder.CreateSIToFP(value.getValue(), type->getBase()), type};
			} else {
				return {builder.CreateUIToFP(value.getValue(), type->getBase()), type};
			}
		}
	} else {
		compilationError("createCast: Not yet implemented.");
		return {nullptr, nullptr};
	}
}

#endif	// EMMC_TYPES_H
