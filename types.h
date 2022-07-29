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

#include "logging.h"
#include "token.h"

bool isBuiltinIntegerType(llvm::Type* value) { return value->isIntegerTy(); }

llvm::Value* buildBuiltinIntegerBinOp(llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs, TokenType op) {
    if(lhs->getType()->getIntegerBitWidth() != rhs->getType()->getIntegerBitWidth()) {
        compilationError("Cannot perform binary operation on integers of different widths "
            + std::to_string(lhs->getType()->getIntegerBitWidth()) + " and "
            + std::to_string(rhs->getType()->getIntegerBitWidth()));
    }

	switch (op) {
		case TOK_PLUS:
			return builder.CreateAdd(lhs, rhs);
		case TOK_MINUS:
			return builder.CreateSub(lhs, rhs);
		case TOK_PRODUCT:
			return builder.CreateMul(lhs, rhs);
		case TOK_DIVISION:
			return builder.CreateSDiv(lhs, rhs);
		case TOK_EQUALS:
			return builder.CreateICmpEQ(lhs, rhs);
        case TOK_NOT_EQUALS:
            return builder.CreateICmpNE(lhs, rhs);
        case TOK_LESS:
            return builder.CreateICmpSLT(lhs, rhs);
        case TOK_LESS_OR_EQUAL:
            return builder.CreateICmpSLE(lhs, rhs);
        case TOK_GREATER:
            return builder.CreateICmpSGT(lhs, rhs);
        case TOK_GREATER_OR_EQUAL:
            return builder.CreateICmpSGE(lhs, rhs);
        case TOK_MODULO:
            return builder.CreateSRem(lhs, rhs);
		default:
			compilationError("buildBuiltinIntegerBinOp: Not yet implemented.");
			return nullptr;
	}
}

llvm::Value* buildBinOp(llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs, TokenType op) {
    if(isBuiltinIntegerType(lhs->getType()) && isBuiltinIntegerType(rhs->getType())) {
        return buildBuiltinIntegerBinOp(builder, lhs, rhs, op);
    } else {
        compilationError("buildBinOp: Not yet implemented.");
        return nullptr;
    }
}

llvm::Type* getTypeFromString(const std::string& type, llvm::IRBuilder<>& builder) {
    if (type == "i64") {
        return builder.getInt64Ty();
    } else if (type == "i32") {
        return builder.getInt32Ty();
    } else if (type == "i16") {
        return builder.getInt16Ty();
    } else if (type == "i8") {
        return builder.getInt8Ty();
    } else {
        compilationError("getTypeFromString: Not yet implemented." + type);
        return nullptr;
    }
}

llvm::Value* createCast(llvm::IRBuilder<>& builder, llvm::Value* value, llvm::Type* type, bool isSigned = true) {
    if(value->getType() == type) {
        return value;
    } else if(value->getType()->isIntegerTy() && type->isIntegerTy()) {
        if(value->getType()->getIntegerBitWidth() < type->getIntegerBitWidth()) {
            if(isSigned) {
                return builder.CreateSExt(value, type);
            } else {
                return builder.CreateZExt(value, type);
            }
        } else {
            return builder.CreateTrunc(value, type);
        }
    } else if(value->getType()->isFloatingPointTy() && type->isFloatingPointTy()) {
        return builder.CreateFPExt(value, type);
    } else {
        compilationError("createCast: Not yet implemented.");
        return nullptr;
    }
}

#endif	// EMMC_TYPES_H
