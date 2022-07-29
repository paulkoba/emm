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

void equalizeBitWidth(llvm::IRBuilder<>& builder, llvm::Value *&lhs, llvm::Value *&rhs, bool isSigned = true) {
    auto lhsType = lhs->getType();
    auto rhsType = rhs->getType();

    if (lhsType->isIntegerTy() && rhsType->isIntegerTy()) {
        auto lhsBitWidth = lhsType->getIntegerBitWidth();
        auto rhsBitWidth = rhsType->getIntegerBitWidth();

        if (lhsBitWidth > rhsBitWidth) {
            if(isSigned) {
                rhs = builder.CreateSExt(rhs, lhsType);
            } else {
                rhs = builder.CreateZExt(rhs, lhsType);
            }
        } else if (lhsBitWidth < rhsBitWidth) {
            if(isSigned) {
                lhs = builder.CreateSExt(lhs, rhsType);
            } else {
                lhs = builder.CreateZExt(lhs, rhsType);
            }
        }
    }
}

llvm::Value* buildBuiltinIntegerBinOp(llvm::IRBuilder<>& builder, llvm::Value* lhs, llvm::Value* rhs, TokenType op) {
	// Extend integers to the widest of their types
    equalizeBitWidth(builder, lhs, rhs);

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
        case TOK_ASSIGN:
            return builder.CreateStore(rhs, lhs);
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

llvm::Value* createCast(llvm::IRBuilder<>& builder, llvm::Value* value, llvm::Type* type) {
    if(value->getType() == type) {
        return value;
    } else if(value->getType()->isIntegerTy() && type->isIntegerTy()) {
        if(value->getType()->getIntegerBitWidth() < type->getIntegerBitWidth()) {
            return builder.CreateSExt(value, type);
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

std::string getTypeFromLiteral(const std::string& literal) {
    return literal;
}

#endif	// EMMC_TYPES_H
