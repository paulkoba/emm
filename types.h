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

// TODO: This will need to be completely redone once I start implementing custom types
bool isBuiltinType(const std::string& type) { return true; }
bool isBuiltinType(llvm::Type* value) { return true; }
bool isBuiltinIntegerType(const std::string& type) { return true; }
bool isBuiltinIntegerType(llvm::Type* value) { return true; }

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

llvm::Value* buildBuiltinIntegerBinOp(llvm::IRBuilder<>& builder, const std::string& lhsType,
									  const std::string& rhsType, llvm::Value* lhs, llvm::Value* rhs, TokenType op) {
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
		default:
			compilationError("Not yet implemented.");
			return nullptr;
	}
}

llvm::Type* getTypeFromString(const std::string& type, llvm::IRBuilder<>& builder) {
    if(isBuiltinType(type)) {
        if(isBuiltinIntegerType(type)) {
            if(type == "i64") {
                return builder.getInt64Ty();
            } else if(type == "i32") {
                return builder.getInt32Ty();
            } else if(type == "i16") {
                return builder.getInt16Ty();
            } else if(type == "i8") {
                return builder.getInt8Ty();
            } else {
                return nullptr;
            }
        } else {
            return nullptr;
        }
    } else {
        compilationError("Not yet implemented.");
        return nullptr;
    }
}

#endif	// EMMC_TYPES_H
