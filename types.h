//
// Created by fail on 7/27/22.
//

#ifndef EMMC_TYPES_H
#define EMMC_TYPES_H

#include <string>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/IRBuilder.h>

#include "token.h"
#include "logging.h"

// TODO: This will need to be completely redone once I start implementing custom types
bool isBuiltinType(const std::string& type) {
    return true;
}

bool isBuiltinIntegerType(const std::string& type) {
    return true;
}

llvm::Value* buildBuiltinIntegerType(llvm::IRBuilder<>& builder, const std::string& type, int64_t value) {
    if(type == "I64") {
        return llvm::ConstantInt::get(builder.getContext(), llvm::APInt(64, value));
    } else if(type == "I32") {
        return llvm::ConstantInt::get(builder.getContext(), llvm::APInt(32, value));
    } else if(type == "I16") {
        return llvm::ConstantInt::get(builder.getContext(), llvm::APInt(16, value));
    } else if(type == "I8") {
        return llvm::ConstantInt::get(builder.getContext(), llvm::APInt(8, value));
    } else {
        return nullptr;
    }
}

llvm::Value* extendTo64Bits(llvm::IRBuilder<>& builder, const std::string& type, llvm::Value* lhs) {
    if(type == "i64") {
        return lhs;
    } else if(type == "i32") {
        std::cerr << "Warning: extending 32-bit integer to 64-bit" << std::endl;
        return builder.CreateZExt(lhs, builder.getInt64Ty());
    } else if(type == "i16") {
        std::cerr << "Warning: extending 16-bit integer to 64-bit" << std::endl;
        return builder.CreateZExt(lhs, builder.getInt64Ty());
    } else if(type == "i8") {
        std::cerr << "Warning: extending 8-bit integer to 64-bit" << std::endl;
        return builder.CreateZExt(lhs, builder.getInt64Ty());
    } else {
        return nullptr;
    }
}

llvm::Value* buildBuiltinIntegerBinOp(llvm::IRBuilder<>& builder, const std::string& lhsType, const std::string& rhsType, llvm::Value* lhs, llvm::Value* rhs, TokenType op) {
    // Extend integers to 64 bits if they are smaller than 64 bits
    lhs = extendTo64Bits(builder, lhsType, lhs);
    rhs = extendTo64Bits(builder, rhsType, rhs);

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

#endif //EMMC_TYPES_H
