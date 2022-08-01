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
#include "value.h"
#include "type_registry.h"

// TODO: This will need to be redone to use AST Nodes themselves instead of llvm::Value*
Value buildBuiltinIntegerBinOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs, TokenType op) {
    if(lhs.getValue()->getType()->getIntegerBitWidth() != rhs.getValue()->getType()->getIntegerBitWidth()) {
        compilationError("Cannot perform binary operation on integers of different widths "
            + std::to_string(lhs.getValue()->getType()->getIntegerBitWidth()) + " and "
            + std::to_string(rhs.getValue()->getType()->getIntegerBitWidth()));
    }

    llvm::Value* result = nullptr;

	switch (op) {
		case TOK_PLUS:
            result = builder.CreateAdd(lhs.getValue(), rhs.getValue());
            return {result, lhs.getType()};
		case TOK_MINUS:
            result = builder.CreateSub(lhs.getValue(), rhs.getValue());
            return {result, lhs.getType()};
		case TOK_PRODUCT:
            result = builder.CreateMul(lhs.getValue(), rhs.getValue());
            return {result, lhs.getType()};
		case TOK_DIVISION:
            if(rhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateSDiv(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateUDiv(lhs.getValue(), rhs.getValue());
            }

            return {result, lhs.getType()};
		case TOK_EQUALS:
			return {builder.CreateICmpEQ(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
        case TOK_NOT_EQUALS:
            return {builder.CreateICmpNE(lhs.getValue(), rhs.getValue()), getTypeRegistry()->getType("bool")};
        case TOK_LESS:
            if(lhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateICmpSLT(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateICmpULT(lhs.getValue(), rhs.getValue());
            }

            return {result, getTypeRegistry()->getType("bool")};
        case TOK_LESS_OR_EQUAL:
            if(lhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateICmpSLE(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateICmpULE(lhs.getValue(), rhs.getValue());
            }

            return {result, getTypeRegistry()->getType("bool")};
        case TOK_GREATER:
            if(lhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateICmpSGT(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateICmpUGT(lhs.getValue(), rhs.getValue());
            }

            return {result, getTypeRegistry()->getType("bool")};
        case TOK_GREATER_OR_EQUAL:
            if(lhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateICmpSGE(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateICmpUGE(lhs.getValue(), rhs.getValue());
            }

            return {result, getTypeRegistry()->getType("bool")};
        case TOK_MODULO:
            if(lhs.getType()->usesSignedBuiltinOperators()) {
                result = builder.CreateSRem(lhs.getValue(), rhs.getValue());
            } else {
                result = builder.CreateURem(lhs.getValue(), rhs.getValue());
            }

            return {result, lhs.getType()};
        case TOK_ASSIGN:
            // TODO: Verify that I return the right thing here. Also remove dyn_cast magic.
            result = builder.CreateStore(rhs.getValue(), llvm::dyn_cast<llvm::LoadInst>(lhs.getValue())->getPointerOperand());
            return {result, lhs.getType()};
		default:
			compilationError("buildBuiltinIntegerBinOp: Not yet implemented.");
			return {nullptr, nullptr};
	}
}

Value buildBinOp(llvm::IRBuilder<>& builder, Value lhs, Value rhs, TokenType op) {
    if(lhs.getType()->usesBuiltinOperators() && rhs.getType()->usesBuiltinOperators()) {
        return buildBuiltinIntegerBinOp(builder, lhs, rhs, op);
    } else {
        compilationError("buildBinOp: Not yet implemented.");
        return {nullptr, nullptr};
    }
}

Value createCast(llvm::IRBuilder<>& builder, Value value, Type* type) {
    if(value.getType() == type) {
        return value;
    } else if(value.getType()->usesBuiltinOperators()) {
        if(value.getType()->getBase()->getIntegerBitWidth() < type->getBase()->getIntegerBitWidth()) {
            if(type->usesSignedBuiltinOperators()) {
                return {builder.CreateSExt(value.getValue(), type->getBase()), type};
            } else {
                return {builder.CreateZExt(value.getValue(), type->getBase()), type};
            }
        } else {
            return {builder.CreateTrunc(value.getValue(), type->getBase()), type};
        }
    } else if(value.getType()->getBase()->isFloatingPointTy() && type->getBase()->isFloatingPointTy()) {
        return {builder.CreateFPExt(value.getValue(), type->getBase()), type};
    } else {
        compilationError("createCast: Not yet implemented.");
        return {nullptr, nullptr};
    }
}

#endif	// EMMC_TYPES_H
