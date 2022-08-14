//
// Created by fail on 8/14/22.
//

#ifndef EMMC_BASE_AST_NODE_H
#define EMMC_BASE_AST_NODE_H

#include <llvm/IR/Constants.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Value.h>

#include <cstdint>
#include <memory>
#include <string>
#include <utility>
#include <vector>

#include "function.h"
#include "token.h"
#include "type_registry.h"
#include "types.h"
#include "variable.h"

class BaseASTNode {
public:
    BaseASTNode *parent = nullptr;

public:
    virtual ~BaseASTNode() = default;

    [[nodiscard]] virtual std::string generateDOTHeader() const {
        return std::to_string((int64_t)this) + " [label=\"BaseASTNode\"]\n";
    }

    virtual std::string generateDOT() { return ""; }

    virtual Value codegen(llvm::IRBuilder<> &builder) {
        compilationError("Cannot perform codegen on this node");
        return {};
    }

    virtual Value codegenAssignment(llvm::IRBuilder<> &builder, Value value) {
        compilationError("Cannot perform assignment codegen on this node");
        return {};
    }

    virtual Value codegenPtr(llvm::IRBuilder<> &builder) {
        compilationError("Cannot perform pointer codegen on this node");
        return {};
    }

    virtual void populateParents() {
        // Do nothing
    }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual void createVariableInNearestScope(Variable variable) {
        if (parent) {
            parent->createVariableInNearestScope(std::move(variable));
        }
    }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual Variable *getVariableFromNearestScope(const std::string &name) {
        if (parent) {
            return parent->getVariableFromNearestScope(name);
        }

        return nullptr;
    }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual llvm::Module *getModule() {
        if (parent) {
            return parent->getModule();
        } else {
            return nullptr;
        }
    }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual llvm::Value *getReturnValue() {
        if (parent) {
            return parent->getReturnValue();
        } else {
            return nullptr;
        }
    }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual llvm::BasicBlock *getReturnBlock() {
        if (parent) {
            return parent->getReturnBlock();
        } else {
            return nullptr;
        }
    }

    virtual bool alwaysReturns() { return false; }

    // NOLINTNEXTLINE(misc-no-recursion)
    virtual void logVariables() {
        std::cerr << "--- Not in a scope \n";
        if(parent) {
            parent->logVariables();
        }
    }
};

class HelperASTNode : public BaseASTNode {
private:
    Value value;
public:
    explicit HelperASTNode(Value value) : value(value) {}

    ~HelperASTNode() override = default;

    Value codegen(llvm::IRBuilder<> &builder) override {
        return value;
    }

    Value codegenPtr(llvm::IRBuilder<> &builder) override {
        return value;
    }
};

#endif //EMMC_BASE_AST_NODE_H
