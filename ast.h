//
// Created by fail on 7/25/22.
//

#ifndef EMMC_AST_H
#define EMMC_AST_H

#include <cstdint>
#include <string>
#include <utility>
#include <memory>
#include <vector>

#include "token.h"

class ExpressionAST {
public:
    virtual ~ExpressionAST() = default;

    [[nodiscard]] virtual std::string generateDOTHeader() const {
        return std::to_string((int64_t) this) + " [label=\"ExpressionAST\"]\n";
    }

    virtual std::string generateDOT() {
        return "";
    }
};

class I64AST : public ExpressionAST {
    int64_t value;
public:
    explicit I64AST(int64_t value) : value(value) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        return std::to_string((int64_t) this) + " [label=\"I64 " + std::to_string(value) + "\"]\n";
    }
};

class VariableAST : public ExpressionAST {
    std::string name;
    std::string type;
public:
    VariableAST(std::string name, std::string type) : name(std::move(name)), type(std::move(type)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        return std::to_string((int64_t) this) + " [label=\"Variable " + name + ":" + type + "\"]\n";
    }
};

class StringAST : public ExpressionAST {
    std::string value;
public:
    explicit StringAST(std::string value) : value(std::move(value)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        return std::to_string((int64_t) this) + " [label=\"String\"]\n";
    }
};

class BinaryExprAST : public ExpressionAST {
    std::unique_ptr<ExpressionAST> lhs, rhs;
    std::string op;
public:
    BinaryExprAST(std::unique_ptr<ExpressionAST> lhs, std::unique_ptr<ExpressionAST> rhs, std::string op) : lhs(
            std::move(lhs)), rhs(std::move(rhs)), op(std::move(op)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output;
        if (lhs) output += lhs->generateDOTHeader();
        if (rhs) output += rhs->generateDOTHeader();
        return output + std::to_string((int64_t) this) + " [label=\"BinaryExprAST" + op + "\"]\n";
    }

    std::string generateDOT() override {
        std::string output;
        if (lhs) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) lhs.get()) + "\n";
            output += lhs->generateDOT();
        }
        if (rhs) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) rhs.get()) + "\n";
            output += rhs->generateDOT();
        }
        return output;
    }
};

class UnaryOpAST : public ExpressionAST {
    std::unique_ptr<ExpressionAST> operand;
    std::string op;

public:
    UnaryOpAST(std::unique_ptr<ExpressionAST> operand, std::string op) : operand(std::move(operand)),
                                                                         op(std::move(op)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output;
        if (operand) output += operand->generateDOTHeader();
        return output + std::to_string((int64_t) this) + " [label=\"UnaryOpAST" + op + "\"]\n";
    }

    std::string generateDOT() override {
        std::string output;
        if (operand) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) operand.get()) + "\n";
            operand->generateDOT();
        }
        return output;
    }
};

class CallExprAST : public ExpressionAST {
    std::string name;
    std::vector<std::unique_ptr<ExpressionAST>> args;

public:
    CallExprAST(std::string name, std::vector<std::unique_ptr<ExpressionAST>> args) : name(std::move(name)), args(
            std::move(args)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output;
        for (const auto& arg : args) {
            output += arg->generateDOTHeader();
        }
        return output + std::to_string((int64_t) this) + " [label=\"CallExprAST\"]\n";
    }

    std::string generateDOT() override {
        std::string output;
        for (const auto& arg : args) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) arg.get()) + "\n";
            output += arg->generateDOT();
        }
        return output;
    }
};

class PrototypeAST : public ExpressionAST {
    std::string name;
    std::string returnType;
    std::vector<std::pair<std::string, std::string>> args; // Name:Type Pairs

public:
    PrototypeAST(std::string name, std::string returnType, std::vector<std::pair<std::string, std::string>> args)
            : name(std::move(name)), returnType(std::move(returnType)),
              args(std::move(args)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"PrototypeAST " + name + "; Args: ";

        for (const auto &arg: args) {
            output += " " + arg.first + ":" + arg.second;
        }

        output += "\"]\n";

        return output;
    }
};

class ScopeAST : public ExpressionAST {
    std::vector<std::unique_ptr<ExpressionAST>> statements;

public:
    explicit ScopeAST(std::vector<std::unique_ptr<ExpressionAST>> statements) : statements(std::move(statements)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"ScopeAST\"]\n";
        for (const auto &statement: statements) {
            output += statement->generateDOTHeader();
        }
        return output;
    }

    [[nodiscard]] std::string generateDOT() override {
        std::string output;
        for (const auto &statement: statements) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) statement.get()) + "\n";
            output += statement->generateDOT();
        }
        return output;
    }
};

class FunctionAST : public ExpressionAST {
    std::unique_ptr<PrototypeAST> proto;
    std::unique_ptr<ScopeAST> body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> proto, std::unique_ptr<ScopeAST> body) : proto(std::move(proto)),
                                                                                       body(std::move(body)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"FunctionAST\"]\n";
        if (proto) output += proto->generateDOTHeader();
        if (body) output += body->generateDOTHeader();

        return output;
    }

    [[nodiscard]] std::string generateDOT() override {
        std::string output;
        if (proto) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) proto.get()) + "\n";
            output += proto->generateDOT();
        }
        if (body) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) body.get()) + "\n";
            output += body->generateDOT();
        }
        return output;
    }
};

class IfAST : public ExpressionAST {
    std::unique_ptr<ExpressionAST> condition;
    std::unique_ptr<ScopeAST> trueBranch;
    std::unique_ptr<ScopeAST> falseBranch;

public:
    IfAST(std::unique_ptr<ExpressionAST> condition, std::unique_ptr<ScopeAST> trueBranch,
          std::unique_ptr<ScopeAST> falseBranch) : condition(std::move(condition)), trueBranch(std::move(trueBranch)),
                                                   falseBranch(std::move(falseBranch)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"IfAST\"]\n";
        if (condition) output += condition->generateDOTHeader();
        if (trueBranch) output += trueBranch->generateDOTHeader();
        if (falseBranch) output += falseBranch->generateDOTHeader();

        return output;
    }

    [[nodiscard]] std::string generateDOT() override {
        std::string output;
        if (condition) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) condition.get()) + "\n";
            output += condition->generateDOT();
        }
        if (trueBranch) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) trueBranch.get()) + "\n";
            output += trueBranch->generateDOT();
        }
        if (falseBranch) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) falseBranch.get()) + "\n";
            output += falseBranch->generateDOT();
        }
        return output;
    }
};

class ReturnAST : public ExpressionAST {
    std::unique_ptr<ExpressionAST> value;

public:
    explicit ReturnAST(std::unique_ptr<ExpressionAST> value) : value(std::move(value)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"ReturnAST\"]\n";
        if (value) output += value->generateDOTHeader();
        return output;
    }

    [[nodiscard]] std::string generateDOT() override {
        std::string output;
        if (value) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) value.get()) + "\n";
            output += value->generateDOT();
        }
        return output;
    }
};

class LetAST : public ExpressionAST {
    std::string name;
    std::string type;
    std::unique_ptr<ExpressionAST> value;

public:
    LetAST(std::string name, std::string type, std::unique_ptr<ExpressionAST> value) : name(std::move(name)),
                                                                                     type(std::move(type)),
                                                                                     value(std::move(value)) {}

    [[nodiscard]] std::string generateDOTHeader() const override {
        std::string output = std::to_string((int64_t) this) + " [label=\"LetAST " + name + ":" + type + "\"]\n";
        if (value) output += value->generateDOTHeader();
        return output;
    }

    [[nodiscard]] std::string generateDOT() override {
        std::string output;
        if (value) {
            output += std::to_string((int64_t) this) + " -> " + std::to_string((int64_t) value.get()) + "\n";
            output += value->generateDOT();
        }
        return output;
    }
};

#endif //EMMC_AST_H
