//
// Created by fail on 7/25/22.
//

#ifndef EMMC_PARSER_H
#define EMMC_PARSER_H

#include "token.h"
#include "ast.h"
#include "logging.h"

static int getTokenPrecedence(TokenType token) {
    switch(token) {
        case TOK_EQUALS:
        case TOK_LESS:
        case TOK_LESS_OR_EQUAL:
        case TOK_GREATER:
        case TOK_GREATER_OR_EQUAL:
            return 10;
        case TOK_PLUS:
        case TOK_MINUS:
            return 20;
        case TOK_PRODUCT:
        case TOK_DIVISION:
            return 30;
        case TOK_ASSIGN:
            return 80;
        default:
            return -1;
    }
}

static std::unique_ptr<ExpressionAST> parseExpression(const std::vector<Token>& tokens, int& idx);

static std::unique_ptr<ExpressionAST> parsePrimary(const std::vector<Token>& tokens, int& idx) {
    const Token& token = tokens[idx];
    ++idx;
    switch(token.type) {
        case TOK_IDENTIFIER:
            return std::make_unique<VariableAST>(token.literal, token.type);
        case TOK_INTEGER:
            return std::make_unique<I64AST>(std::stoll(token.literal));
        case TOK_STRING:
            return std::make_unique<StringAST>(token.literal);
        case TOK_LPAREN: {
            auto expr = parseExpression(tokens, idx);
            if(!expr) return expr;

            if(tokens[idx].type != TOK_RPAREN) {
                compilationError(token.line, "Expected ')', got: " + token.literal);
            }
            ++idx;
            return expr;
        }
        default:
            compilationError(token.line, "Unexpected token: " + token.literal);
            return nullptr;
    }
}

static std::unique_ptr<ExpressionAST> parseUnary(const std::vector<Token>& tokens, int& idx) {
    const Token& token = tokens[idx];
    if(token.type == TOK_MINUS) {
        ++idx;
        auto expr = parsePrimary(tokens, idx);
        if(!expr) return expr;
        return std::make_unique<UnaryOpAST>(std::move(expr), token.literal);
    }

    return parsePrimary(tokens, idx);
}

static std::unique_ptr<ExpressionAST> parseBinaryOp(const std::vector<Token>& tokens, int& idx, std::unique_ptr<ExpressionAST> left, int precedence) {
    while(true) {
        auto binOpPrecedence = getTokenPrecedence(tokens[idx].type);
        if(binOpPrecedence < precedence) {
            return left;
        }

        const Token& token = tokens[idx];
        ++idx;
        auto right = parseUnary(tokens, idx);
        if(!right) return right;

        auto nextPrecedence = getTokenPrecedence(tokens[idx].type);

        if(binOpPrecedence < nextPrecedence) {
            right = parseBinaryOp(tokens, idx, std::move(right), binOpPrecedence + 1);
            if(!right) return right;
        }

        left = std::make_unique<BinaryExprAST>(std::move(left), std::move(right), token.literal);
    }
}

static std::unique_ptr<ExpressionAST> parseExpression(const std::vector<Token>& tokens, int& idx) {
    auto lhs = parseUnary(tokens, idx);
    if (!lhs) return nullptr;

    return parseBinaryOp(tokens, idx, std::move(lhs), 0);
}

static std::unique_ptr<PrototypeAST> parsePrototype(const std::vector<Token>& tokens, int& idx) {
    std::string name = tokens[idx].literal;
    idx++;
    if(tokens[idx].type != TOK_LPAREN) {
        compilationError(tokens[idx].line, "Expected '('");
        return nullptr;
    }
    idx++;
    // Parse arguments
    std::vector<std::pair<std::string, std::string>> args;

    while(tokens[idx].type != TOK_RPAREN) {
        std::pair<std::string, std::string> nextPair;

        if(tokens[idx].type != TOK_IDENTIFIER) {
            compilationError(tokens[idx].line, "Expected identifier, got " + tokens[idx].literal);
            return nullptr;
        }
        nextPair.first = tokens[idx].literal;
        ++idx;

        if(tokens[idx].type != TOK_COLON) {
            compilationError(tokens[idx].line, "Expected \":\" , got " + tokens[idx].literal);
            return nullptr;
        }
        ++idx;

        if(tokens[idx].type != TOK_IDENTIFIER) {
            compilationError(tokens[idx].line, "Expected identifier, got " + tokens[idx].literal);
            return nullptr;
        }
        nextPair.second = tokens[idx].literal;
        ++idx;

        args.push_back(nextPair);

        if(tokens[idx].type == TOK_COMMA) ++idx;
    }

    ++idx;

    if(tokens[idx].type != TOK_OUTPUT_TYPE) {
        compilationError(tokens[idx].line, "Expected \"->\" , got " + tokens[idx].literal);
        return nullptr;
    }
    ++idx;

    if(tokens[idx].type != TOK_IDENTIFIER) {
        compilationError(tokens[idx].line, "Expected return type , got " + tokens[idx].literal);
        return nullptr;
    }
    std::string returnType = tokens[idx].literal;
    ++idx;

    return std::make_unique<PrototypeAST>(name, returnType, args);
}

static std::unique_ptr<FunctionAST> parseFunction(const std::vector<Token>& tokens, int& idx) {
    std::unique_ptr<PrototypeAST> prototype = parsePrototype(tokens, idx);

    if(!prototype) {
        return nullptr;
    }

    if(tokens[idx].type != TOK_LBRACE) {
        compilationError(tokens[idx].line, "Expected \"{\" , got " + tokens[idx].literal);
        return nullptr;
    }

    ++idx;

    auto body = parseExpression(tokens, idx);

    if(tokens[idx].type != TOK_RBRACE) {
        compilationError(tokens[idx].line, "Expected \"}\" , got " + tokens[idx].literal);
        return nullptr;
    }

    return std::make_unique<FunctionAST>(std::move(prototype), std::move(body));
}

static std::vector<std::unique_ptr<ExpressionAST>> parseEverything(const std::vector<Token>& tokens) {
    std::vector<std::unique_ptr<ExpressionAST>> result;
    int idx = 0;
    while(idx < tokens.size()) {
        if(tokens[idx].type == TOK_FN) {
            idx++;
            auto funcResult = parseFunction(tokens, idx);
            if(!funcResult) {
                compilationError(tokens[idx].line, "Unexpected token " + tokens[idx].literal);
                continue;
            }
            result.push_back(std::move(funcResult));
        } else {
            compilationError(tokens[idx].line, "Unexpected token " + tokens[idx].literal);
            ++idx;
        }
    }

    return result;
}

#endif //EMMC_PARSER_H
