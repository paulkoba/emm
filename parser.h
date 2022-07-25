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
            return 0;
    }
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

    if(tokens[idx].type != TOK_RBRACE) {
        compilationError(tokens[idx].line, "Expected \"}\" , got " + tokens[idx].literal);
        return nullptr;
    }

    return std::make_unique<FunctionAST>(std::move(prototype), nullptr);
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
