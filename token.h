//
// Created by fail on 7/25/22.
//

#ifndef EMMC_TOKEN_H
#define EMMC_TOKEN_H

#include <string>
#include <sstream>
#include <utility>

enum TokenType {
    TOK_NONE = 0,
    TOK_EOF = -1,
    TOK_IDENTIFIER = -2,
    TOK_INTEGER = -3,
    TOK_STRING = -4,
    TOK_LPAREN = -5,
    TOK_RPAREN = -6,
    TOK_LBRACE = -7,
    TOK_RBRACE = -8,
    TOK_LBRACKET = -9,
    TOK_RBRACKET = -10,
    TOK_COLON = -11,
    TOK_PLUS = -12,
    TOK_MINUS = -13,
    TOK_PRODUCT = -14,
    TOK_DIVISION = -15,
    TOK_EQUAL = -16,
    TOK_ASSIGN = -17,
    TOK_GREATER = -18,
    TOK_LESS = -19,
    TOK_OUTPUT_TYPE = -20,
    TOK_SEMICOLON = -21,
};

std::string tokenTypeToString(TokenType type) {
    switch (type) {
        case TOK_NONE: return "TOK_NONE";
        case TOK_EOF: return "TOK_EOF";
        case TOK_IDENTIFIER: return "TOK_IDENTIFIER";
        case TOK_INTEGER: return "TOK_INTEGER";
        case TOK_STRING: return "TOK_STRING";
        case TOK_LPAREN: return "TOK_LPAREN";
        case TOK_RPAREN: return "TOK_RPAREN";
        case TOK_LBRACE: return "TOK_LBRACE";
        case TOK_RBRACE: return "TOK_RBRACE";
        case TOK_LBRACKET: return "TOK_LBRACKET";
        case TOK_RBRACKET: return "TOK_RBRACKET";
        case TOK_COLON: return "TOK_COLON";
        case TOK_PLUS: return "TOK_PLUS";
        case TOK_MINUS: return "TOK_MINUS";
        case TOK_PRODUCT: return "TOK_PRODUCT";
        case TOK_DIVISION: return "TOK_DIVISION";
        case TOK_EQUAL: return "TOK_EQUAL";
        case TOK_ASSIGN: return "TOK_ASSIGN";
        case TOK_GREATER: return "TOK_GREATER";
        case TOK_LESS: return "TOK_LESS";
        case TOK_OUTPUT_TYPE: return "TOK_OUTPUT_TYPE";
        case TOK_SEMICOLON: return "TOK_SEMICOLON";
    }
}

struct Token {
    TokenType type;
    std::string literal;
    int line;

    Token() : type(TOK_NONE), line(0) {}
    Token(TokenType type, std::string  literal, int line) : type(type), literal(std::move(literal)), line(line) {}

    friend std::ostream& operator <<(std::ostream& os, const Token& token);
};

std::ostream& operator <<(std::ostream& os, const Token& token) {
    os << "{" << tokenTypeToString(token.type) << ", \"" << token.literal << "\", " << token.line << "}";
    return os;
}

TokenType getTrivialTokenType(char ch) {
    switch(ch) {
        case '+':
            return TOK_PLUS;
        case '-':
            return TOK_MINUS;
        case '*':
            return TOK_PRODUCT;
        case '/':
            return TOK_DIVISION;
        case '=':
            return TOK_EQUAL;
        case '>':
            return TOK_GREATER;
        case '<':
            return TOK_LESS;
        case ';':
            return TOK_SEMICOLON;
        case '(':
            return TOK_LPAREN;
        case ')':
            return TOK_RPAREN;
        case '{':
            return TOK_LBRACE;
        case '}':
            return TOK_RBRACE;
        case '[':
            return TOK_LBRACKET;
        case ']':
            return TOK_RBRACKET;
        case ':':
            return TOK_COLON;
        default:
            return TOK_NONE;
    }
}


#endif //EMMC_TOKEN_H
