//
// Created by fail on 7/25/22.
//

// TODO: There is a lot of duplication, should think about how to clean this up.

#ifndef EMMC_TOKEN_H
#define EMMC_TOKEN_H

#include <sstream>
#include <string>
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
	TOK_EQUALS = -16,
	TOK_ASSIGN = -17,
	TOK_GREATER = -18,
	TOK_LESS = -19,
	TOK_OUTPUT_TYPE = -20,
	TOK_SEMICOLON = -21,
	TOK_LESS_OR_EQUAL = -22,
	TOK_GREATER_OR_EQUAL = -23,
    TOK_AS = -24,
	TOK_COMMA = -25,
    TOK_MODULO = -26,
    TOK_NOT_EQUALS = -27,
    TOK_NOT = -28,
    TOK_FLOATING_POINT = -29,

	// KEYWORDS
	TOK_IF = -100,
	TOK_ELSE = -101,
	TOK_WHILE = -102,
	TOK_RETURN = -103,
	TOK_LET = -104,
	TOK_FN = -105,
	TOK_TRUE = -106,
	TOK_FALSE = -107,
	TOK_MUT = -108,
};

// TODO: Get rid of static variable
TokenType tryMatchKeyword(const std::string& input) {
	static std::unordered_map<std::string, TokenType> keywords = {
		{"if", TOK_IF}, {"else", TOK_ELSE}, {"while", TOK_WHILE}, {"return", TOK_RETURN}, {"let", TOK_LET},
		{"fn", TOK_FN}, {"true", TOK_TRUE}, {"false", TOK_FALSE}, {"mut", TOK_MUT}, {"as", TOK_AS}
	};

	return keywords.count(input) ? keywords[input] : TOK_NONE;
}

TokenType getTrivialTokenType(char ch) {
	switch (ch) {
		case '+':
			return TOK_PLUS;
		case '-':
			return TOK_MINUS;
		case '*':
			return TOK_PRODUCT;
		case '/':
			return TOK_DIVISION;
        case '%':
            return TOK_MODULO;
		case '=':
			return TOK_ASSIGN;
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
		case ',':
			return TOK_COMMA;
        case '!':
            return TOK_NOT;
		default:
			return TOK_NONE;
	}
}

std::string tokenTypeToString(TokenType type) {
	switch (type) {
		case TOK_NONE:
			return "TOK_NONE";
		case TOK_EOF:
			return "TOK_EOF";
		case TOK_IDENTIFIER:
			return "TOK_IDENTIFIER";
		case TOK_INTEGER:
			return "TOK_INTEGER";
		case TOK_STRING:
			return "TOK_STRING";
		case TOK_LPAREN:
			return "TOK_LPAREN";
		case TOK_RPAREN:
			return "TOK_RPAREN";
		case TOK_LBRACE:
			return "TOK_LBRACE";
		case TOK_RBRACE:
			return "TOK_RBRACE";
		case TOK_LBRACKET:
			return "TOK_LBRACKET";
		case TOK_RBRACKET:
			return "TOK_RBRACKET";
		case TOK_COLON:
			return "TOK_COLON";
		case TOK_PLUS:
			return "TOK_PLUS";
		case TOK_MINUS:
			return "TOK_MINUS";
		case TOK_PRODUCT:
			return "TOK_PRODUCT";
		case TOK_DIVISION:
			return "TOK_DIVISION";
		case TOK_EQUALS:
			return "TOK_EQUALS";
		case TOK_ASSIGN:
			return "TOK_ASSIGN";
		case TOK_GREATER:
			return "TOK_GREATER";
		case TOK_LESS:
			return "TOK_LESS";
		case TOK_OUTPUT_TYPE:
			return "TOK_OUTPUT_TYPE";
		case TOK_SEMICOLON:
			return "TOK_SEMICOLON";
		case TOK_LESS_OR_EQUAL:
			return "TOK_LESS_OR_EQUAL";
		case TOK_GREATER_OR_EQUAL:
			return "TOK_GREATER_OR_EQUAL";
        case TOK_AS:
            return "TOK_AS";
		case TOK_COMMA:
			return "TOK_COMMA";
		case TOK_IF:
			return "TOK_IF";
		case TOK_ELSE:
			return "TOK_ELSE";
		case TOK_WHILE:
			return "TOK_WHILE";
		case TOK_RETURN:
			return "TOK_RETURN";
		case TOK_LET:
			return "TOK_LET";
		case TOK_FN:
			return "TOK_FN";
		case TOK_TRUE:
			return "TOK_TRUE";
		case TOK_FALSE:
			return "TOK_FALSE";
		case TOK_MUT:
			return "TOK_MUT";
        case TOK_MODULO:
            return "TOK_MODULO";
        case TOK_NOT_EQUALS:
            return "TOK_NOT_EQUALS";
        case TOK_NOT:
            return "TOK_NOT";
        case TOK_FLOATING_POINT:
            return "TOK_FLOATING_POINT";
		default:
			return "UNKNOWN";
	}
}

struct Token {
	TokenType type;
	std::string literal;
	int64_t line;

	Token() : type(TOK_NONE), line(0) {}
	Token(TokenType type, std::string literal, int line) : type(type), literal(std::move(literal)), line(line) {}

	friend std::ostream& operator<<(std::ostream& os, const Token& token);
};

std::ostream& operator<<(std::ostream& os, const Token& token) {
	os << "{" << tokenTypeToString(token.type) << ", \"" << token.literal << "\", " << token.line << "}";
	return os;
}

#endif	// EMMC_TOKEN_H
