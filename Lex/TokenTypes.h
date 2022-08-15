//
// Created by fail on 8/14/22.
//

#ifndef EMMC_TOKENTYPES_H
#define EMMC_TOKENTYPES_H

#include "../Basic/EnumMagic.h"
#include "../Basic/Logging.h"

DECLARE_ENUM(TokenType, NONE, END_OF_FILE, IDENTIFIER, INTEGER, STRING, LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET,
			 RBRACKET, COLON, PLUS, MINUS, PRODUCT, DIVISION, EQUALS, ASSIGN, GREATER, LESS, OUTPUT_TYPE, SEMICOLON,
			 LESS_OR_EQUAL, GREATER_OR_EQUAL, COMMA, MODULO, NOT_EQUALS, SHIFT_LEFT, SHIFT_RIGHT, LOGICAL_AND,
			 LOGICAL_OR, DECREMENT, INCREMENT, PLUS_ASSIGN, MINUS_ASSIGN, PRODUCT_ASSIGN, DIVISION_ASSIGN,
			 MODULO_ASSIGN, BITWISE_AND, NOT, FLOATING_POINT, DOT, KW_IF, KW_ELSE, KW_WHILE, KW_RETURN, KW_LET, KW_FN,
			 KW_TRUE, KW_FALSE, KW_MUT, KW_EXTERN, KW_STRUCT, KW_IMPL, KW_AS)

TokenType::TokenType tryMatchKeyword(const std::string& input) {
	static std::unordered_map<std::string, TokenType::TokenType> keywords = {
		{"if", TokenType::KW_IF},		  {"else", TokenType::KW_ELSE},		{"while", TokenType::KW_WHILE},
		{"return", TokenType::KW_RETURN}, {"let", TokenType::KW_LET},		{"fn", TokenType::KW_FN},
		{"true", TokenType::KW_TRUE},	  {"false", TokenType::KW_FALSE},	{"mut", TokenType::KW_MUT},
		{"as", TokenType::KW_AS},		  {"extern", TokenType::KW_EXTERN}, {"struct", TokenType::KW_STRUCT},
		{"impl", TokenType::KW_IMPL}};

	return keywords.count(input) ? keywords[input] : TokenType::NONE;
}

static std::int64_t getOperatorPrecedence(TokenType::TokenType token) {
	switch (token) {
		case TokenType::TokenType::ASSIGN:
		case TokenType::TokenType::PLUS_ASSIGN:
		case TokenType::TokenType::MINUS_ASSIGN:
		case TokenType::TokenType::PRODUCT_ASSIGN:
		case TokenType::TokenType::DIVISION_ASSIGN:
		case TokenType::TokenType::MODULO_ASSIGN:
			return 1;
		case TokenType::TokenType::LOGICAL_OR:
		case TokenType::TokenType::LOGICAL_AND:
			return 5;
		case TokenType::TokenType::EQUALS:
		case TokenType::TokenType::NOT_EQUALS:
		case TokenType::TokenType::GREATER:
		case TokenType::TokenType::LESS:
		case TokenType::TokenType::GREATER_OR_EQUAL:
		case TokenType::TokenType::LESS_OR_EQUAL:
			return 10;
		case TokenType::TokenType::PLUS:
		case TokenType::TokenType::MINUS:
			return 20;
		case TokenType::TokenType::PRODUCT:
		case TokenType::TokenType::DIVISION:
		case TokenType::TokenType::MODULO:
			return 30;
		case TokenType::TokenType::KW_AS:
			return 40;
		case TokenType::TokenType::LBRACKET:
			return 50;
		case TokenType::TokenType::DOT:
			return 60;
		case TokenType::TokenType::NONE:
			return -1;
		default:
			compilationError("Precedence not defined for operator " + std::string(TokenType::toString(token)) +
							 ". This is a compiler bug.");
			return -1;
	}
}

std::string functionNameFromTokenType(TokenType::TokenType op) {
	switch (op) {
		case TokenType::PLUS:
			return "operator_add";
		case TokenType::MINUS:
			return "operator_sub";
		case TokenType::PRODUCT:
			return "operator_mul";
		case TokenType::DIVISION:
			return "operator_div";
		case TokenType::MODULO:
			return "operator_mod";
		case TokenType::EQUALS:
			return "operator_equals";
		case TokenType::ASSIGN:
			return "operator_assign";
		case TokenType::GREATER:
			return "operator_greater";
		case TokenType::LESS:
			return "operator_less";
		case TokenType::LESS_OR_EQUAL:
			return "operator_less_or_equal";
		case TokenType::GREATER_OR_EQUAL:
			return "operator_greater_or_equal";
		case TokenType::NOT_EQUALS:
			return "operator_not_equals";
		default:
			return "operator_unknown";
	}
}

#endif	// EMMC_TOKENTYPES_H
