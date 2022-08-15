//
// Created by fail on 8/15/22.
//

#ifndef EMMC_TOKEN_H
#define EMMC_TOKEN_H

#include <llvm/ADT/StringRef.h>

#include <cstdint>
#include <ostream>

#include "../Basic/Logging.h"
#include "TokenTypes.h"

class Token {
	TokenType::TokenType type;
	std::int64_t line;

   public:
	Token() : type(TokenType::NONE), line(-1) {}
	Token(TokenType::TokenType type, std::int64_t line) : type(type), line(line) {}

	virtual ~Token() = default;

	[[nodiscard]] constexpr TokenType::TokenType getType() const { return type; }
	[[nodiscard]] constexpr std::int64_t getLine() const { return line; }

	friend std::ostream& operator<<(std::ostream& os, const Token& token);
};

std::ostream& operator<<(std::ostream& os, const Token& token) {
	os << "{" << toString(token.type) << "\", " << token.line << "}";
	return os;
}

// We use this for storing both identifiers and keywords
class IdentifierToken : public Token {
	llvm::StringRef value;

   public:
	IdentifierToken(llvm::StringRef value, std::int64_t line) : Token(TokenType::IDENTIFIER, line), value(value) {}
	IdentifierToken(llvm::StringRef value, std::int64_t line, TokenType::TokenType type)
		: Token(type, line), value(value) {}

	[[nodiscard]] constexpr llvm::StringRef getValue() const { return value; }

	~IdentifierToken() override = default;

	[[nodiscard]] bool operator!() const { return value.empty(); }
};

class OperatorToken : public Token {
	llvm::StringRef value;

   public:
	OperatorToken(llvm::StringRef value, std::int64_t line, TokenType::TokenType type)
		: Token(type, line), value(value) {}

	[[nodiscard]] constexpr llvm::StringRef getValue() const { return value; }

	~OperatorToken() override = default;

	[[nodiscard]] bool operator!() const { return getType() == TokenType::NONE; }
};

class NumericToken : public Token {
	llvm::StringRef value;
	llvm::StringRef literal;

   public:
	NumericToken(llvm::StringRef value, llvm::StringRef literal, std::int64_t line, TokenType::TokenType type)
		: Token(type, line), value(value), literal(literal) {}

	[[nodiscard]] constexpr llvm::StringRef getValue() const { return value; }
	[[nodiscard]] constexpr llvm::StringRef getLiteral() const { return literal; }

	~NumericToken() override = default;

	[[nodiscard]] bool operator!() const { return getType() == TokenType::NONE; }
};

#endif	// EMMC_TOKEN_H
