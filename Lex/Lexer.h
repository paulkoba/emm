//
// Created by fail on 8/14/22.
//

#ifndef EMMC_LEXER_H
#define EMMC_LEXER_H

#include <llvm/ADT/StringRef.h>

#include <fstream>
#include <string>

#include "Token.h"

class Lexer {
	char* buffer = nullptr;
    char* bufferEnd = nullptr;
	char* current = nullptr;
    std::int64_t line = 1;

    static void consumeWhitespace(char*& current, std::int64_t& line) {
        while(std::isspace(*current)) {
            if(*current == '\n') {
                line++;
            }
            current++;
        }
    }

   public:
    void consumeWhitespace() {
        while(std::isspace(*current)) {
            if(*current == '\n') {
                line++;
            }
            current++;
        }
    }

	explicit Lexer(const char* filename) {
		std::ifstream ifs;
		ifs.open(filename, std::ios::binary);
		ifs.seekg(0, std::ios::end);
		long length = ifs.tellg();
		ifs.seekg(0, std::ios::beg);
		buffer = new char[length];
		ifs.read(buffer, length);

		current = buffer;
        bufferEnd = buffer + length;
	}

	~Lexer() { delete[] buffer; }

    [[nodiscard]] IdentifierToken peekIdentifierOrKeyword() {
        char* start = current;
        std::int64_t tempLine = line;

        consumeWhitespace(start, tempLine);

        char* identifierStart = start;

        if(!std::isalpha(*start)) {
            return {"", tempLine, TokenType::NONE};
        }

        while(std::isalnum(*start) || *start == '_') {
            start++;
        }

        auto stringRef = llvm::StringRef(identifierStart, start - identifierStart);

        auto type = tryMatchKeyword(stringRef.str());

        // If this is not a keyword, then this is an identifier
        if(type == TokenType::NONE) {
            type = TokenType::IDENTIFIER;
        }

        return {stringRef, tempLine, type};
    }

    [[nodiscard]] IdentifierToken peekIdentifier() {
        char* start = current;
        std::int64_t tempLine = line;

        consumeWhitespace(start, tempLine);

        char* identifierStart = start;

        if(!std::isalpha(*start)) {
            return {"", tempLine, TokenType::NONE};
        }

        while(std::isalnum(*start) || *start == '_') {
            start++;
        }

        auto stringRef = llvm::StringRef(identifierStart, start - identifierStart);

        return {stringRef, tempLine, TokenType::IDENTIFIER};
    }

    [[nodiscard]] IdentifierToken peekKeyword() {
        char* start = current;
        std::int64_t tempLine = line;

        consumeWhitespace(start, tempLine);

        char* identifierStart = start;

        if(!std::isalpha(*start)) {
            return {"", tempLine, TokenType::NONE};
        }

        while(std::isalnum(*start) || *start == '_') {
            start++;
        }

        auto stringRef = llvm::StringRef(identifierStart, start - identifierStart);

        auto type = tryMatchKeyword(stringRef.str());

        return {stringRef, tempLine, type};
    }

    IdentifierToken consumeIdentifierOrKeyword() {
        IdentifierToken token = peekIdentifierOrKeyword();
        if(!token) {
            compilationError(line, "Expected identifier or keyword");
        }
        // This is safe, because buffer will not be actually modified
        current = const_cast<char *>(token.getValue().end());
        line = token.getLine();
        return token;
    }

    IdentifierToken consumeIdentifier() {
        IdentifierToken token = peekIdentifier();
        if(!token) {
            compilationError(line, "Expected identifier");
        }
        // This is safe, because buffer will not be actually modified
        current = const_cast<char *>(token.getValue().end());
        line = token.getLine();
        return token;
    }

    IdentifierToken consumeKeyword() {
        IdentifierToken token = peekKeyword();
        if(!token) {
            compilationError(line, "Expected keyword");
        }

        // This is safe, because buffer will not be actually modified
        current = const_cast<char *>(token.getValue().end());
        line = token.getLine();
        return token;
    }

     OperatorToken peekOperator() {
        char* start = current;
        std::int64_t tempLine = line;
        while(std::isspace(*start)) {
            if(*start == '\n') {
                tempLine++;
            }
            start++;
        }

        auto operatorStart = start;

        // This looks terrible, but it should be optimized away by the compiler

        // Handle operators with two characters
        if (*start == '=' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::EQUALS};
        } else if (*start == '!' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::NOT_EQUALS};
        } else if (*start == '<' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::LESS_OR_EQUAL};
        } else if (*start == '>' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::GREATER_OR_EQUAL};
        } else if (*start == '<' && *(start + 1) == '<') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::SHIFT_LEFT};
        } else if (*start == '>' && *(start + 1) == '>') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::SHIFT_RIGHT};
        } else if (*start == '&' && *(start + 1) == '&') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::LOGICAL_AND};
        } else if (*start == '|' && *(start + 1) == '|') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::LOGICAL_OR};
        } else if (*start == '+' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::PLUS_ASSIGN};
        } else if (*start == '-' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::MINUS_ASSIGN};
        } else if (*start == '*' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::PRODUCT_ASSIGN};
        } else if (*start == '/' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::DIVISION_ASSIGN};
        } else if (*start == '%' && *(start + 1) == '=') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::MODULO_ASSIGN};
        } else if (*start == 'a' && *(start + 1) == 's') {
            return {llvm::StringRef(operatorStart, 2), tempLine, TokenType::KW_AS};
        }

        // Handle operators with one character
        switch (*start) {
            case '+':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::PLUS};
            case '-':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::MINUS};
            case '*':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::PRODUCT};
            case '/':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::DIVISION};
            case '%':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::MODULO};
            case '<':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::LESS};
            case '>':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::GREATER};
            case '=':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::ASSIGN};
            case '!':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::NOT};
            case '.':
                return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::DOT};
        }

        return {llvm::StringRef(operatorStart, 1), tempLine, TokenType::NONE};
    }

    OperatorToken consumeOperator() {
        OperatorToken token = peekOperator();
        // This is safe, because buffer will actually be modified
        current = const_cast<char *>(token.getValue().end());
        line = token.getLine();
        return token;
    }

    bool consumeLeftBracket() {
        consumeWhitespace();
        if(*current != '[') {
            compilationError(line, "Expected left bracket, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeRightBracket() {
        consumeWhitespace();
        if(*current != ']') {
            compilationError(line, "Expected right bracket, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeLeftParen() {
        consumeWhitespace();
        if(*current != '(') {
            compilationError(line, "Expected left parenthesis, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeRightParen() {
        consumeWhitespace();
        if(*current != ')') {
            compilationError(line, "Expected right parenthesis, got: \"" + std::string(1, *current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeLeftBrace() {
        consumeWhitespace();
        if(*current != '{') {
            compilationError(line, "Expected left brace, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeRightBrace() {
        consumeWhitespace();
        if(*current != '}') {
            compilationError(line, "Expected right brace, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeComma() {
        consumeWhitespace();
        if(*current != ',') {
            compilationError(line, "Expected comma, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeColon() {
        consumeWhitespace();
        if(*current != ':') {
            compilationError(line, "Expected colon, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current++;
        return true;
    }

    bool consumeOutputOperator() {
        consumeWhitespace();
        if(*current != '-' && *(current + 1) != '>') {
            compilationError(line, "Expected output operator, got: \"" + std::to_string(*current) + "\"");
            return false;
        }
        current += 2;
        return true;
    }

    // Returns whether the next token is a left brace
    [[nodiscard]] bool peekLeftBrace() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == '{';
    }

    // Returns whether the next token is a right brace
    [[nodiscard]] bool peekRightBrace() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == '}';
    }

    // Returns whether the next token is a left bracket
    [[nodiscard]] bool peekLeftBracket() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == '[';
    }

    // Returns whether the next token is a right bracket
    [[nodiscard]] bool peekRightBracket() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == ']';
    }

    // Returns whether the next token is a left parenthesis
    [[nodiscard]] bool peekLeftParen() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == '(';
    }

    // Returns whether the next token is a right parenthesis
    [[nodiscard]] bool peekRightParen() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == ')';
    }

    // Returns whether the next token is a comma
    [[nodiscard]] bool peekComma() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == ',';
    }

    // Returns whether the next token is a colon
    [[nodiscard]] bool peekColon() const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);

        return *start == ':';
    }

    [[nodiscard]] bool isEOF() const {
        return current >= bufferEnd;
    }

    [[nodiscard]] std::int64_t getLine() const {
        return line;
    }

    // Minuses are handled as unary operators
    [[nodiscard]] NumericToken peekNumber() const {
        TokenType::TokenType outputType = TokenType::INTEGER;
        char* end = current;
        std::int64_t tempLine = line;
        consumeWhitespace(end, tempLine);
        char* start = end;

        if(!isdigit(*end)) {
            return {llvm::StringRef(end, 0), llvm::StringRef(end, 0), tempLine, TokenType::NONE};
        }

        while(isdigit(*end)) {
            end++;
        }

        if(*end == '.') {
            outputType = TokenType::FLOATING_POINT;

            end++;
            while(isdigit(*end)) {
                end++;
            }
        }

        char* literalStart = nullptr;
        char* literalEnd = end;
        // Handle type literal
        if(std::isalpha(*literalEnd)) {
            literalStart = literalEnd;
            while(std::isalnum(*literalEnd)) {
                literalEnd++;
            }
        }

        if(literalStart != nullptr) {
            return {llvm::StringRef(start, end - start),
                                llvm::StringRef(literalStart, literalEnd - literalStart), tempLine, outputType};
        } else {
            return {llvm::StringRef(start, end - start), "", tempLine, outputType};
        }
    }

    NumericToken consumeNumber() {
        NumericToken output = peekNumber();

        if(!output.getLiteral().empty()) {
            current = const_cast<char *>(output.getLiteral().end());
        } else {
            current = const_cast<char *>(output.getValue().end());
        }

        line = output.getLine();

        return output;
    }

    [[nodiscard]] llvm::StringRef lookahead(std::int64_t chars) const {
        char* start = current;
        std::int64_t tempLine = line;
        consumeWhitespace(start, tempLine);
        return {start, static_cast<size_t>(chars)};
    }
};

template <typename T>
void compilationError(Lexer* lexer, T arg) {
    std::cout << "Compilation error at line " << lexer->getLine() << ": " << arg << std::endl;
    std::cout << "Source: " << std::endl;
    std::cout << lexer->lookahead(20).str() << std::endl;
    raise(SIGSEGV);
}

#endif