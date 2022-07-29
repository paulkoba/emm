//
// Created by fail on 7/25/22.
//

#ifndef EMMC_LEXER_H
#define EMMC_LEXER_H

#include <vector>

#include "token.h"

enum LexerState {
	LEXER_STATE_START,
	LEXER_STATE_NUMBER,
	LEXER_STATE_IDENTIFIER,
	LEXER_STATE_STRING,
};

std::vector<Token> lex(const std::string& input) {
	std::vector<Token> tokens;
	LexerState state = LEXER_STATE_START;
	int line = 1;
	int pos = 0;

	Token current;

	auto addPreviousToken = [&]() {
		if (current.type == TOK_IDENTIFIER) {
			TokenType r = tryMatchKeyword(current.literal);
			if (r != TOK_NONE) {
				current.type = r;
			}
		}

		if (current.type != TOK_NONE) {
			tokens.push_back(current);
		}

		current = {TOK_NONE, "", line};
	};

	while (pos < input.size()) {
		if (state == LEXER_STATE_STRING) {
			// TODO: escape sequences other than "
			if (input[pos] == '"') {
				if (!current.literal.empty() && current.literal.back() == '\\') {
					current.literal.pop_back();
					current.literal += '"';
				} else {
					state = LEXER_STATE_START;
					addPreviousToken();
				}
			} else {
				current.literal += input[pos];
			}
			pos++;
			continue;
		}

		if (input[pos] == '"') {
			if (state != LEXER_STATE_START) {
				addPreviousToken();
			}
			state = LEXER_STATE_STRING;
			current.type = TOK_STRING;
			pos++;
			continue;
		}

		// TODO: Clean this up, internal condition is not obvious and almost
		// certainly wrong
		if (input[pos] == ' ' || input[pos] == '\t') {
			if (state != LEXER_STATE_START) {
				addPreviousToken();
				state = LEXER_STATE_START;
			}
			pos++;
			addPreviousToken();
			continue;
		}

		if (input[pos] == '\n') {
			if (state != LEXER_STATE_START) {
				addPreviousToken();
				state = LEXER_STATE_START;
			}
			line++;
			pos++;
			addPreviousToken();
			continue;
		}

		if (input[pos] == '#') {
			while (pos < input.size() && input[pos] != '\n') {
				pos++;
			}
			continue;
		}

		if (std::isdigit(input[pos])) {
			if (current.type != TOK_NONE && current.type != TOK_INTEGER && current.type != TOK_IDENTIFIER) {
				addPreviousToken();
				state = LEXER_STATE_START;	// i.e. go into the next condition
			}

			if (state == LEXER_STATE_START) {
				state = LEXER_STATE_NUMBER;
				current.type = TOK_INTEGER;
			}

			current.literal += input[pos];
			pos++;
			continue;
		}

		if (std::isalpha(input[pos]) || input[pos] == '_') {
			if (current.type != TOK_NONE && current.type != TOK_IDENTIFIER) {
				addPreviousToken();
				state = LEXER_STATE_START;
			}

			if (state == LEXER_STATE_START) {
				state = LEXER_STATE_IDENTIFIER;
				current.type = TOK_IDENTIFIER;
			}
			if (state == LEXER_STATE_NUMBER) {
				addPreviousToken();
				state = LEXER_STATE_IDENTIFIER;
				current.type = TOK_IDENTIFIER;
			}

			current.literal += input[pos];
			pos++;
			continue;
		}

		TokenType tt = getTrivialTokenType(input[pos]);

		// = and - need special treatment, as they may be part of more complex
		// lexemes.
		if (tt != TOK_NONE && tt != TOK_GREATER && tt != TOK_ASSIGN) {
			addPreviousToken();
			current.type = tt;
			current.literal = input[pos];
			pos++;
			continue;
		}

		if (tt == TOK_GREATER) {
			if (current.type == TOK_MINUS) {
				current = {TOK_OUTPUT_TYPE, "->", line};
			} else {
				addPreviousToken();
				current = {TOK_GREATER, ">", line};
			}
			pos++;
			continue;
		}

		if (tt == TOK_ASSIGN) {
			if (current.type == TOK_ASSIGN) {
				current = {TOK_EQUALS, "==", line};
			} else if (current.type == TOK_LESS) {
				current = {TOK_LESS_OR_EQUAL, "<=", line};
			} else if (current.type == TOK_GREATER) {
				current = {TOK_GREATER_OR_EQUAL, ">=", line};
			} else if (current.type == TOK_NOT) {
                current = {TOK_NOT_EQUALS, "!=", line};
            } else {
				addPreviousToken();
				current = {TOK_ASSIGN, "=", line};
			}
			pos++;
			continue;
		}

		std::cout << "Unexpected character: " << input[pos] << " at line " << line << std::endl;
		return {};
	}

	addPreviousToken();

	tokens.emplace_back(TOK_EOF, "", line);

	return tokens;
}

#endif	// EMMC_LEXER_H
