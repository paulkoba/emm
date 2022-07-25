//
// Created by fail on 7/25/22.
//

#ifndef EMMC_LEXER_H
#define EMMC_LEXER_H

#include "token.h"
#include <vector>

enum LexerState {
    LEXER_STATE_START,
    LEXER_STATE_NUMBER,
    LEXER_STATE_IDENTIFIER,
};

std::vector<Token> lex(const std::string& input) {
    std::vector<Token> tokens;
    LexerState state = LEXER_STATE_START;
    int line = 1;
    int pos = 0;

    Token current;

    auto addPreviousToken = [&]() {
        if(current.type != TOK_NONE && current.type != TOK_STRING) {
            tokens.push_back(current);
        }

        current = {TOK_NONE, "", line};
    };

    while(pos < input.size()) {
        if(input[pos] == ' ' || input[pos] == '\t') {
            if(state != LEXER_STATE_START) {
                addPreviousToken();
                state = LEXER_STATE_START;
            }
            pos++;
            addPreviousToken();
            continue;
        }

        if(input[pos] == '\n') {
            if(state != LEXER_STATE_START) {
                addPreviousToken();
                state = LEXER_STATE_START;
            }
            line++;
            pos++;
            addPreviousToken();
            continue;
        }

        if(input[pos] == '#') {
            while(pos < input.size() && input[pos] != '\n') {
                pos++;
            }
            continue;
        }

        if(std::isdigit(input[pos])) {
            if(current.type != TOK_NONE && current.type != TOK_INTEGER && current.type != TOK_IDENTIFIER) {
                addPreviousToken();
                state = LEXER_STATE_START;
            }

            if(state == LEXER_STATE_START) {
                state = LEXER_STATE_NUMBER;
                current.type = TOK_INTEGER;
            }

            current.literal += input[pos];
            pos++;
            continue;
        }

        if(std::isalpha(input[pos])) {
            if(current.type != TOK_NONE && current.type != TOK_IDENTIFIER) {
                addPreviousToken();
                state = LEXER_STATE_START;
            }

            if(state == LEXER_STATE_START) {
                state = LEXER_STATE_IDENTIFIER;
                current.type = TOK_IDENTIFIER;
            }
            if(state == LEXER_STATE_NUMBER) {
                addPreviousToken();
                state = LEXER_STATE_IDENTIFIER;
                current.type = TOK_IDENTIFIER;
            }

            current.literal += input[pos];
            pos++;
            continue;
        }

        TokenType tt = getTrivialTokenType(input[pos]);

        if(tt != TOK_NONE && tt != TOK_GREATER) {
            addPreviousToken();
            current.type = tt;
            current.literal = input[pos];
            pos++;
            continue;
        }

        if(tt == TOK_GREATER) {
            if(current.type == TOK_MINUS) {
                current = {TOK_OUTPUT_TYPE, "->", line};
            } else {
                addPreviousToken();
                current = {TOK_GREATER, ">", line};
            }
            pos++;
            continue;
        }

        std::cout << "Unknown character: " << input[pos] << std::endl;
        return {};
    }

    addPreviousToken();

    tokens.emplace_back(TOK_EOF, "", line);

    return tokens;
}

#endif //EMMC_LEXER_H
