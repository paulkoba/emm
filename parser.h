//
// Created by fail on 7/25/22.
//

#ifndef EMMC_PARSER_H
#define EMMC_PARSER_H

#include "ast.h"
#include "logging.h"
#include "token.h"

static std::unique_ptr<IfAST> parseCondition(const std::vector<Token>& tokens, int& idx);
static std::unique_ptr<WhileAST> parseWhile(const std::vector<Token>& tokens, int& idx);
static std::unique_ptr<ReturnAST> parseReturn(const std::vector<Token>& tokens, int& idx);
static std::unique_ptr<LetAST> parseLet(const std::vector<Token>& tokens, int& idx);
static std::unique_ptr<BaseASTNode> parseExpression(const std::vector<Token>& tokens, int& idx);

static int getTokenPrecedence(TokenType token) {
	switch (token) {
		case TOK_ASSIGN:
			return 1;
		case TOK_AS:
			return 9;
		case TOK_EQUALS:
		case TOK_LESS:
		case TOK_LESS_OR_EQUAL:
		case TOK_GREATER:
		case TOK_GREATER_OR_EQUAL:
		case TOK_NOT_EQUALS:
			return 10;
		case TOK_PLUS:
		case TOK_MINUS:
			return 20;
		case TOK_PRODUCT:
		case TOK_DIVISION:
		case TOK_MODULO:
			return 30;
		default:
			return -1;
	}
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::vector<std::unique_ptr<BaseASTNode>> parseArgList(const std::vector<Token>& tokens, int& idx) {
	std::vector<std::unique_ptr<BaseASTNode>> args;
	if (idx < tokens.size() && tokens[idx].type == TOK_RPAREN) {
		idx++;
		return args;
	}

	auto first = parseExpression(tokens, idx);
	args.push_back(std::move(first));

	while (idx < tokens.size() && tokens[idx].type == TOK_COMMA) {
		idx++;
		auto next = parseExpression(tokens, idx);
		args.push_back(std::move(next));
	}

	if (idx < tokens.size() && tokens[idx].type == TOK_RPAREN) {
		idx++;
	} else {
		compilationError(tokens[idx].line, "Expected ')'");
	}

	return args;
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<BaseASTNode> parsePrimary(const std::vector<Token>& tokens, int& idx) {
	const Token& token = tokens[idx];
	++idx;
	switch (token.type) {
		case TOK_IDENTIFIER:
			if (tokens[idx].type == TOK_LPAREN) {
				return std::make_unique<CallExprAST>(token.literal, parseArgList(tokens, ++idx));
			}
			return std::make_unique<VariableAST>(token.literal, "");
		case TOK_INTEGER:
			if (tokens[idx].type == TOK_IDENTIFIER) {
				const auto& type = tokens[idx].literal;
				auto r = fromLiteral(token.literal, type);
				if (r) {
					idx++;
					return std::move(r);
				}
			}
			return std::make_unique<I64AST>(std::stoll(token.literal));
		case TOK_FLOATING_POINT:
			if (tokens[idx].type == TOK_IDENTIFIER) {
				const auto& type = tokens[idx].literal;
				auto r = fromLiteral(token.literal, type);
				if (r) {
					idx++;
					return std::move(r);
				}
			}
			return std::make_unique<F64AST>(std::stod(token.literal));
		case TOK_STRING:
			return std::make_unique<StringAST>(token.literal);
		case TOK_LPAREN: {
			auto expr = parseExpression(tokens, idx);
			if (!expr) return expr;

			if (tokens[idx].type != TOK_RPAREN) {
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

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<BaseASTNode> parseUnary(const std::vector<Token>& tokens, int& idx) {
	const Token& token = tokens[idx];
	if (token.type == TOK_MINUS) {
		++idx;
		auto expr = parsePrimary(tokens, idx);
		if (!expr) return expr;
		return std::make_unique<UnaryOpAST>(std::move(expr), token.type);
	}

	return parsePrimary(tokens, idx);
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<BaseASTNode> parseBinaryOp(const std::vector<Token>& tokens, int& idx,
												  std::unique_ptr<BaseASTNode> left, int precedence) {
	while (true) {
		if (tokens[idx].type == TOK_AS) {
			++idx;
			if (tokens[idx].type != TOK_IDENTIFIER) {
				compilationError(tokens[idx].line, "Expected type name, got: " + tokens[idx].literal);
				return nullptr;
			}
			auto type = tokens[idx].literal;
			++idx;

			left = std::make_unique<AsAST>(std::move(left), type);
			continue;
		}

		auto binOpPrecedence = getTokenPrecedence(tokens[idx].type);

		if (binOpPrecedence < precedence) {
			return left;
		}

		const Token& token = tokens[idx];
		++idx;
		auto right = parseUnary(tokens, idx);
		if (!right) return right;

		auto nextPrecedence = getTokenPrecedence(tokens[idx].type);

		if (binOpPrecedence < nextPrecedence) {
			right = parseBinaryOp(tokens, idx, std::move(right), binOpPrecedence + 1);
			if (!right) return right;
		}

		left = std::make_unique<BinaryExprAST>(std::move(left), std::move(right), token.type);
	}
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<BaseASTNode> parseExpression(const std::vector<Token>& tokens, int& idx) {
	auto lhs = parseUnary(tokens, idx);
	if (!lhs) return nullptr;

	return parseBinaryOp(tokens, idx, std::move(lhs), 0);
}

static std::unique_ptr<PrototypeAST> parsePrototype(const std::vector<Token>& tokens, int& idx) {
	std::string name = tokens[idx].literal;
	idx++;
	if (tokens[idx].type != TOK_LPAREN) {
		compilationError(tokens[idx].line, "Expected '('");
		return nullptr;
	}
	idx++;
	// Parse arguments
	std::vector<std::pair<std::string, std::string>> args;

	while (tokens[idx].type != TOK_RPAREN) {
		std::pair<std::string, std::string> nextPair;

		if (tokens[idx].type != TOK_IDENTIFIER) {
			compilationError(tokens[idx].line, "Expected identifier, got " + tokens[idx].literal);
			return nullptr;
		}
		nextPair.first = tokens[idx].literal;
		++idx;

		if (tokens[idx].type != TOK_COLON) {
			compilationError(tokens[idx].line, "Expected \":\" , got " + tokens[idx].literal);
			return nullptr;
		}
		++idx;

		if (tokens[idx].type != TOK_IDENTIFIER) {
			compilationError(tokens[idx].line, "Expected identifier, got " + tokens[idx].literal);
			return nullptr;
		}
		nextPair.second = tokens[idx].literal;
		++idx;

		args.push_back(nextPair);

		if (tokens[idx].type == TOK_COMMA) ++idx;
	}

	++idx;

	if (tokens[idx].type != TOK_OUTPUT_TYPE) {
		compilationError(tokens[idx].line, "Expected \"->\" , got " + tokens[idx].literal);
		return nullptr;
	}
	++idx;

	if (tokens[idx].type != TOK_IDENTIFIER) {
		compilationError(tokens[idx].line, "Expected return type , got " + tokens[idx].literal);
		return nullptr;
	}
	std::string returnType = tokens[idx].literal;
	++idx;

	return std::make_unique<PrototypeAST>(name, returnType, args);
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<BaseASTNode> parseStatement(const std::vector<Token>& tokens, int& idx) {
	if (tokens[idx].type == TOK_IF) {
		return parseCondition(tokens, idx);
	} else if (tokens[idx].type == TOK_WHILE) {
		return parseWhile(tokens, idx);
	} else if (tokens[idx].type == TOK_RETURN) {
		idx++;
		return parseReturn(tokens, idx);
	} else if (tokens[idx].type == TOK_LET) {
		idx++;
		return parseLet(tokens, idx);
	} else {
		auto expr = parseExpression(tokens, idx);
		if (!expr) {
			compilationError(tokens[idx].line, "Expected \"}\" or expression, got " + tokens[idx].literal);
			return nullptr;
		}
		return expr;
	}
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<ScopeAST> parseScope(const std::vector<Token>& tokens, int& idx) {
	std::vector<std::unique_ptr<BaseASTNode>> expressions;

	if (tokens[idx].type != TOK_LBRACE) {
		compilationError(tokens[idx].line, "Expected \"{\" , got " + tokens[idx].literal);
		return nullptr;
	}

	++idx;

	bool shouldPush = true;

	while (tokens[idx].type != TOK_RBRACE) {
		if (tokens[idx].type == TOK_RETURN) {
			++idx;
			auto expr = parseReturn(tokens, idx);
			if (shouldPush) expressions.push_back(std::move(expr));
			shouldPush = false;
		} else {
			auto expr = parseStatement(tokens, idx);
			if (shouldPush) expressions.push_back(std::move(expr));
		}
	}

	++idx;

	return std::make_unique<ScopeAST>(std::move(expressions));
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<WhileAST> parseWhile(const std::vector<Token>& tokens, int& idx) {
	++idx;

	bool bracketedCondition = tokens[idx].type == TOK_LPAREN;

	if (bracketedCondition) {
		++idx;
	}

	std::unique_ptr<BaseASTNode> condition = parseExpression(tokens, idx);
	if (bracketedCondition) {
		if (tokens[idx].type != TOK_RPAREN) {
			compilationError(tokens[idx].line, "Expected \"(\", got: " + tokens[idx].literal);
			return nullptr;
		}
		++idx;
	}

	bool bracketedTrueBranch = tokens[idx].type == TOK_LBRACE;

	std::unique_ptr<ScopeAST> whileBody;

	if (bracketedTrueBranch) {
		whileBody = parseScope(tokens, idx);
	} else {
		auto expr = parseStatement(tokens, idx);
		if (!expr) {
			compilationError(tokens[idx].line, "Couldn't parse true branch expression");
			return nullptr;
		}
		// TODO: This doesn't work for some reason with initializer-list ?
		std::vector<std::unique_ptr<BaseASTNode>> scope;
		scope.push_back(std::move(expr));
		whileBody = std::make_unique<ScopeAST>(std::move(scope));
	}

	return std::make_unique<WhileAST>(std::move(condition), std::move(whileBody));
}

static std::unique_ptr<FunctionAST> parseFunction(const std::vector<Token>& tokens, int& idx) {
	std::unique_ptr<PrototypeAST> prototype = parsePrototype(tokens, idx);

	if (!prototype) {
		return nullptr;
	}

	std::unique_ptr<ScopeAST> body = parseScope(tokens, idx);

	return std::make_unique<FunctionAST>(std::move(prototype), std::move(body));
}

// NOLINTNEXTLINE(misc-no-recursion)
static std::unique_ptr<IfAST> parseCondition(const std::vector<Token>& tokens, int& idx) {
	++idx;

	bool bracketedCondition = tokens[idx].type == TOK_LPAREN;

	if (bracketedCondition) {
		++idx;
	}

	std::unique_ptr<BaseASTNode> condition = parseExpression(tokens, idx);
	if (bracketedCondition) {
		if (tokens[idx].type != TOK_RPAREN) {
			compilationError(tokens[idx].line, "Expected \"(\", got: " + tokens[idx].literal);
			return nullptr;
		}
		++idx;
	}

	bool bracketedTrueBranch = tokens[idx].type == TOK_LBRACE;

	std::unique_ptr<ScopeAST> trueBranchScope;

	if (bracketedTrueBranch) {
		trueBranchScope = parseScope(tokens, idx);
	} else {
		auto expr = parseStatement(tokens, idx);
		if (!expr) {
			compilationError(tokens[idx].line, "Couldn't parse true branch expression");
			return nullptr;
		}
		// TODO: This doesn't work for some reason with initializer-list ?
		std::vector<std::unique_ptr<BaseASTNode>> scope;
		scope.push_back(std::move(expr));
		trueBranchScope = std::make_unique<ScopeAST>(std::move(scope));
	}

	std::unique_ptr<ScopeAST> falseBranchScope;

	if (tokens[idx].type == TOK_ELSE) {
		++idx;
		bool bracketedFalseBranch = tokens[idx].type == TOK_LBRACE;

		if (bracketedFalseBranch) {
			falseBranchScope = parseScope(tokens, idx);
		} else {
			auto expr = parseStatement(tokens, idx);
			if (!expr) {
				return nullptr;
			}
			// TODO: This doesn't work for some reason with initializer-list ?
			std::vector<std::unique_ptr<BaseASTNode>> scope;
			scope.push_back(std::move(expr));
			falseBranchScope = std::make_unique<ScopeAST>(std::move(scope));
		}
	}

	return std::make_unique<IfAST>(std::move(condition), std::move(trueBranchScope), std::move(falseBranchScope));
}

static std::unique_ptr<ReturnAST> parseReturn(const std::vector<Token>& tokens, int& idx) {
	std::unique_ptr<BaseASTNode> expression = parseExpression(tokens, idx);

	if (!expression) {
		compilationError(tokens[idx].line, "Couldn't parse return statement");
		return nullptr;
	}

	return std::make_unique<ReturnAST>(std::move(expression));
}

static std::unique_ptr<LetAST> parseLet(const std::vector<Token>& tokens, int& idx) {
	if (tokens[idx].type != TOK_IDENTIFIER) {
		compilationError(tokens[idx].line, "Expected identifier, got " + tokens[idx].literal);
		return nullptr;
	}
	std::string name = tokens[idx].literal;
	++idx;

	bool typeSpecified = false;

	if (tokens[idx].type == TOK_COLON) {
		typeSpecified = true;
	}

	std::string type;
	if (typeSpecified) {
		++idx;
		if (tokens[idx].type != TOK_IDENTIFIER) {
			compilationError(tokens[idx].line, "Expected type identifier, got " + tokens[idx].literal);
			return nullptr;
		}
		type = tokens[idx].literal;
		++idx;
	}

	bool initializerSpecified = tokens[idx].type == TOK_ASSIGN;
	std::unique_ptr<BaseASTNode> initializer;

	if (initializerSpecified) {
		++idx;
		initializer = parseExpression(tokens, idx);
	}

	return std::make_unique<LetAST>(name, type, std::move(initializer));
}

static std::unique_ptr<ModuleAST> parseFile(const std::vector<Token>& tokens, std::unique_ptr<llvm::Module> module) {
	std::vector<std::unique_ptr<BaseASTNode>> result;
	int idx = 0;
	while (idx < tokens.size()) {
		if (tokens[idx].type == TOK_FN) {
			idx++;
			auto funcResult = parseFunction(tokens, idx);
			if (!funcResult) {
				continue;
			}
			result.push_back(std::move(funcResult));
		} else if (tokens[idx].type == TOK_EOF) {
			break;
		} else {
			compilationError(tokens[idx].line, "Unexpected token " + tokens[idx].literal);
			++idx;
		}
	}

	return std::make_unique<ModuleAST>(std::move(result), std::move(module));
}

#endif	// EMMC_PARSER_H
