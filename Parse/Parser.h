//
// Created by fail on 8/14/22.
//

#ifndef EMMC_PARSER_H
#define EMMC_PARSER_H

#include "../AST/AST.h"
#include "../Lex/Lexer.h"

class Parser {
	Lexer* lexer = nullptr;
	std::unique_ptr<llvm::Module> module = nullptr;

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<IfAST> parseIf() {
		bool bracketed = lexer->peekLeftParen();
		if (bracketed) {
			lexer->consumeLeftParen();
		}

		auto condition = parseExpression();
		if (bracketed) {
			if (!lexer->consumeRightParen()) {
				return nullptr;
			}
		}

		bool bracketedTrueBranch = lexer->peekLeftBrace();

		if (!bracketed && !bracketedTrueBranch) {
			compilationError("Can't omit both brackets and braces in the if statement");
			return nullptr;
		}

		std::unique_ptr<ScopeAST> trueScope;

		if (bracketedTrueBranch) {
			trueScope = parseScope();
		} else {
			auto expr = parseStatement();
			if (!expr) {
				return nullptr;
			}

			std::vector<std::unique_ptr<BaseASTNode>> scope;
			scope.push_back(std::move(expr));
			trueScope = std::make_unique<ScopeAST>(std::move(scope));
		}

		auto elsePeek = lexer->peekKeyword();
		std::unique_ptr<ScopeAST> falseBranchScope;

		if (elsePeek.getType() == TokenType::KW_ELSE) {
			lexer->consumeKeyword();

			bool bracketedFalseBranch = lexer->peekLeftBrace();

			if (bracketedFalseBranch) {
				falseBranchScope = parseScope();
			} else {
				auto expr = parseStatement();
				if (!expr) {
					return nullptr;
				}

				std::vector<std::unique_ptr<BaseASTNode>> scope;
				scope.push_back(std::move(expr));
				falseBranchScope = std::make_unique<ScopeAST>(std::move(scope));
			}
		}

		return std::make_unique<IfAST>(std::move(condition), std::move(trueScope), std::move(falseBranchScope));
	}

	// TODO: This will need to be completely redone
	//  NOLINTNEXTLINE(misc-no-recursion)
	std::string parseType() {
		std::string output;
		auto typeName = lexer->consumeIdentifier();
		if (!typeName) {
			return output;
		}

		output = typeName.getValue().str();

		if (lexer->peekOperator().getType() == TokenType::LESS) {
			output += " < ";

			lexer->consumeOperator();

			output += parseType();

			while (lexer->peekOperator().getType() == TokenType::COMMA) {
				lexer->consumeOperator();
				output += parseType();
			}

			if (lexer->peekOperator().getType() != TokenType::GREATER) {
				compilationError(lexer, "Expected '>'");
				return output;
			}

			lexer->consumeOperator();

			output += " > ";
		}
		return output;
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::vector<std::unique_ptr<BaseASTNode>> parseArgList() {
		std::vector<std::unique_ptr<BaseASTNode>> args;
		if (lexer->peekRightParen()) {
			lexer->consumeRightParen();
			return args;
		}

		auto first = parseExpression();
		args.push_back(std::move(first));

		while (lexer->peekComma()) {
			lexer->consumeComma();
			auto next = parseExpression();
			args.push_back(std::move(next));
		}

		if (lexer->consumeRightParen()) {
			return args;
		} else {
			compilationError(lexer, "Expected ')'");
		}

		return args;
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<BaseASTNode> parseUnary() {
		auto op = lexer->peekOperator();
		// Unary minus, unary plus, and dereference operator are the only unary operators currently supported
		if (op.getType() == TokenType::MINUS || op.getType() == TokenType::PLUS ||
			op.getType() == TokenType::TokenType::PRODUCT || op.getType() == TokenType::TokenType::BITWISE_AND) {
			auto r = lexer->consumeOperator();
			if (!r) {
				return nullptr;
			}

			auto expr = parsePrimary();
			return std::make_unique<UnaryOpAST>(std::move(expr), op.getType());
		}

		return parsePrimary();
	}

    std::unique_ptr<StructInitializerAST> parseStructInitializer(const std::string& structName) {
        if (!lexer->consumeLeftBrace()) {
            return nullptr;
        }
        std::vector<std::pair<std::string, std::unique_ptr<BaseASTNode>>> members;

        while (!lexer->peekRightBrace()) {
            auto name = lexer->consumeIdentifier();
            if (!name) {
                compilationError(lexer, "Expected identifier");
                return nullptr;
            }

            auto colon = lexer->consumeColon();
            if (!colon) {
                return nullptr;
            }

            auto expr = parseExpression();
            if (!expr) {
                return nullptr;
            }
            members.push_back(std::make_pair<>(name.getValue().str(), std::move(expr)));
            if (lexer->peekComma()) {
                lexer->consumeComma();
            }
        }

        if (!lexer->consumeRightBrace()) {
            return nullptr;
        }

        return std::make_unique<StructInitializerAST>(structName, std::move(members));
    }

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<BaseASTNode> parsePrimary() {
		auto tryPeekIdentifier = lexer->peekIdentifier();
		if (tryPeekIdentifier.getType() == TokenType::IDENTIFIER) {
			auto id = lexer->consumeIdentifier();
            if(lexer->peekLeftBrace()) {
                return parseStructInitializer(id.getValue().str());
            } else if (lexer->peekLeftParen()) {
				lexer->consumeLeftParen();
				auto args = parseArgList();

				return std::make_unique<CallExprAST>(id.getValue().str(), std::move(args));
			} else if(lexer->peekOperator().getType() == TokenType::DOUBLE_COLON) {
                lexer->consumeOperator();
                auto funcName = lexer->consumeIdentifier();
                lexer->consumeLeftParen();

                auto args = parseArgList();

                return std::make_unique<CallExprAST>(mangling_combine(funcName.getValue().str(), id.getValue().str()), std::move(args));
            } else {
				return std::make_unique<VariableAST>(id.getValue().str(), "");
			}
		}

		auto tryPeekNumber = lexer->peekNumber();
		if (tryPeekNumber.getType() != TokenType::NONE) {
			auto num = lexer->consumeNumber();

			if (num.getType() == TokenType::INTEGER) {
				if (num.getLiteral().empty()) {
					return std::make_unique<I64AST>(std::stoll(num.getValue().str()));
				} else {
					return fromLiteral(num.getValue().str(), num.getLiteral().str());
				}
			}

			if (num.getType() == TokenType::FLOATING_POINT) {
				if (num.getLiteral().empty()) {
					return std::make_unique<F64AST>(std::stod(num.getValue().str()));
				} else {
					return fromLiteral(num.getValue().str(), num.getLiteral().str());
				}
			}

			compilationError("Unreachable code reached. This is a compiler bug.");
		}

		auto lparen = lexer->peekLeftParen();
		if (lparen) {
			lexer->consumeLeftParen();

			auto expr = parseExpression();

			if (!lexer->consumeRightParen()) {
				return nullptr;
			}

			return std::move(expr);
		}
		compilationError(lexer, "Unexpected token");

		return nullptr;
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<BaseASTNode> parseBinaryOp(std::unique_ptr<BaseASTNode> left, std::int64_t precedence) {
		while (true) {
			if (lexer->peekLeftBracket()) {
				if (!lexer->consumeLeftBracket()) {
					return nullptr;
				}

				auto expr = parseExpression();
				if (!expr) {
					return nullptr;
				}

				if (!lexer->consumeRightBracket()) {
					return nullptr;
				}

				left = std::make_unique<IndexAST>(std::move(left), std::move(expr));
				continue;
			}

			auto peekOp = lexer->peekOperator();

			if (peekOp.getType() == TokenType::KW_AS) {
				if (!lexer->consumeOperator()) {
					return nullptr;
				}

				auto type = parseType();
				if (type.empty()) {
					return nullptr;
				}

				left = std::make_unique<AsAST>(std::move(left), type);
				continue;
			}

			if (peekOp.getType() == TokenType::DOT) {
				if (!lexer->consumeOperator()) {
					return nullptr;
				}

				auto member = lexer->consumeIdentifier();
				if (!member) {
					return nullptr;
				}

				if (!lexer->peekLeftParen()) {
					left = std::make_unique<MemberAST>(member.getValue().str(), std::move(left));
				} else {
					lexer->consumeLeftParen();
					auto args = parseArgList();
					args.insert(args.begin(), std::move(left));
					if(member.getValue().str() == "deref") {
                        if(args.size() != 1) {
                            compilationError(lexer, "deref must not have any arguments");
                        }
                        left = std::make_unique<DerefAST>(std::move(args[0]));
                    } else {
                        left = std::make_unique<StructCallExprAST>(member.getValue().str(), std::move(args));
                    }
				}

				continue;
			}

			auto binOpPrecedence = getOperatorPrecedence(peekOp.getType());
			if (binOpPrecedence < precedence) {
				return left;
			}
			auto token = lexer->consumeOperator();
			if (!token) {
				return nullptr;
			}
			auto right = parseUnary();
			if (!right) {
				return nullptr;
			}

			auto nextPrecedence = getOperatorPrecedence(lexer->peekOperator().getType());

			if (binOpPrecedence < nextPrecedence) {
				// TODO: Verify that this is right
				right = parseBinaryOp(std::move(right), binOpPrecedence + 1);
				if (!right) {
					return nullptr;
				}
			}

			left = std::make_unique<BinaryExprAST>(std::move(left), std::move(right), token.getType());
		}
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<BaseASTNode> parseExpression() {
		auto lhs = parseUnary();
		if (!lhs) {
			return nullptr;
		}

		return parseBinaryOp(std::move(lhs), 0);
	}

	std::unique_ptr<LetAST> parseLet() {
		auto name = lexer->consumeIdentifier();
		if (!name) {
			return nullptr;
		}

		bool typeSpecified = false;

		if (lexer->peekColon()) {
			if (!lexer->consumeColon()) {
				return nullptr;
			}

			typeSpecified = true;
		}

		std::string type;

		if (typeSpecified) {
			type = parseType();
		}

		bool initializerSpecified = false;
		if (lexer->peekOperator().getType() == TokenType::ASSIGN) {
			if (!lexer->consumeOperator()) {
				return nullptr;
			}

			initializerSpecified = true;
		}

		std::unique_ptr<BaseASTNode> initializer;

		if (initializerSpecified) {
			initializer = parseExpression();
		}

		return std::make_unique<LetAST>(name.getValue().str(), type, std::move(initializer));
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<BaseASTNode> parseStatement() {
		auto peekKeyword = lexer->peekKeyword();

		switch (peekKeyword.getType()) {
			case TokenType::NONE: {
				auto expr = parseExpression();
				if (!expr) {
					return nullptr;
				}
				return expr;
			}
			case TokenType::KW_RETURN: {
				auto token = lexer->consumeKeyword();
				if (!token) {
					return nullptr;
				}
				auto expr = parseExpression();
				if (!expr) {
					return nullptr;
				}
				return std::make_unique<ReturnAST>(std::move(expr));
			}
			case TokenType::KW_WHILE: {
				auto token = lexer->consumeKeyword();
				if (!token) {
					return nullptr;
				}
				auto loop = parseWhile();
				if (!loop) {
					return nullptr;
				}
				return loop;
			}
			case TokenType::KW_IF: {
				auto token = lexer->consumeKeyword();
				if (!token) {
					return nullptr;
				}
				auto ifStmt = parseIf();
				if (!ifStmt) {
					return nullptr;
				}
				return ifStmt;
			}
			case TokenType::KW_LET: {
				auto token = lexer->consumeKeyword();
				if (!token) {
					return nullptr;
				}
				auto letStmt = parseLet();
				if (!letStmt) {
					return nullptr;
				}
				return letStmt;
			}
			default: {
				compilationError(lexer, "Unexpected token " + peekKeyword.getValue().str());
				return nullptr;
			}
		}
	}

	std::unique_ptr<ImplAST> parseImpl() {
		auto structName = lexer->consumeIdentifier();
		if (!structName) {
			return nullptr;
		}

		bool leftBraceFound = lexer->consumeLeftBrace();
		if (!leftBraceFound) {
			return nullptr;
		}

		std::vector<std::unique_ptr<BaseASTNode>> nodes;

		while (!lexer->peekRightBrace()) {
			auto fnKeyword = lexer->consumeKeyword();

			if (!fnKeyword) {
				return nullptr;
			}

            bool isStatic = false;

            if(fnKeyword.getType() == TokenType::KW_STATIC) {
                fnKeyword = lexer->consumeKeyword();

                isStatic = true;
            }

			if (fnKeyword.getType() != TokenType::TokenType::KW_FN) {
				compilationError(lexer, "Expected keyword 'fn'");
				return nullptr;
			}

			auto fn = parseFunction(structName.getValue().str(), isStatic);
			if (!fn) {
				return nullptr;
			}
			nodes.push_back(std::move(fn));
		}

		auto rightBraceFound = lexer->consumeRightBrace();
		if (!rightBraceFound) {
			return nullptr;
		}

		return std::make_unique<ImplAST>(std::move(nodes), structName.getValue().str());
	}

	std::unique_ptr<StructAST> parseStruct() {
		auto structName = lexer->consumeIdentifier();
		if (!structName) {
			return nullptr;
		}

		bool leftBraceFound = lexer->consumeLeftBrace();
		if (!leftBraceFound) {
			return nullptr;
		}

		std::vector<std::pair<std::string, std::string>> members;
		while (!lexer->peekRightBrace()) {
			auto identifier = lexer->consumeIdentifier();
			if (!identifier) {
				return nullptr;
			}

			auto colonFound = lexer->consumeColon();
			if (!colonFound) {
				return nullptr;
			}

			auto type = parseType();
			if (type.empty()) {
				return nullptr;
			}

			members.emplace_back(identifier.getValue().str(), type);

			if (lexer->peekComma()) {
				lexer->consumeComma();
			}
		}

		auto rightBraceFound = lexer->consumeRightBrace();
		if (!rightBraceFound) {
			return nullptr;
		}

		return std::make_unique<StructAST>(structName.getValue().str(), members);
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<ScopeAST> parseScope() {
		std::vector<std::unique_ptr<BaseASTNode>> statements;

		auto leftBraceFound = lexer->consumeLeftBrace();
		if (!leftBraceFound) {
			return nullptr;
		}

        bool shouldGenerate = true;

		while (!lexer->peekRightBrace()) {
			auto statement = parseStatement();
			if (!statement) {
				return nullptr;
			}
            if(statement->alwaysReturns()) {
                shouldGenerate = false;
            }

			if(shouldGenerate) statements.push_back(std::move(statement));
		}

		auto rightBraceFound = lexer->consumeRightBrace();
		if (!rightBraceFound) {
			return nullptr;
		}

		return std::make_unique<ScopeAST>(std::move(statements));
	}

	// NOLINTNEXTLINE(misc-no-recursion)
	std::unique_ptr<WhileAST> parseWhile() {
		bool parenthesized = lexer->peekLeftParen();

		if (parenthesized) {
			lexer->consumeLeftParen();
		}

		auto condition = parseExpression();
		if (!condition) {
			return nullptr;
		}

		if (parenthesized) {
			bool foundRightParent = lexer->consumeRightParen();
			if (!foundRightParent) {
				return nullptr;
			}
		}

		bool braced = lexer->peekLeftBrace();

		if (!braced && !parenthesized) {
			compilationError(lexer, "Can't omit both parenthesis and braces");
			return nullptr;
		}

		std::unique_ptr<ScopeAST> body;

		if (braced) {
			body = parseScope();
		} else {
			auto expr = parseStatement();
			if (!expr) {
				return nullptr;
			}

			// TODO: This doesn't work for some reason with initializer-list ?
			std::vector<std::unique_ptr<BaseASTNode>> scope;
			scope.push_back(std::move(expr));
			body = std::make_unique<ScopeAST>(std::move(scope));
		}

		return std::make_unique<WhileAST>(std::move(condition), std::move(body));
	}

	std::unique_ptr<PrototypeAST> parsePrototype(const std::string& structName = "", bool isStatic = false) {
		auto name = lexer->consumeIdentifier();
		if (!name) {
			return nullptr;
		}

        if(tryMatchKeyword(name.getValue().str()) != TokenType::NONE) {
            compilationError(lexer, "Expected identifier");
            return nullptr;
        }

		bool lparenFound = lexer->consumeLeftParen();
		if (!lparenFound) {
			return nullptr;
		}

		std::vector<std::pair<std::string, std::string>> args;

		if (!structName.empty() && !isStatic) {
			args.emplace_back("self", "Pointer < " + structName + " > ");
		}

		while (!lexer->peekRightParen()) {
			std::pair<std::string, std::string> arg;
			auto argName = lexer->consumeIdentifier();
			if (!argName) {
				return nullptr;
			}

			bool colonFound = lexer->consumeColon();
			if (!colonFound) {
				return nullptr;
			}

			arg.first = argName.getValue();

			arg.second = parseType();
			if (arg.second.empty()) {
				compilationError(lexer, "Expected type");
				return nullptr;
			}

			if (lexer->peekComma()) {
				lexer->consumeComma();
			}

			args.emplace_back(std::move(arg));
		}

		lexer->consumeRightParen();

		auto outputTypeSymbol = lexer->consumeOutputOperator();
		if (!outputTypeSymbol) {
			return nullptr;
		}

		std::string returnType = parseType();

		return std::make_unique<PrototypeAST>(!isStatic ? name.getValue().str() : mangling_combine(name.getValue().str(), structName), returnType, args, !structName.empty() && !isStatic);
	}

	std::unique_ptr<FunctionAST> parseFunction() {
		std::unique_ptr<PrototypeAST> proto = parsePrototype();
		if (!proto) {
			return nullptr;
		}

		std::unique_ptr<ScopeAST> scope = parseScope();

		return std::make_unique<FunctionAST>(std::move(proto), std::move(scope));
	}

	std::unique_ptr<FunctionAST> parseFunction(const std::string& structName, bool isStatic = false) {
		std::unique_ptr<PrototypeAST> proto = parsePrototype(structName, isStatic);
		if (!proto) {
			return nullptr;
		}

		std::unique_ptr<ScopeAST> scope = parseScope();

		return std::make_unique<FunctionAST>(std::move(proto), std::move(scope), structName, isStatic);
	}

   public:
	explicit Parser(Lexer* lexer, std::unique_ptr<llvm::Module> module) : lexer(lexer), module(std::move(module)) {
		if (!this->lexer) {
			compilationError("Couldn't create lexer. This is a compiler bug.");
		}

		if (!this->module) {
			compilationError("Couldn't create module. This is a compiler bug.");
		}
	}

	std::unique_ptr<ModuleAST> parse() {
		std::vector<std::unique_ptr<BaseASTNode>> nodes;

		while (!lexer->isEOF()) {
			auto keyword = lexer->consumeKeyword();

			switch (keyword.getType()) {
				case TokenType::KW_FN: {
					auto func = parseFunction();
					if (!func) {
						return nullptr;
					}
					nodes.push_back(std::move(func));
					break;
				}
				case TokenType::KW_STRUCT: {
					auto structResult = parseStruct();
					if (!structResult) {
						return nullptr;
					}
					nodes.push_back(std::move(structResult));
					break;
				}
				case TokenType::KW_EXTERN: {
					// extern is always immediately followed by fn
					auto fnKeyword = lexer->consumeKeyword();
					if (fnKeyword.getType() != TokenType::KW_FN) {
						compilationError(lexer, "Expected fn keyword after extern keyword");
						return nullptr;
					}
					auto fn = parsePrototype();
					if (!fn) {
						return nullptr;
					}
					nodes.push_back(std::move(fn));
					break;
				}
				case TokenType::KW_IMPL: {
					auto impl = parseImpl();
					if (!impl) {
						return nullptr;
					}
					nodes.push_back(std::move(impl));
					break;
				}
				default: {
					compilationError(lexer, "Unexpected keyword " + keyword.getValue().str());
					return nullptr;
				}
			}
		}

		return std::make_unique<ModuleAST>(std::move(nodes), std::move(module));
	}
};

#endif	// EMMC_PARSER_H
