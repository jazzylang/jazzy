/*
 * MIT License
 *
 * Copyright (c) 2023 Dylan Tuttle
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

use crate::infrastructure::error::{ErrorMessage, ErrorReporter, ErrorType};
use crate::infrastructure::log::Logger;
use crate::parser::parser_data::{
    ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode, RootNode,
    UnaryExprNode, UnaryOperator, AST,
};
use crate::scanner::scanner_data::{Token, TokenType};

use super::parser_data::{NodePointer, NotANode};

pub struct Parser<'parse> {
    pub tokens: Vec<Token>,
    pub ast: &'parse mut AST,
    pub logger: &'parse mut Logger,
    pub error: &'parse mut ErrorReporter,

    current: usize,
}

impl Parser<'_> {
    pub fn new<'parse>(
        tokens: Vec<Token>,
        ast: &'parse mut AST,
        logger: &'parse mut Logger,
        error: &'parse mut ErrorReporter,
    ) -> Parser<'parse> {
        return Parser {
            tokens,
            ast,
            logger,
            error,
            current: 0,
        };
    }

    pub fn parse(&mut self) -> NodePointer {
        self.logger.log("");
        self.logger.log("-------------");
        self.logger.log("BEGIN Parsing");
        self.logger.log("-------------");

        // Begin parsing
        let expression = self.expression();

        // Add a root node and return its pointer
        let root_node = self
            .ast
            .new_node(ASTNode::RootNode(RootNode::new(expression)));

        self.logger.log("\nAST after parsing:");
        self.logger.log(&format!(
            "{}",
            self.ast.get_node(root_node).to_string(&self.ast)
        ));

        return root_node;
    }

    /*
     * expression -> equality ;
     * equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
     * comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
     * term       -> factor ( ( "-" | "+" ) factor )* ;
     * factor     -> unary ( ( "/" | "*" ) unary )* ;
     * unary      -> ( "!" | "-" ) unary | primary ;
     * primary    -> NUM | STR | "true" | "false" | "(" expression ")" ;
     */

    pub fn expression(&mut self) -> NodePointer {
        self.logger.log("Parser::expression()");
        return self.equality();
    }

    pub fn equality(&mut self) -> NodePointer {
        self.logger.log("Parser::equality()");

        // Get the expression on the left of the operator
        self.logger
            .log("Parser::equality(): Parsing left expression:");
        let mut left = self.comparison();

        while !self.is_at_end() && self.match_token(&[TokenType::NotEqual, TokenType::Equal]) {
            // We found an equality token, so consume it
            let operator = self.consume();

            // Warn the user if they don't have whitespace padding the operator
            self.warn_padding_around_operator(&operator);

            match operator.lexeme.clone() {
                None => self.logger.log("Parser::equality(): Found equality token"),
                Some(lexeme) => self.logger.log(&format!(
                    "Parser::equality(): Found equality token \"{}\"",
                    lexeme,
                )),
            }

            // Get the expression on the right of the operator
            self.logger
                .log("Parser::equality(): Parsing right expression:");
            let right = self.comparison();

            let binary_node = match operator.token_type {
                TokenType::NotEqual => {
                    BinaryExprNode::new(BinaryOperator::NotEqual, operator.clone(), left, right)
                }
                TokenType::Equal => {
                    BinaryExprNode::new(BinaryOperator::Equal, operator.clone(), left, right)
                }
                _type => panic!("Expected equality token, but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::equality(): Parsed node:\n{}",
                binary_node.to_string(&self.ast)
            ));

            left = self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(binary_node)));
        }

        return left;
    }

    pub fn comparison(&mut self) -> NodePointer {
        self.logger.log("Parser::comparison()");

        // Get the expression on the left of the operator
        self.logger
            .log("Parser::comparison(): Parsing left expression:");
        let mut left = self.term();

        while !self.is_at_end()
            && self.match_token(&[
                TokenType::GreaterThan,
                TokenType::GreaterThanOrEqual,
                TokenType::LessThan,
                TokenType::LessThanOrEqual,
            ])
        {
            // We found a comparison token, so consume it
            let operator = self.consume();

            // Warn the user if they don't have whitespace padding the operator
            self.warn_padding_around_operator(&operator);

            match operator.lexeme.clone() {
                None => self
                    .logger
                    .log("Parser::comparison(): Found comparison token"),
                Some(lexeme) => self.logger.log(&format!(
                    "Parser::comparison(): Found comparison token \"{}\"",
                    lexeme,
                )),
            }

            // Get the expression on the right of the operator
            self.logger
                .log("Parser::comparison(): Parsing right expression:");
            let right = self.term();

            let binary_node = match operator.token_type {
                TokenType::GreaterThan => {
                    BinaryExprNode::new(BinaryOperator::GreaterThan, operator.clone(), left, right)
                }
                TokenType::GreaterThanOrEqual => BinaryExprNode::new(
                    BinaryOperator::GreaterThanOrEqual,
                    operator.clone(),
                    left,
                    right,
                ),
                TokenType::LessThan => {
                    BinaryExprNode::new(BinaryOperator::LessThan, operator.clone(), left, right)
                }
                TokenType::LessThanOrEqual => BinaryExprNode::new(
                    BinaryOperator::LessThanOrEqual,
                    operator.clone(),
                    left,
                    right,
                ),
                _type => panic!("Expected comparison token but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::comparison(): Parsed node:\n{}",
                binary_node.to_string(&self.ast)
            ));

            left = self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(binary_node)));
        }

        return left;
    }

    pub fn term(&mut self) -> NodePointer {
        self.logger.log("Parser::term()");

        self.logger.log("Parser::term(): Parsing left expression:");
        // Get the expression on the left of the operator
        let mut left = self.factor();

        while !self.is_at_end() && self.match_token(&[TokenType::Plus, TokenType::Minus]) {
            // We found a term token, so consume it
            let operator = self.consume();

            // Warn the user if they don't have whitespace padding the operator
            self.warn_padding_around_operator(&operator);

            match operator.lexeme.clone() {
                None => self.logger.log("Parser::term(): Found term token"),
                Some(lexeme) => self
                    .logger
                    .log(&format!("Parser::term(): Found term token \"{}\"", lexeme,)),
            }

            self.logger.log("Parser::term(): Parsing right expression:");
            // Get the expression on the right of the operator
            let right = self.factor();

            let binary_node = match operator.token_type {
                TokenType::Plus => {
                    BinaryExprNode::new(BinaryOperator::Plus, operator.clone(), left, right)
                }
                TokenType::Minus => {
                    BinaryExprNode::new(BinaryOperator::Minus, operator.clone(), left, right)
                }
                _type => panic!("Expected term token, but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::term(): Parsed node:\n{}",
                binary_node.to_string(&self.ast)
            ));

            left = self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(binary_node)));
        }

        return left;
    }

    pub fn factor(&mut self) -> NodePointer {
        self.logger.log("Parser::factor()");

        self.logger
            .log("Parser::factor(): Parsing left expression:");
        // Get the expression on the left of the operator
        let mut left = self.boolean();

        while !self.is_at_end()
            && self.match_token(&[TokenType::Star, TokenType::Slash, TokenType::Modulo])
        {
            // We found a factor token, so consume it
            let operator = self.consume();

            // Warn the user if they don't have whitespace padding the operator
            self.warn_padding_around_operator(&operator);

            match operator.lexeme.clone() {
                None => self.logger.log("Parser::factor(): Found factor token"),
                Some(lexeme) => self.logger.log(&format!(
                    "Parser::factor(): Found factor token \"{}\"",
                    lexeme,
                )),
            }

            self.logger
                .log("Parser::factor(): Parsing right expression:");
            // Get the expression on the right of the operator
            let right = self.boolean();

            let binary_node = match operator.token_type {
                TokenType::Star => {
                    BinaryExprNode::new(BinaryOperator::Times, operator.clone(), left, right)
                }
                TokenType::Slash => {
                    BinaryExprNode::new(BinaryOperator::Divide, operator.clone(), left, right)
                }
                TokenType::Modulo => {
                    BinaryExprNode::new(BinaryOperator::Modulus, operator.clone(), left, right)
                }
                _type => panic!("Expected factor token but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::factor(): Parsed node:\n{}",
                binary_node.to_string(&self.ast)
            ));

            left = self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(binary_node)));
        }

        return left;
    }

    pub fn boolean(&mut self) -> NodePointer {
        self.logger.log("Parser::boolean()");

        self.logger
            .log("Parser::boolean(): Parsing left expression:");
        // Get the expression on the left of the operator
        let mut left = self.unary();

        while !self.is_at_end() && self.match_token(&[TokenType::And, TokenType::Or]) {
            // We found a factor token, so consume it
            let operator = self.consume();

            // Warn the user if they don't have whitespace padding the operator
            self.warn_padding_around_operator(&operator);

            match operator.lexeme.clone() {
                None => self.logger.log("Parser::boolean(): Found boolean token"),
                Some(lexeme) => self.logger.log(&format!(
                    "Parser::boolean(): Found boolean token \"{}\"",
                    lexeme,
                )),
            }

            self.logger
                .log("Parser::boolean(): Parsing right expression:");
            // Get the expression on the right of the operator
            let right = self.unary();

            let binary_node = match operator.token_type {
                TokenType::And => {
                    BinaryExprNode::new(BinaryOperator::And, operator.clone(), left, right)
                }
                TokenType::Or => {
                    BinaryExprNode::new(BinaryOperator::Or, operator.clone(), left, right)
                }
                _type => panic!("Expected boolean token but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::boolean(): Parsed node:\n{}",
                binary_node.to_string(&self.ast)
            ));

            left = self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(binary_node)));
        }

        return left;
    }

    pub fn unary(&mut self) -> NodePointer {
        self.logger.log("Parser::unary()");

        if !self.is_at_end() && self.match_token(&[TokenType::Minus, TokenType::Not]) {
            // We found a unary token, so consume it
            let operator = self.consume();

            match operator.lexeme.clone() {
                None => self.logger.log("Parser::unary(): Found unary token"),
                Some(lexeme) => self.logger.log(&format!(
                    "Parser::unary(): Found unary token \"{}\"",
                    lexeme,
                )),
            }

            self.logger
                .log("Parser::unary(): Parsing right expression:");
            // Get the expression on the right of the operator
            let operand = self.unary();

            let unary_node = match operator.token_type {
                TokenType::Minus => {
                    UnaryExprNode::new(UnaryOperator::Minus, operator.clone(), operand)
                }
                TokenType::Not => UnaryExprNode::new(UnaryOperator::Not, operator.clone(), operand),
                _type => panic!("Expected unary token, but got {:?}", _type),
            };

            self.logger.log(&format!(
                "Parser::unary(): Parsed node:\n{}",
                unary_node.to_string(&self.ast)
            ));
            return self
                .ast
                .new_node(ASTNode::ExpressionNode(ExpressionNode::Unary(unary_node)));
        }

        // Otherwise, we didn't find a unary operator token,
        // so we must have reached the highest level of precedence
        return self.primary();
    }

    pub fn primary(&mut self) -> NodePointer {
        self.logger.log("Parser::primary()");

        let current_token = self.current_token();
        match &current_token.lexeme {
            None => {
                let error_type = ErrorType::ExpectedExpressionError;
                self.error.report(
                    error_type.clone(),
                    vec![ErrorMessage::new(
                        Some(format!(
                            "{} but got {}",
                            error_type.description(),
                            match &current_token.lexeme {
                                None => format!("{:?}", current_token.token_type),
                                Some(lexeme) => format!("\"{}\"", lexeme),
                            }
                        )),
                        &current_token.location_info,
                    )],
                    None,
                );

                return self.ast.new_node(ASTNode::NotANode(NotANode::new()));
            }
            Some(lexeme) => {
                let literal_node = match current_token.token_type {
                    TokenType::False => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(Literal::False(false), current_token.clone()),
                                )));

                        pointer
                    }
                    TokenType::True => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(Literal::True(true), current_token.clone()),
                                )));

                        pointer
                    }
                    TokenType::IntLit => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(
                                        Literal::Int(lexeme.parse::<i128>().unwrap()),
                                        current_token.clone(),
                                    ),
                                )));

                        pointer
                    }
                    TokenType::FloatLit => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(
                                        Literal::Float(lexeme.parse::<f64>().unwrap()),
                                        current_token.clone(),
                                    ),
                                )));

                        pointer
                    }
                    TokenType::HexLit => {
                        let without_prefix = lexeme.trim_start_matches("0x");
                        let hex_val = i128::from_str_radix(without_prefix, 16).unwrap();
                        self.logger.log(&format!("0xless: {:?}", hex_val));
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(Literal::Int(hex_val), current_token.clone()),
                                )));

                        pointer
                    }
                    TokenType::BinaryLit => {
                        let without_prefix = lexeme.trim_start_matches("0b");
                        let binary_val = i128::from_str_radix(without_prefix, 2).unwrap();
                        println!("{:?}", binary_val);
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(
                                        Literal::Int(binary_val),
                                        current_token.clone(),
                                    ),
                                )));

                        pointer
                    }
                    TokenType::StringLit => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
                                    LiteralNode::new(
                                        Literal::String(lexeme.clone()),
                                        current_token.clone(),
                                    ),
                                )));

                        pointer
                    }
                    TokenType::LeftParen => {
                        // Consume the left parenthesis
                        self.consume();

                        let left_paren = current_token;

                        // Parse the expression
                        self.logger
                            .log("Parser::primary(): Parsing parenthesized expression:");

                        let expression = self.expression();

                        let next_token = self.current_token();
                        if self.is_at_end() || next_token.token_type != TokenType::RightParen {
                            // If we don't find a right parenthesis, we have a syntax error

                            let error_type = ErrorType::UnMatchedParenError;
                            match next_token.lexeme {
                                None => self.error.report(
                                    error_type,
                                    vec![
                                        ErrorMessage::new(
                                            Some(format!(
                                                "Expected token \")\" after expression but got token {:?}",
                                                next_token.token_type
                                            )),
                                            &next_token.location_info
                                        ),
                                        ErrorMessage::new(
                                            Some(String::from("Un-matched left parenthesis")),
                                            &left_paren.location_info.clone(),
                                        ),
                                    ],
                                    Some(format!("Try putting a \")\" after the expression")),
                                ),
                                Some(lexeme) => self.error.report(
                                    error_type,
                                    vec![
                                        ErrorMessage::new(
                                            Some(format!(
                                                "Expected token \")\" after expression but got token \"{}\"",
                                                lexeme
                                            )),
                                            &next_token.location_info
                                        ),
                                        ErrorMessage::new(
                                            Some(String::from("Un-matched left parenthesis")),
                                            &left_paren.location_info.clone(),
                                        ),
                                    ],
                                    Some(format!("Try putting a \")\" after the expression")),
                                ),
                            }
                        } else if !self.is_at_end() {
                            self.consume();
                        }

                        self.logger.log(&format!(
                            "Parser::primary(): Parsed node:\n{}",
                            self.ast.get_node(expression).to_string(&self.ast)
                        ));

                        expression
                    }
                    _ => {
                        let error_type = ErrorType::ExpectedExpressionError;
                        self.error.report(
                            error_type.clone(),
                            vec![ErrorMessage::new(
                                Some(format!(
                                    "{} but got {}",
                                    error_type.description(),
                                    match &current_token.lexeme {
                                        None => format!("{:?}", current_token.token_type),
                                        Some(lexeme) => format!("\"{}\"", lexeme),
                                    }
                                )),
                                &current_token.location_info,
                            )],
                            None,
                        );

                        return self.ast.new_node(ASTNode::NotANode(NotANode::new()));
                    }
                };

                return literal_node;
            }
        }
    }

    pub fn warn_padding_around_operator(&mut self, operator: &Token) {
        match &operator.lexeme {
            None => {}
            Some(lexeme) => {
                // If the operator has whitespace preceding but not following it
                if !operator.whitespace_precedes && operator.whitespace_follows {
                    self.error.report(
                        ErrorType::MissingPaddingAroundOperatorError,
                        vec![
                            ErrorMessage::new(
                                Some(String::from("in jazzy, you have to surround binary operators with whitespace")),
                                &operator.location_info,
                            )
                        ],
                        Some(format!("Consider placing a space before the operator like so: \"left {} right\"", lexeme)),
                    );
                } else if operator.whitespace_precedes && !operator.whitespace_follows {
                    self.error.report(
                        ErrorType::MissingPaddingAroundOperatorError,
                        vec![
                            ErrorMessage::new(
                                Some(String::from("in jazzy, you have to surround binary operators with whitespace")),
                                &operator.location_info,
                            )
                        ],
                        Some(format!("Consider placing a space after the operator like so: \"left {} right\"", lexeme)),
                    );
                } else if !operator.whitespace_precedes && !operator.whitespace_follows {
                    self.error.report(
                        ErrorType::MissingPaddingAroundOperatorError,
                        vec![
                            ErrorMessage::new(
                                Some(String::from("in jazzy, you have to surround binary operators with whitespace")),
                                &operator.location_info,
                            ),
                        ],
                        Some(format!("Consider placing a space before and after the operator like so: \"left {} right\"", lexeme)),
                    );
                }
            }
        }
    }

    fn match_token(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type.clone()) {
                return true;
            }
        }

        return false;
    }

    fn check(&mut self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }

        return self.current_token().token_type == token_type;
    }

    fn is_at_end(&mut self) -> bool {
        return self.tokens[self.current].token_type == TokenType::EOF;
    }

    fn current_token(&mut self) -> Token {
        return self.tokens[self.current].clone();
    }

    fn consume(&mut self) -> Token {
        let token = self.current_token();
        self.current += 1;
        return token;
    }
}
