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
    ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode, UnaryExprNode,
    UnaryOperator, AST,
};
use crate::scanner::scanner_data::{Token, TokenType};
use crate::semantic_checker::semantic_checker_data::{DataType, Primitive, Type};

use super::parser_data::{
    IdentifierStatementNode, NodePointer, NotANode, StatementNode, TypeHintNode,
    VariableAssignmentNode, VariableDeclarationNode, VariableExprNode,
};

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

    pub fn parse(&mut self) {
        self.logger.log("");
        self.logger.log("-------------");
        self.logger.log("BEGIN Parsing");
        self.logger.log("-------------");

        // Parse a list of statements and/or expressions
        let constructs = self.constructs();

        // Add these constructs to the root node
        match self.ast.get_mut_root_node() {
            ASTNode::RootNode(root_node) => root_node.add_children(constructs),
            _ => panic!("get_mut_root_node should only return a RootNode"),
        }

        self.logger.log("\nAST after parsing:");
        self.logger.log(&format!(
            "{}",
            self.ast.get_root_node().to_string(&self.ast)
        ));
    }

    // Parse a list of expressions or statements
    pub fn constructs(&mut self) -> Vec<NodePointer> {
        let mut constructs = vec![];

        while !self.is_at_end() {
            // If the current token is "Let"
            // (which is the first token of a variable assignment),
            // parse a statement
            if self.current_token().token_type == TokenType::Let {
                constructs.push(self.statement());
            }
            // Otherwise, parse an expression
            else {
                constructs.push(self.expression());
            }
        }

        return constructs;
    }

    /*
     * statement  -> var_inst | var_assmt ;
     * var_decl   -> "let" ["mut"] ID ["(" type ")"] ":=" expression ";" ;
     * var_assmt  -> ID ":=" expression ";" ;
     * type       ->   "i8" | "i16" | "i32" | "i64" | "i128"
     *               | "u8" | "u16" | "u32" | "u64" | "u128"
     *               | "f32" | "f64"
     *               | "bool" | "str"
     *               | ID
     * expression -> equality ;
     * equality   -> comparison ( ( "!=" | "==" ) comparison )* ;
     * comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
     * term       -> factor ( ( "-" | "+" ) factor )* ;
     * factor     -> unary ( ( "/" | "*" ) unary )* ;
     * unary      -> ( "!" | "-" ) unary | primary ;
     * primary    -> NUM | STR | "true" | "false" | "(" expression ")" ;
     */

    pub fn statement(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::statement(): current token {:?}",
            token_type,
        ));
        if token_type == TokenType::Let {
            return self.var_decl();
        } else {
            return self.var_assmt();
        }
    }

    pub fn var_decl(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::var_decl(): current token {:?}",
            token_type,
        ));

        // "let"
        if !self.is_at_end() && !self.match_token(&[TokenType::Let]) {
            let token_type = self.current_token().token_type.clone();
            let location_info = &self.current_token().location_info;
            match self.current_token().lexeme {
                Some(lexeme) => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \"let\" but got \"{}\"", lexeme)),
                        location_info,
                    )],
                    None,
                ),
                None => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \"let\" but got \"{:?}\"", token_type)),
                        location_info,
                    )],
                    None,
                ),
            }
        }
        self.consume();

        // ["mut"]
        let mut mutable = false;
        if !self.is_at_end() && self.match_token(&[TokenType::Mut]) {
            mutable = true;
            self.consume();
        }

        // ID
        let id;
        let variable_name;
        if !self.is_at_end() && self.match_token(&[TokenType::ID]) {
            let id_token = self.current_token().clone();
            match self.current_token().lexeme {
                Some(lexeme) => {
                    variable_name = lexeme.clone();
                    id = self
                        .ast
                        .new_node(ASTNode::IdentifierNode(IdentifierStatementNode::new(
                            lexeme, id_token,
                        )));
                }
                None => panic!("Should have a lexeme"),
            }
        } else {
            let token_type = self.current_token().token_type.clone();
            let location_info = &self.current_token().location_info;
            match self.current_token().lexeme {
                Some(lexeme) => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{}\"", lexeme)),
                        location_info,
                    )],
                    None,
                ),
                None => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{:?}\"", token_type)),
                        location_info,
                    )],
                    None,
                ),
            }
            variable_name = String::from("NotANode");
            id = self.ast.new_node(ASTNode::NotANode(NotANode::new()));
        }
        self.consume();

        // ["(" type ")"]
        let mut type_hint = None;
        if !self.is_at_end() && self.match_token(&[TokenType::LeftParen]) {
            let left_paren_location = &self.current_token().location_info;
            self.consume();

            type_hint = Some(self.type_());

            if !self.is_at_end() && !self.match_token(&[TokenType::RightParen]) {
                let token_type = self.current_token().token_type.clone();
                let location_info = &self.current_token().location_info;
                match self.current_token().lexeme {
                    Some(lexeme) => self.error.report(
                        ErrorType::UnexpectedTokenError,
                        vec![
                            ErrorMessage::new(
                                Some(format!(
                                    "Expected \")\" after type hint but got \"{}\"",
                                    lexeme
                                )),
                                location_info,
                            ),
                            ErrorMessage::new(
                                Some(String::from("Un-matched left parenthesis")),
                                left_paren_location,
                            ),
                        ],
                        None,
                    ),
                    None => self.error.report(
                        ErrorType::UnexpectedTokenError,
                        vec![
                            ErrorMessage::new(
                                Some(format!(
                                    "Expected \")\" after type hint but got \"{:?}\"",
                                    token_type
                                )),
                                location_info,
                            ),
                            ErrorMessage::new(
                                Some(String::from("Un-matched left parenthesis")),
                                left_paren_location,
                            ),
                        ],
                        None,
                    ),
                }
            }

            self.consume();
        }

        // ":="
        let assmt_token = self.current_token().clone();
        if !self.is_at_end() && !self.match_token(&[TokenType::Assign]) {
            let location_info = &self.current_token().location_info;
            self.error.report(
                ErrorType::UnexpectedTokenError,
                vec![ErrorMessage::new(
                    Some(String::from("Did you mean \":=\"?")),
                    location_info,
                )],
                None,
            );
        }
        self.consume();

        // expression
        let expression = self.expression();

        // ";"
        if !self.is_at_end() && !self.match_token(&[TokenType::Semicolon]) {
            let token_type = self.current_token().token_type.clone();
            let location_info = &self.current_token().location_info;
            match self.current_token().lexeme {
                Some(lexeme) => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{}\"", lexeme)),
                        location_info,
                    )],
                    None,
                ),
                None => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{:?}\"", token_type)),
                        location_info,
                    )],
                    None,
                ),
            }
        }
        self.consume();

        return self
            .ast
            .new_node(ASTNode::StatementNode(StatementNode::VariableDeclaration(
                VariableDeclarationNode::new(
                    mutable,
                    variable_name,
                    id,
                    type_hint,
                    expression,
                    assmt_token,
                ),
            )));
    }

    pub fn var_assmt(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::var_assmt(): current token {:?}",
            token_type,
        ));

        // ID
        let id;
        let variable_name;
        if !self.is_at_end() && self.match_token(&[TokenType::ID]) {
            let id_token = self.current_token().clone();
            match self.current_token().lexeme {
                Some(lexeme) => {
                    variable_name = lexeme.clone();
                    id = self
                        .ast
                        .new_node(ASTNode::IdentifierNode(IdentifierStatementNode::new(
                            lexeme, id_token,
                        )));
                }
                None => panic!("Should have a lexeme"),
            }
        } else {
            let token_type = self.current_token().token_type.clone();
            let location_info = &self.current_token().location_info;
            match self.current_token().lexeme {
                Some(lexeme) => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{}\"", lexeme)),
                        location_info,
                    )],
                    None,
                ),
                None => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{:?}\"", token_type)),
                        location_info,
                    )],
                    None,
                ),
            }
            variable_name = String::from("NotANode");
            id = self.ast.new_node(ASTNode::NotANode(NotANode::new()));
        }
        self.consume();

        // ":="
        let assmt_token = self.current_token().clone();
        if !self.is_at_end() && !self.match_token(&[TokenType::Assign]) {
            let location_info = &self.current_token().location_info;
            self.error.report(
                ErrorType::UnexpectedTokenError,
                vec![ErrorMessage::new(
                    Some(String::from("Did you mean \":=\"?")),
                    location_info,
                )],
                None,
            );
        }
        self.consume();

        // expression
        let expression = self.expression();

        // ";"
        if !self.is_at_end() && !self.match_token(&[TokenType::Semicolon]) {
            let token_type = self.current_token().token_type.clone();
            let location_info = &self.current_token().location_info;
            match self.current_token().lexeme {
                Some(lexeme) => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{}\"", lexeme)),
                        location_info,
                    )],
                    None,
                ),
                None => self.error.report(
                    ErrorType::UnexpectedTokenError,
                    vec![ErrorMessage::new(
                        Some(format!("Expected \";\" but got \"{:?}\"", token_type)),
                        location_info,
                    )],
                    None,
                ),
            }
        }
        self.consume();

        return self
            .ast
            .new_node(ASTNode::StatementNode(StatementNode::VariableAssignment(
                VariableAssignmentNode::new(variable_name, id, expression, assmt_token),
            )));
    }

    pub fn type_(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger
            .log(&format!("Parser::type_(): current token {:?}", token_type,));

        let type_;
        match self.current_token().token_type {
            TokenType::I8 => type_ = DataType::new(Type::Primitive(Primitive::I8)),
            TokenType::I16 => type_ = DataType::new(Type::Primitive(Primitive::I16)),
            TokenType::I32 => type_ = DataType::new(Type::Primitive(Primitive::I32)),
            TokenType::I64 => type_ = DataType::new(Type::Primitive(Primitive::I64)),
            TokenType::I128 => type_ = DataType::new(Type::Primitive(Primitive::I128)),
            TokenType::U8 => type_ = DataType::new(Type::Primitive(Primitive::U8)),
            TokenType::U16 => type_ = DataType::new(Type::Primitive(Primitive::U16)),
            TokenType::U32 => type_ = DataType::new(Type::Primitive(Primitive::U32)),
            TokenType::U64 => type_ = DataType::new(Type::Primitive(Primitive::U64)),
            TokenType::U128 => type_ = DataType::new(Type::Primitive(Primitive::U128)),
            TokenType::F32 => type_ = DataType::new(Type::Primitive(Primitive::F32)),
            TokenType::F64 => type_ = DataType::new(Type::Primitive(Primitive::F64)),
            TokenType::Bool => {
                type_ = DataType::new(Type::Primitive(Primitive::Bool));
            }
            TokenType::Str => {
                type_ = DataType::new(Type::Primitive(Primitive::String));
            }
            _ => type_ = DataType::new(Type::UnTyped),
        }

        let type_token = self.current_token().clone();
        self.consume();
        return self
            .ast
            .new_node(ASTNode::TypeHintNode(TypeHintNode::new(type_, type_token)));
    }

    pub fn expression(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::expression(): current token {:?}",
            token_type,
        ));
        return self.equality();
    }

    pub fn equality(&mut self) -> NodePointer {
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::equality(): current token {:?}",
            token_type,
        ));

        // Get the expression on the left of the operator
        self.logger
            .log("Parser::equality(): Parsing left expression:");
        let mut left = self.comparison();

        // First, if the operator is :=, we actually have a variable assignment, not a binary
        // expression
        if self.match_token(&[TokenType::Assign]) {
            self.retreat();
            return self.var_assmt();
        }

        while !self.is_at_end()
            && self.match_token(&[TokenType::NotEqual, TokenType::Equal, TokenType::BadAssign])
        {
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
                TokenType::BadAssign => {
                    self.error.report(
                        ErrorType::UnexpectedTokenError,
                        vec![ErrorMessage::new(
                            Some(String::from("Did you mean \"==\"?")),
                            &operator.location_info,
                        )],
                        None,
                    );
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
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::comparison(): current token {:?}",
            token_type,
        ));

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
        let token_type = self.current_token().token_type;
        self.logger
            .log(&format!("Parser::term(): current token {:?}", token_type,));

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
        let token_type = self.current_token().token_type;
        self.logger
            .log(&format!("Parser::factor(): current token {:?}", token_type,));

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
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::boolean(): current token {:?}",
            token_type,
        ));

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
        let token_type = self.current_token().token_type;
        self.logger
            .log(&format!("Parser::unary(): current token {:?}", token_type,));

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
        let token_type = self.current_token().token_type;
        self.logger.log(&format!(
            "Parser::primary(): current token {:?}",
            token_type,
        ));

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
                    TokenType::ID => {
                        self.consume();
                        let pointer =
                            self.ast
                                .new_node(ASTNode::ExpressionNode(ExpressionNode::Variable(
                                    VariableExprNode::new(lexeme.to_owned(), current_token.clone()),
                                )));

                        pointer
                    }
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

                        self.consume();

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

    fn retreat(&mut self) {
        self.current -= 1;
    }
}
