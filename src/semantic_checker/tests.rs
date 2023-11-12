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

use crate::{
    infrastructure::{
        error::{ConsoleErrorReporter, ErrorReporter, ErrorType},
        file::LocationInfo,
        log::Logger,
    },
    parser::parser_data::{
        ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode, NodePointer,
        UnaryExprNode, UnaryOperator, AST,
    },
    scanner::scanner_data::{Token, TokenType},
    semantic_checker::semantic_checker_data::{DataType, Primitive, Type},
};

use super::semantic_checker::SemanticChecker;

#[test]
pub fn test_type_check_literal() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    let int_literal_type = semantic_checker.type_check_literal(&mut LiteralNode::new(
        Literal::Int(1),
        generate_generic_token(),
    ));

    assert_eq!(
        int_literal_type,
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        )
    );

    let float_literal_type = semantic_checker.type_check_literal(&mut LiteralNode::new(
        Literal::Float(3.14),
        generate_generic_token(),
    ));

    assert_eq!(
        float_literal_type,
        DataType::new(
            Type::Primitive(Primitive::CompileTimeFloat),
            Type::Primitive(Primitive::CompileTimeFloat).get_label()
        )
    );

    let true_literal_type = semantic_checker.type_check_literal(&mut LiteralNode::new(
        Literal::True(true),
        generate_generic_token(),
    ));

    assert_eq!(
        true_literal_type,
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        )
    );

    let false_literal_type = semantic_checker.type_check_literal(&mut LiteralNode::new(
        Literal::False(false),
        generate_generic_token(),
    ));

    assert_eq!(
        false_literal_type,
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        )
    );

    let string_literal_type = semantic_checker.type_check_literal(&mut LiteralNode::new(
        Literal::String(String::from("test string")),
        generate_generic_token(),
    ));

    assert_eq!(
        string_literal_type,
        DataType::new(
            Type::Primitive(Primitive::String),
            Type::Primitive(Primitive::String).get_label()
        )
    );
}

#[test]
pub fn test_type_check_arithmetic_binary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(2));

    // ----
    // PLUS
    // ----
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Plus,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // -----
    // MINUS
    // -----
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Minus,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // -----
    // TIMES
    // -----
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Times,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // ------
    // DIVIDE
    // ------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Divide,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // -------
    // MODULUS
    // -------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Modulus,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );
}

#[test]
pub fn test_type_check_boolean_binary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // ---
    // AND
    // ---
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::And,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // --
    // OR
    // --
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Or,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        ))
    );
}

#[test]
pub fn test_type_check_comparison_binary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(2));

    // ---------
    // LESS THAN
    // ---------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::LessThan,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // ------------------
    // LESS THAN OR EQUAL
    // ------------------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::LessThanOrEqual,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // ------------
    // GREATER THAN
    // ------------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::GreaterThan,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // ---------------------
    // GREATER THAN OR EQUAL
    // ---------------------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::GreaterThanOrEqual,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );
}

#[test]
pub fn test_type_check_equality_binary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // -----
    // EQUAL
    // -----
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::Equal,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );

    // ---------
    // NOT EQUAL
    // ---------
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_binary(&mut BinaryExprNode::new(
            BinaryOperator::NotEqual,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        )),
    );
}

#[test]
pub fn test_type_check_unary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------
    // Setting up operands
    // -------------------
    let int_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let bool_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // -----
    // MINUS
    // -----
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label()
        ),
        semantic_checker.type_check_unary(&mut UnaryExprNode::new(
            UnaryOperator::Minus,
            generate_generic_token(),
            int_pointer
        )),
    );

    // ---
    // NOT
    // ---
    assert_eq!(
        DataType::new(
            Type::Primitive(Primitive::Bool),
            Type::Primitive(Primitive::Bool).get_label()
        ),
        semantic_checker.type_check_unary(&mut UnaryExprNode::new(
            UnaryOperator::Not,
            generate_generic_token(),
            bool_pointer
        )),
    );
}

#[test]
pub fn test_error_type_check_binary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -------------------------------------------------
    // ERROR 1: Boolean operands in arithmetic operators
    // -------------------------------------------------
    let mut left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    let mut right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    semantic_checker.type_check_binary(&mut BinaryExprNode::new(
        BinaryOperator::Plus,
        generate_generic_token(),
        left_pointer,
        right_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }

    // -------------------------------------------------
    // ERROR 2: Boolean operands in comparison operators
    // -------------------------------------------------
    left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    semantic_checker.type_check_binary(&mut BinaryExprNode::new(
        BinaryOperator::LessThan,
        generate_generic_token(),
        left_pointer,
        right_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }

    // ---------------------------------------------
    // ERROR 3: Number operands in boolean operators
    // ---------------------------------------------
    left_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(2));

    semantic_checker.type_check_binary(&mut BinaryExprNode::new(
        BinaryOperator::And,
        generate_generic_token(),
        left_pointer,
        right_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }

    // ---------------------------------------------------------
    // ERROR 4: Differently-typed operands in equality operators
    // ---------------------------------------------------------
    left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));

    semantic_checker.type_check_binary(&mut BinaryExprNode::new(
        BinaryOperator::Equal,
        generate_generic_token(),
        left_pointer,
        right_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }
}

#[test]
pub fn test_error_type_check_unary_expr() {
    let mut ast = AST::new();

    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut semantic_checker = SemanticChecker::new(&mut ast, &mut logger, &mut error);

    // -----------------------------------------------
    // ERROR 1: Boolean operand in arithmetic operator
    // -----------------------------------------------
    let mut operand_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));

    semantic_checker.type_check_unary(&mut UnaryExprNode::new(
        UnaryOperator::Minus,
        generate_generic_token(),
        operand_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }

    // -------------------------------------------
    // ERROR 2: Number operand in boolean operator
    // -------------------------------------------
    operand_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));

    semantic_checker.type_check_unary(&mut UnaryExprNode::new(
        UnaryOperator::Not,
        generate_generic_token(),
        operand_pointer,
    ));

    match semantic_checker.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::IncompatibleTypesError)
            );
            console_error.clear_for_next_execution();
        }
    }
}

pub fn generate_literal_operand(ast: &mut AST, val: Literal) -> NodePointer {
    let operand_pointer = ast.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(val.clone(), generate_generic_token()),
    )));

    match val {
        Literal::Int(_) => ast.get_mut_node(operand_pointer).set_type(DataType::new(
            Type::Primitive(Primitive::CompileTimeInt),
            Type::Primitive(Primitive::CompileTimeInt).get_label(),
        )),
        Literal::Float(_) => ast.get_mut_node(operand_pointer).set_type(DataType::new(
            Type::Primitive(Primitive::CompileTimeFloat),
            Type::Primitive(Primitive::CompileTimeFloat).get_label(),
        )),
        Literal::True(_) | Literal::False(_) => {
            ast.get_mut_node(operand_pointer).set_type(DataType::new(
                Type::Primitive(Primitive::Bool),
                Type::Primitive(Primitive::Bool).get_label(),
            ))
        }
        Literal::String(_) => ast.get_mut_node(operand_pointer).set_type(DataType::new(
            Type::Primitive(Primitive::String),
            Type::Primitive(Primitive::String).get_label(),
        )),
    }

    return operand_pointer;
}

// Since tokens are useless for the purpose of testing,
// we can save a lot of lines by generating the same one
// every time
pub fn generate_generic_token() -> Token {
    let mut location_info = LocationInfo::new();
    location_info.offset = Some(1);
    location_info.length = Some(1);
    location_info.repl_str = Some(String::from("repl_str"));

    return Token::new(
        TokenType::True,
        Some(String::from("true")),
        true,
        true,
        location_info,
    );
}
