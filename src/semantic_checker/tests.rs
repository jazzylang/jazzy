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
        error::{ConsoleErrorReporter, ErrorReporter},
        file::LocationInfo,
        log::Logger,
    },
    parser::parser_data::{
        ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode, NodePointer,
        UnaryExprNode, UnaryOperator, AST,
    },
    scanner::scanner_data::{Token, TokenType},
    semantic_checker::semantic_checker_data::{DataType, Primitive, SymbolTable, Type},
};

use super::semantic_checker::*;

#[test]
pub fn test_type_check_literal() {
    let int_literal_type = type_check_literal(&mut LiteralNode::new(
        Literal::Int(1),
        generate_generic_token(),
    ));

    assert_eq!(
        int_literal_type,
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),)
    );

    let float_literal_type = type_check_literal(&mut LiteralNode::new(
        Literal::Float(3.14),
        generate_generic_token(),
    ));

    assert_eq!(
        float_literal_type,
        DataType::new(Type::Primitive(Primitive::CompileTimeFloat),)
    );

    let true_literal_type = type_check_literal(&mut LiteralNode::new(
        Literal::True(true),
        generate_generic_token(),
    ));

    assert_eq!(
        true_literal_type,
        DataType::new(Type::Primitive(Primitive::Bool),)
    );

    let false_literal_type = type_check_literal(&mut LiteralNode::new(
        Literal::False(false),
        generate_generic_token(),
    ));

    assert_eq!(
        false_literal_type,
        DataType::new(Type::Primitive(Primitive::Bool),)
    );

    let string_literal_type = type_check_literal(&mut LiteralNode::new(
        Literal::String(String::from("test string")),
        generate_generic_token(),
    ));

    assert_eq!(
        string_literal_type,
        DataType::new(Type::Primitive(Primitive::String),)
    );
}

#[test]
pub fn test_type_check_arithmetic_binary_expr() {
    let mut ast = AST::new();
    let mut symbol_table = SymbolTable::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let semantic_checker =
        SemanticChecker::new(&mut ast, &mut symbol_table, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(2));

    // ----
    // PLUS
    // ----
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt)),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Plus,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // -----
    // MINUS
    // -----
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Minus,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // -----
    // TIMES
    // -----
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Times,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // ------
    // DIVIDE
    // ------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Divide,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // -------
    // MODULUS
    // -------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Modulus,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );
}

#[test]
pub fn test_type_check_boolean_binary_expr() {
    let mut ast = AST::new();
    let mut symbol_table = SymbolTable::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let semantic_checker =
        SemanticChecker::new(&mut ast, &mut symbol_table, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // ---
    // AND
    // ---
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::And,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // --
    // OR
    // --
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Or,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        )
    );
}

#[test]
pub fn test_type_check_comparison_binary_expr() {
    let mut ast = AST::new();
    let mut symbol_table = SymbolTable::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let semantic_checker =
        SemanticChecker::new(&mut ast, &mut symbol_table, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(2));

    // ---------
    // LESS THAN
    // ---------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::LessThan,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // ------------------
    // LESS THAN OR EQUAL
    // ------------------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::LessThanOrEqual,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // ------------
    // GREATER THAN
    // ------------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::GreaterThan,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // ---------------------
    // GREATER THAN OR EQUAL
    // ---------------------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::GreaterThanOrEqual,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );
}

#[test]
pub fn test_type_check_equality_binary_expr() {
    let mut ast = AST::new();
    let mut symbol_table = SymbolTable::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let semantic_checker =
        SemanticChecker::new(&mut ast, &mut symbol_table, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_pointer = generate_literal_operand(semantic_checker.ast, Literal::True(true));
    let right_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // -----
    // EQUAL
    // -----
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::Equal,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );

    // ---------
    // NOT EQUAL
    // ---------
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_binary(
            &mut BinaryExprNode::new(
                BinaryOperator::NotEqual,
                generate_generic_token(),
                left_pointer,
                right_pointer,
            ),
            &mut ast,
            &mut error,
            &mut logger,
        ),
    );
}

#[test]
pub fn test_type_check_unary_expr() {
    let mut ast = AST::new();
    let mut symbol_table = SymbolTable::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let semantic_checker =
        SemanticChecker::new(&mut ast, &mut symbol_table, &mut logger, &mut error);

    // -------------------
    // Setting up operands
    // -------------------
    let int_pointer = generate_literal_operand(semantic_checker.ast, Literal::Int(1));
    let bool_pointer = generate_literal_operand(semantic_checker.ast, Literal::False(false));

    // -----
    // MINUS
    // -----
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::CompileTimeInt),),
        type_check_unary(
            &mut UnaryExprNode::new(UnaryOperator::Minus, generate_generic_token(), int_pointer),
            &mut ast,
            &mut error,
        ),
    );

    // ---
    // NOT
    // ---
    assert_eq!(
        DataType::new(Type::Primitive(Primitive::Bool),),
        type_check_unary(
            &mut UnaryExprNode::new(UnaryOperator::Not, generate_generic_token(), bool_pointer),
            &mut ast,
            &mut error,
        ),
    );
}

pub fn generate_literal_operand(ast: &mut AST, val: Literal) -> NodePointer {
    let operand_pointer = ast.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(val.clone(), generate_generic_token()),
    )));

    match val {
        Literal::Int(_) => ast
            .get_mut_node(operand_pointer)
            .set_type(DataType::new(Type::Primitive(Primitive::CompileTimeInt))),
        Literal::Float(_) => ast
            .get_mut_node(operand_pointer)
            .set_type(DataType::new(Type::Primitive(Primitive::CompileTimeFloat))),
        Literal::True(_) | Literal::False(_) => ast
            .get_mut_node(operand_pointer)
            .set_type(DataType::new(Type::Primitive(Primitive::Bool))),
        Literal::String(_) => ast
            .get_mut_node(operand_pointer)
            .set_type(DataType::new(Type::Primitive(Primitive::String))),
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
