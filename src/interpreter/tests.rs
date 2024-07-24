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

use std::collections::HashMap;

use crate::{
    infrastructure::{
        error::{ConsoleErrorReporter, ErrorReporter, ErrorType},
        log::Logger,
    },
    interpreter::interpreter_data::{LiteralVal, Value},
    parser::parser_data::{BinaryExprNode, BinaryOperator, Literal, AST},
    semantic_checker::{
        semantic_checker_data::{DataType, Primitive, Type},
        tests::{generate_generic_token, generate_literal_operand},
    },
};

use super::interpreter::Interpreter;

#[test]
fn test_evaluate_binary_arithmetic_expression() {
    let mut ast = AST::new();
    let mut variables = HashMap::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut interpreter = Interpreter::new(&mut ast, &mut variables, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let left_int_pointer = generate_literal_operand(interpreter.ast, Literal::Int(1));
    let left_int_value = Value::LiteralVal(LiteralVal::Int(1));
    let right_int_pointer = generate_literal_operand(interpreter.ast, Literal::Int(2));
    let right_int_value = Value::LiteralVal(LiteralVal::Int(2));

    let left_float_pointer = generate_literal_operand(interpreter.ast, Literal::Float(1.0));
    let left_float_value = Value::LiteralVal(LiteralVal::Float(1.0));
    let right_float_pointer = generate_literal_operand(interpreter.ast, Literal::Float(2.0));
    let right_float_value = Value::LiteralVal(LiteralVal::Float(2.0));

    // ----
    // PLUS
    // ----
    assert_eq!(
        Value::LiteralVal(LiteralVal::Int(3)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Plus,
                generate_generic_token(),
                left_int_pointer,
                right_int_pointer
            ),
            &left_int_value,
            &right_int_value
        )
    );

    assert_eq!(
        Value::LiteralVal(LiteralVal::Float(3.0)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Plus,
                generate_generic_token(),
                left_float_pointer,
                right_float_pointer
            ),
            &left_float_value,
            &right_float_value
        )
    );

    // -----
    // MINUS
    // -----
    assert_eq!(
        Value::LiteralVal(LiteralVal::Int(-1)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Minus,
                generate_generic_token(),
                left_int_pointer,
                right_int_pointer
            ),
            &left_int_value,
            &right_int_value
        )
    );

    assert_eq!(
        Value::LiteralVal(LiteralVal::Float(-1.0)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Minus,
                generate_generic_token(),
                left_float_pointer,
                right_float_pointer
            ),
            &left_float_value,
            &right_float_value
        )
    );

    // -----
    // TIMES
    // -----
    assert_eq!(
        Value::LiteralVal(LiteralVal::Int(2)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Times,
                generate_generic_token(),
                left_int_pointer,
                right_int_pointer
            ),
            &left_int_value,
            &right_int_value
        )
    );

    assert_eq!(
        Value::LiteralVal(LiteralVal::Float(2.0)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Times,
                generate_generic_token(),
                left_float_pointer,
                right_float_pointer
            ),
            &left_float_value,
            &right_float_value
        )
    );

    // ------
    // DIVIDE
    // ------
    assert_eq!(
        Value::LiteralVal(LiteralVal::Int(0)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Divide,
                generate_generic_token(),
                left_int_pointer,
                right_int_pointer
            ),
            &left_int_value,
            &right_int_value
        )
    );

    assert_eq!(
        Value::LiteralVal(LiteralVal::Float(0.5)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Divide,
                generate_generic_token(),
                left_float_pointer,
                right_float_pointer
            ),
            &left_float_value,
            &right_float_value
        )
    );

    // -------
    // MODULUS
    // -------
    // 1 mod 2 == 1
    assert_eq!(
        Value::LiteralVal(LiteralVal::Int(1)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Modulus,
                generate_generic_token(),
                left_int_pointer,
                right_int_pointer
            ),
            &left_int_value,
            &right_int_value
        )
    );

    // 2.0 mod 1.0 == 0.0
    assert_eq!(
        Value::LiteralVal(LiteralVal::Float(0.0)),
        interpreter.evaluate_binary_arithmetic_expression(
            &BinaryExprNode::new(
                BinaryOperator::Modulus,
                generate_generic_token(),
                right_float_pointer,
                left_float_pointer
            ),
            &right_float_value,
            &left_float_value
        )
    );
}

#[test]
fn test_evaluate_binary_boolean_expression() {
    let mut ast = AST::new();
    let mut variables = HashMap::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut interpreter = Interpreter::new(&mut ast, &mut variables, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let true_pointer = generate_literal_operand(interpreter.ast, Literal::True(true));
    let true_value = Value::LiteralVal(LiteralVal::Bool(true));
    let false_pointer = generate_literal_operand(interpreter.ast, Literal::False(false));
    let false_value = Value::LiteralVal(LiteralVal::Bool(false));

    // ---
    // AND
    // ---
    // true and true == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::And,
                generate_generic_token(),
                true_pointer,
                true_pointer
            ),
            &true_value,
            &true_value
        )
    );

    // true and false == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::And,
                generate_generic_token(),
                true_pointer,
                false_pointer
            ),
            &true_value,
            &false_value
        )
    );

    // false and true == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::And,
                generate_generic_token(),
                false_pointer,
                true_pointer
            ),
            &false_value,
            &true_value
        )
    );

    // false and false == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::And,
                generate_generic_token(),
                false_pointer,
                false_pointer
            ),
            &false_value,
            &false_value
        )
    );

    // --
    // OR
    // --
    // true or true == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::Or,
                generate_generic_token(),
                true_pointer,
                true_pointer
            ),
            &true_value,
            &true_value
        )
    );

    // true or false == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::Or,
                generate_generic_token(),
                true_pointer,
                false_pointer
            ),
            &true_value,
            &false_value
        )
    );

    // false or true == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::Or,
                generate_generic_token(),
                false_pointer,
                true_pointer
            ),
            &false_value,
            &true_value
        )
    );

    // false or false == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_boolean_expression(
            &BinaryExprNode::new(
                BinaryOperator::Or,
                generate_generic_token(),
                false_pointer,
                false_pointer
            ),
            &false_value,
            &false_value
        )
    );
}

#[test]
fn test_evaluate_binary_comparison_expression() {
    let mut ast = AST::new();
    let mut variables = HashMap::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut interpreter = Interpreter::new(&mut ast, &mut variables, &mut logger, &mut error);

    // -------------------------------
    // Setting up operands (can reuse)
    // -------------------------------
    let one_pointer = generate_literal_operand(interpreter.ast, Literal::Int(1));
    let one_value = Value::LiteralVal(LiteralVal::Int(1));
    let two_pointer = generate_literal_operand(interpreter.ast, Literal::Int(2));
    let two_value = Value::LiteralVal(LiteralVal::Int(2));

    // ---------
    // LESS THAN
    // ---------
    // 1 < 2 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThan,
                generate_generic_token(),
                one_pointer,
                two_pointer
            ),
            &one_value,
            &two_value
        )
    );

    // 2 < 1 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThan,
                generate_generic_token(),
                two_pointer,
                one_pointer
            ),
            &two_value,
            &one_value
        )
    );

    // 1 < 1 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThan,
                generate_generic_token(),
                one_pointer,
                one_pointer
            ),
            &one_value,
            &one_value
        )
    );

    // ------------------
    // LESS THAN OR EQUAL
    // ------------------
    // 1 <= 2 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThanOrEqual,
                generate_generic_token(),
                one_pointer,
                two_pointer
            ),
            &one_value,
            &two_value
        )
    );

    // 2 <= 1 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThanOrEqual,
                generate_generic_token(),
                two_pointer,
                one_pointer
            ),
            &two_value,
            &one_value
        )
    );

    // 1 <= 1 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::LessThanOrEqual,
                generate_generic_token(),
                one_pointer,
                one_pointer
            ),
            &one_value,
            &one_value
        )
    );

    // ------------------
    // GREATER THAN
    // ------------------
    // 1 > 2 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThan,
                generate_generic_token(),
                one_pointer,
                two_pointer
            ),
            &one_value,
            &two_value
        )
    );

    // 2 > 1 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThan,
                generate_generic_token(),
                two_pointer,
                one_pointer
            ),
            &two_value,
            &one_value
        )
    );

    // 1 > 1 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThan,
                generate_generic_token(),
                one_pointer,
                one_pointer
            ),
            &one_value,
            &one_value
        )
    );

    // ---------------------
    // GREATER THAN OR EQUAL
    // ---------------------
    // 1 >= 2 == false
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(false)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThanOrEqual,
                generate_generic_token(),
                one_pointer,
                two_pointer
            ),
            &one_value,
            &two_value
        )
    );

    // 2 >= 1 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThanOrEqual,
                generate_generic_token(),
                two_pointer,
                one_pointer
            ),
            &two_value,
            &one_value
        )
    );

    // 1 >= 1 == true
    assert_eq!(
        Value::LiteralVal(LiteralVal::Bool(true)),
        interpreter.evaluate_binary_comparison_expression(
            &BinaryExprNode::new(
                BinaryOperator::GreaterThanOrEqual,
                generate_generic_token(),
                one_pointer,
                one_pointer
            ),
            &one_value,
            &one_value
        )
    );
}

#[test]
fn test_error_evaluate_binary_comparison_expression() {
    let mut ast = AST::new();
    let mut variables = HashMap::new();
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut interpreter = Interpreter::new(&mut ast, &mut variables, &mut logger, &mut error);

    // -------------------
    // Setting up operands
    // -------------------
    //
    // Max value of i8
    let left_pointer = generate_literal_operand(interpreter.ast, Literal::Int(255));
    let left_value = Value::LiteralVal(LiteralVal::Int(255));
    // Adding 1 to 255 will overflow i8
    let right_pointer = generate_literal_operand(interpreter.ast, Literal::Int(2));
    let right_value = Value::LiteralVal(LiteralVal::Int(1));

    // Set types of both operands to i8
    interpreter
        .ast
        .get_mut_node(left_pointer)
        .set_type(DataType::new(Type::Primitive(Primitive::I8)));

    interpreter
        .ast
        .get_mut_node(right_pointer)
        .set_type(DataType::new(Type::Primitive(Primitive::I8)));

    interpreter.evaluate_binary_arithmetic_expression(
        &BinaryExprNode::new(
            BinaryOperator::Plus,
            generate_generic_token(),
            left_pointer,
            right_pointer,
        ),
        &left_value,
        &right_value,
    );

    match interpreter.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::OverflowError)
            );
            console_error.clear_for_next_execution();
        }
    }
}
