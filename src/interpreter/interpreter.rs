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
        error::{ErrorMessage, ErrorReporter, ErrorType},
        log::Logger,
    },
    interpreter::interpreter_data::LiteralVal,
    parser::parser_data::{
        ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, NodePointer,
        UnaryOperator, AST,
    },
    semantic_checker::{
        semantic_checker::{is_i16, is_i32, is_i64, is_i8, is_u16, is_u32, is_u64, is_u8},
        semantic_checker_data::{Primitive, Type},
    },
};

use super::interpreter_data::Value;

pub struct Interpreter<'interpret> {
    pub ast: &'interpret mut AST,
    pub logger: &'interpret mut Logger,
    pub error: &'interpret mut ErrorReporter,
}

impl Interpreter<'_> {
    pub fn new<'interpret>(
        ast: &'interpret mut AST,
        logger: &'interpret mut Logger,
        error: &'interpret mut ErrorReporter,
    ) -> Interpreter<'interpret> {
        return Interpreter { ast, logger, error };
    }

    pub fn run(&mut self, node: NodePointer) {
        self.logger.log("");
        self.logger.log("--------------------");
        self.logger.log("BEGIN Interpretation");
        self.logger.log("--------------------");

        self.interpret_node(node);
    }

    pub fn interpret_node(&mut self, node: NodePointer) {
        match self.ast.get_node(node) {
            ASTNode::RootNode(node) => {
                // The child of the root node is an expression,
                // so evaluate it and print the result
                let value = self.evaluate_expression(node.expression);
                match value {
                    Value::LiteralVal(literal) => match literal {
                        LiteralVal::Int(int_val) => println!("{}", int_val),
                        LiteralVal::Float(float_val) => println!("{}", float_val),
                        LiteralVal::Bool(bool_val) => println!("{}", bool_val),
                        LiteralVal::String(string_val) => println!("{}", string_val),
                    },
                }
                println!(""); // Extra newline
            }
            _ => todo!("Do all the other ones!!"),
        }
    }

    pub fn evaluate_expression(&mut self, node: NodePointer) -> Value {
        match self.ast.get_node(node).clone() {
            ASTNode::ExpressionNode(expr) => {
                match expr {
                    ExpressionNode::Literal(literal) => {
                        // Evaluating a literal expression is very easy,
                        // we can just return the value
                        match literal.value {
                            Literal::Int(intval) => {
                                return Value::LiteralVal(LiteralVal::Int(intval))
                            }
                            Literal::Float(floatval) => {
                                return Value::LiteralVal(LiteralVal::Float(floatval))
                            }
                            Literal::True(boolval) | Literal::False(boolval) => {
                                return Value::LiteralVal(LiteralVal::Bool(boolval))
                            }
                            Literal::String(stringval) => {
                                return Value::LiteralVal(LiteralVal::String(stringval))
                            }
                        }
                    }
                    ExpressionNode::Binary(binary) => {
                        // To evaluate a binary expression,
                        // we simply have to evaluate both operands
                        // and then perform the operation on them
                        let left_value = self.evaluate_expression(binary.left);
                        let right_value = self.evaluate_expression(binary.right);

                        match binary.operator {
                            BinaryOperator::Plus
                            | BinaryOperator::Minus
                            | BinaryOperator::Times
                            | BinaryOperator::Divide
                            | BinaryOperator::Modulus => {
                                return self.evaluate_binary_arithmetic_expression(
                                    &binary,
                                    &left_value,
                                    &right_value,
                                )
                            }
                            BinaryOperator::And | BinaryOperator::Or => {
                                return self.evaluate_binary_boolean_expression(
                                    &binary,
                                    &left_value,
                                    &right_value,
                                )
                            }
                            BinaryOperator::LessThan
                            | BinaryOperator::LessThanOrEqual
                            | BinaryOperator::GreaterThan
                            | BinaryOperator::GreaterThanOrEqual => {
                                return self.evaluate_binary_comparison_expression(
                                    &binary,
                                    &left_value,
                                    &right_value,
                                )
                            }
                            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                                return Value::LiteralVal(LiteralVal::Bool(
                                    left_value == right_value,
                                ))
                            }
                        }
                    }
                    ExpressionNode::Unary(unary) => {
                        // To evaluate a binary expression,
                        // we simply have to evaluate the operand
                        // and then perform the operation on it
                        let operand_value = self.evaluate_expression(unary.operand);

                        match unary.operator {
                            UnaryOperator::Minus => match operand_value {
                                Value::LiteralVal(operand_value) => match operand_value {
                                    LiteralVal::Int(operand_value) => {
                                        return Value::LiteralVal(LiteralVal::Int(-operand_value))
                                    }
                                    LiteralVal::Float(operand_value) => {
                                        return Value::LiteralVal(LiteralVal::Float(-operand_value))
                                    }
                                    _ => panic!("Should have type checked away -non-number"),
                                },
                            },
                            UnaryOperator::Not => match operand_value {
                                Value::LiteralVal(operand_value) => match operand_value {
                                    LiteralVal::Bool(operand_value) => {
                                        return Value::LiteralVal(LiteralVal::Bool(!operand_value))
                                    }
                                    _ => panic!("Should have type checked away not non-bool"),
                                },
                            },
                        }
                    }
                }
            }
            ASTNode::RootNode(_) => panic!("interpret_expression called on RootNode"),
            ASTNode::NotANode(_) => panic!("interpret_expression called on NotANode"),
        }
    }

    pub fn evaluate_binary_boolean_expression(
        &mut self,
        op_node: &BinaryExprNode,
        left_value: &Value,
        right_value: &Value,
    ) -> Value {
        match left_value {
            Value::LiteralVal(left_value) => {
                match right_value {
                    Value::LiteralVal(right_value) => {
                        match left_value {
                            LiteralVal::Bool(left_value) => {
                                match right_value {
                                    LiteralVal::Bool(right_value) => {
                                        // Calculate our new value
                                        let new_value;
                                        match op_node.operator {
                                            BinaryOperator::And => {
                                                new_value = *left_value && *right_value
                                            }
                                            BinaryOperator::Or => {
                                                new_value = *left_value || *right_value
                                            }
                                            _ => panic!(
                                                "This function is only for boolean expressions"
                                            ),
                                        }
                                        return Value::LiteralVal(LiteralVal::Bool(new_value));
                                    }
                                    _ => panic!(
                                        "Should have already type checked away Bool {} non-Bool",
                                        op_node.get_operator()
                                    ),
                                }
                            }
                            _ => panic!(
                                "Should have already type checked away {} on non-Bool",
                                op_node.get_operator()
                            ),
                        }
                    }
                }
            }
        }
    }

    pub fn evaluate_binary_comparison_expression(
        &mut self,
        op_node: &BinaryExprNode,
        left_value: &Value,
        right_value: &Value,
    ) -> Value {
        match left_value {
            Value::LiteralVal(left_value) => {
                match right_value {
                    Value::LiteralVal(right_value) => {
                        match left_value {
                            LiteralVal::Int(left_value) => {
                                match right_value {
                                    LiteralVal::Int(right_value) => {
                                        // Calculate our new value
                                        let new_value;
                                        match op_node.operator {
                                            BinaryOperator::LessThan => new_value = left_value < right_value,
                                            BinaryOperator::LessThanOrEqual => new_value = left_value <= right_value,
                                            BinaryOperator::GreaterThan => new_value = left_value > right_value,
                                            BinaryOperator::GreaterThanOrEqual => new_value = left_value >= right_value,
                                            _ => panic!("This function is only for binary arithmetic expressions")
                                        }
                                        return Value::LiteralVal(LiteralVal::Bool(new_value));
                                    }
                                    _ => panic!(
                                        "Should have already type checked away Int {} non-Int",
                                        op_node.get_operator()
                                    ),
                                }
                            }
                            LiteralVal::Float(left_value) => {
                                match right_value {
                                    LiteralVal::Float(right_value) => {
                                        // Calculate our new value
                                        let new_value;
                                        match op_node.operator {
                                            BinaryOperator::LessThan => new_value = left_value < right_value,
                                            BinaryOperator::LessThanOrEqual => new_value = left_value <= right_value,
                                            BinaryOperator::GreaterThan => new_value = left_value > right_value,
                                            BinaryOperator::GreaterThanOrEqual => new_value = left_value >= right_value,
                                            _ => panic!("This function is only for binary arithmetic expressions")
                                        }
                                        return Value::LiteralVal(LiteralVal::Bool(new_value));
                                    }
                                    _ => panic!(
                                        "Should have already type checked away Float {} non-Float",
                                        op_node.get_operator()
                                    ),
                                }
                            }
                            _ => panic!(
                                "Should have already type checked away non-number {} non-number",
                                op_node.get_operator()
                            ),
                        }
                    }
                }
            }
        }
    }

    pub fn evaluate_binary_arithmetic_expression(
        &mut self,
        op_node: &BinaryExprNode,
        left_value: &Value,
        right_value: &Value,
    ) -> Value {
        match left_value {
            Value::LiteralVal(left_value) => {
                match right_value {
                    Value::LiteralVal(right_value) => {
                        match left_value {
                            LiteralVal::Int(left_value) => {
                                match right_value {
                                    LiteralVal::Int(right_value) => {
                                        // Calculate our new value
                                        let new_value;
                                        match op_node.operator {
                                            BinaryOperator::Plus => new_value = left_value + right_value,
                                            BinaryOperator::Minus => new_value = left_value - right_value,
                                            BinaryOperator::Times => new_value = left_value * right_value,
                                            BinaryOperator::Divide => new_value = left_value / right_value,
                                            BinaryOperator::Modulus => new_value = left_value % right_value,
                                            _ => panic!("This function is only for binary arithmetic expressions")
                                        }

                                        // We want to make sure the new value
                                        // doesn't overflow,
                                        // we've already made sure
                                        // left_type == right_type
                                        // during type checking
                                        let left_type = self.ast.get_node(op_node.left).get_type();
                                        if is_i8(&left_type) {
                                            match i8::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as i8, new_value
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if i16::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I16).get_label()
                                                                } else if i32::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I32).get_label()
                                                                } else if i64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::I128).get_label()
                                                                }
                                                            )
                                                        )
                                                    );
                                                }
                                            }
                                        } else if is_i16(&left_type) {
                                            match i16::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as i16,
                                                                        new_value
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if i32::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I32).get_label()
                                                                } else if i64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::I128).get_label()
                                                                }
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_i32(&left_type) {
                                            match i32::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as i32,
                                                                        new_value
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if i64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::I64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::I128).get_label()
                                                                }
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_i64(&left_type) {
                                            match i64::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as i64,
                                                                        new_value
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                Type::Primitive(Primitive::I128).get_label(),
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_u8(&left_type) {
                                            match u8::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as u8,
                                                                        new_value
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if u16::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U16).get_label()
                                                                } else if u32::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U32).get_label()
                                                                } else if u64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::U128).get_label()
                                                                }
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_u16(&left_type) {
                                            match u16::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as u16,
                                                                        new_value,
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if u32::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U32).get_label()
                                                                } else if u64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::U128).get_label()
                                                                }
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_u32(&left_type) {
                                            match u32::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
    op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as u32,
                                                                        new_value,
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                if u64::MAX as i128 > new_value {
                                                                    Type::Primitive(Primitive::U64).get_label()
                                                                } else {
                                                                    Type::Primitive(Primitive::U128).get_label()
                                                                }
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        } else if is_u64(&left_type) {
                                            match u64::try_from(new_value) {
                                                Ok(_) => {
                                                    return Value::LiteralVal(LiteralVal::Int(
                                                        new_value,
                                                    ))
                                                }
                                                Err(_) => {
                                                    self.error.report(
                                                        ErrorType::OverflowError,
                                                        vec![
                                                            ErrorMessage::new(
                                                                Some(
                                                                    format!(
                                                                        "Expression {} {} {} overflows to a value of {} instead of {}",
                                                                        op_node.get_operator(),
                                                                        left_value,
                                                                        right_value,
                                                                        new_value as u64,
                                                                        new_value,
                                                                    )
                                                                ),
                                                                &op_node.get_token().location_info,
                                                            ),
                                                        ],
                                                        Some(
                                                            format!(
                                                                "Consider using a larger type, '{}' would fit the result",
                                                                Type::Primitive(Primitive::U128).get_label(),
                                                            )
                                                        ),
                                                    );
                                                }
                                            }
                                        }
                                        return Value::LiteralVal(LiteralVal::Int(new_value));
                                    }
                                    _ => panic!(
                                        "Should have already type checked away Int + non-Int"
                                    ),
                                }
                            }
                            LiteralVal::Float(left_value) => {
                                match right_value {
                                    LiteralVal::Float(right_value) => {
                                        // We should have already type checked away
                                        // any non-equal types, don't think too
                                        // hard about overflow for now I guess
                                        // Calculate our new value
                                        let new_value;
                                        match op_node.operator {
                                            BinaryOperator::Plus => new_value = left_value + right_value,
                                            BinaryOperator::Minus => new_value = left_value - right_value,
                                            BinaryOperator::Times => new_value = left_value * right_value,
                                            BinaryOperator::Divide => new_value = left_value / right_value,
                                            BinaryOperator::Modulus => new_value = left_value % right_value,
                                            _ => panic!("This function is only for binary arithmetic expressions")
                                        }
                                        return Value::LiteralVal(LiteralVal::Float(new_value));
                                    }
                                    _ => panic!(
                                        "Should have already type checked away Float {} non-Float",
                                        op_node.get_operator()
                                    ),
                                }
                            }
                            _ => panic!(
                                "Should have already type checked away {} on non-number",
                                op_node.get_operator()
                            ),
                        }
                    }
                }
            }
        }
    }
}
