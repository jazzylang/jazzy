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
    ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode, NodePointer,
    UnaryExprNode, UnaryOperator, AST,
};
use crate::semantic_checker::semantic_checker_data::{DataType, Type};

use super::semantic_checker_data::{CanCast, Primitive, PrimitiveClass};

pub struct SemanticChecker<'semantic> {
    pub ast: &'semantic mut AST,
    pub logger: &'semantic mut Logger,
    pub error: &'semantic mut ErrorReporter,
}

impl SemanticChecker<'_> {
    pub fn new<'semantic>(
        ast: &'semantic mut AST,
        logger: &'semantic mut Logger,
        error: &'semantic mut ErrorReporter,
    ) -> SemanticChecker<'semantic> {
        return SemanticChecker { ast, logger, error };
    }

    pub fn check(&mut self, node: NodePointer) {
        self.logger.log("");
        self.logger.log("-----------------------");
        self.logger.log("BEGIN Semantic Checking");
        self.logger.log("-----------------------");

        self.type_check(node);

        self.logger.log("\nAST after type checking:");
        self.logger
            .log(&format!("{}", self.ast.get_node(node).to_string(&self.ast)))
    }

    pub fn type_check(&mut self, node: NodePointer) {
        // First we need to type check any children of this node
        match self.ast.get_mut_node(node) {
            ASTNode::RootNode(node) => {
                let expression = node.expression;
                self.type_check(expression);
            }
            ASTNode::ExpressionNode(node) => {
                match node {
                    ExpressionNode::Binary(node) => {
                        // Type check left and right children
                        let left = node.left;
                        let right = node.right;
                        self.type_check(left);
                        self.type_check(right);
                    }
                    ExpressionNode::Unary(node) => {
                        let operand = node.operand;
                        self.type_check(operand);
                    }
                    // Leaf node, no children
                    ExpressionNode::Literal(_) => {}
                }
            }
            // Leaf node, no children
            ASTNode::NotANode(_) => {}
        };

        // Now we can type check
        self.logger.log(&format!(
            "Type checking node {}",
            self.ast.get_node(node).node_to_string()
        ));

        let node_clone = self.ast.get_mut_node(node).clone();
        match node_clone {
            ASTNode::ExpressionNode(expr) => match expr {
                ExpressionNode::Literal(mut literal_node) => {
                    let new_type = self.type_check_literal(&mut literal_node);
                    self.infer_type(node, new_type);
                }
                ExpressionNode::Binary(mut binary_node) => {
                    let new_type = self.type_check_binary(&mut binary_node);
                    self.infer_type(node, new_type);
                }
                ExpressionNode::Unary(mut unary_node) => {
                    let new_type = self.type_check_unary(&mut unary_node);
                    self.infer_type(node, new_type);
                }
            },
            // Already type checked any children
            ASTNode::RootNode(_) => {}
            // No need to type check, already UnTyped
            ASTNode::NotANode(_) => {}
        }
    }

    pub fn type_check_literal(&mut self, node: &mut LiteralNode) -> DataType {
        // Figure out what kind of literal node this is, and set its type for it
        match node.value {
            Literal::Int(_) => {
                let data_type = Type::Primitive(Primitive::CompileTimeInt);
                return DataType::new(data_type, data_type.get_label());
            }
            Literal::Float(_) => {
                let data_type = Type::Primitive(Primitive::CompileTimeFloat);
                return DataType::new(data_type, data_type.get_label());
            }
            Literal::True(_) | Literal::False(_) => {
                let data_type = Type::Primitive(Primitive::Bool);
                return DataType::new(data_type, data_type.get_label());
            }
            Literal::String(_) => {
                let data_type = Type::Primitive(Primitive::String);
                return DataType::new(data_type, data_type.get_label());
            }
        }
    }

    pub fn type_check_binary(&mut self, node: &mut BinaryExprNode) -> DataType {
        // Get the type of the node's children
        let left_pointer = node.left;
        let left_type = self.ast.get_node(node.left).get_type();
        let right_pointer = node.right;
        let right_type = self.ast.get_node(node.right).get_type();

        // If either of the operands is untyped (meaning we've thrown a type error)
        // just leave this node untyped as well
        // to try and capture as many errors as possible before quitting
        if is_untyped(&left_type) || is_untyped(&right_type) {
            return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
        } else {
            if binary_is_arithmetic(node) {
                // Both operands must be numbers, output is number
                if is_number(&left_type) && is_number(&right_type) {
                    // If both types are the same,
                    // no casting is necessary and we can just infer the type of the operator
                    if !is_compile_time(&left_type) && !is_compile_time(&right_type) {
                        // We know the types of both operands,
                        // so if they're the same type,
                        // infer the type of the operator to be the type of one of them,
                        // and if they're not the same type, throw an error
                        if left_type == right_type {
                            return left_type;
                        } else {
                            self.error.report(
                                ErrorType::IncompatibleTypesError,
                                vec![ErrorMessage::new(
                                    Some(format!(
                                        "Expressions of types '{}' and '{}' are incompatible",
                                        left_type.get_label(),
                                        right_type.get_label()
                                    )),
                                    &node.get_token().location_info,
                                )],
                                Some(
                                    if left_type.size() > right_type.size() {
                                        format!("Consider casting result of right expression into type '{}'", left_type.get_label())
                                    } else if right_type.size() > left_type.size() {
                                        format!("Consider casting result of left expression into type '{}'", right_type.get_label())
                                    } else {
                                        format!("Consider casting one type into the other")
                                    }
                                ),
                            );
                            return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                        }
                    } else if !is_compile_time(&left_type) && is_compile_time(&right_type) {
                        // We know the left type but not the right type,
                        // so infer both the right type and the operator type to be the left type
                        self.infer_type(right_pointer, left_type.clone());
                        return left_type.clone();
                    } else if is_compile_time(&left_type) && !is_compile_time(&right_type) {
                        // We know the right type but not the left type,
                        // so infer both the left type and the operator type
                        // to be the right type
                        self.infer_type(left_pointer, right_type.clone());
                        return right_type.clone();
                    } else {
                        // Otherwise, neither of the types are known right now
                        // (or more accurately, we know whether they are ints or floats
                        // but not their size), so the best we can do is to just bubble
                        // that compile time number type up to the operand
                        //
                        // First, make sure they are both compile time integers
                        // or both compile time floats
                        if left_type == right_type {
                            return left_type;
                        } else {
                            // Otherwise, you can't do arithmetic on a float and an integer
                            self.error.report(
                                ErrorType::IncompatibleTypesError,
                                vec![ErrorMessage::new(
                                    Some(format!(
                                        "{} can not be {} {}",
                                        if is_compile_time_int(&left_type) {
                                            "Integers"
                                        } else {
                                            "Floating-point numbers"
                                        },
                                        match node.operator {
                                            BinaryOperator::Plus => "added to",
                                            BinaryOperator::Minus => "subtracted by",
                                            BinaryOperator::Times => "multiplied by",
                                            BinaryOperator::Divide | BinaryOperator::Modulus => "divided by",
                                            _ => "PANIC Should have already ensured operator was arithmetic",
                                        },
                                        if is_compile_time_int(&right_type) {
                                            "integers"
                                        } else {
                                            "floating-point numbers"
                                        },
                                    )),
                                    &node.get_token().location_info,
                                )],
                                Some(String::from("Consider casting the result of one expression into the type of the other")),
                            );
                            return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                        }
                    }
                } else {
                    let left = self.ast.get_node(left_pointer).clone();
                    let right = self.ast.get_node(right_pointer).clone();
                    self.incompatible_type_error_two_operands(
                        &left,
                        &right,
                        &PrimitiveClass::Number,
                        &node,
                    );
                    return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                }
            } else if binary_is_boolean(node) {
                // Both operands must be bools, result is bool
                if is_bool(&left_type) && is_bool(&right_type) {
                    return DataType::new(
                        Type::Primitive(Primitive::Bool),
                        Type::Primitive(Primitive::Bool).get_label(),
                    );
                } else {
                    let left = self.ast.get_node(left_pointer).clone();
                    let right = self.ast.get_node(right_pointer).clone();
                    self.incompatible_type_error_two_operands(
                        &left,
                        &right,
                        &PrimitiveClass::Bool,
                        &node,
                    );
                    return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                }
            } else if binary_is_comparison(node) {
                // Operands must be numbers (no need to implicitly cast), result is bool
                if is_number(&left_type) && is_number(&right_type) {
                    if left_type != right_type {
                        self.error.report(
                            ErrorType::IncompatibleTypesError,
                            vec![
                                ErrorMessage::new(
                                    Some(format!(
                                        "Expressions with types '{}' and '{}' are incompatible",
                                        left_type.get_label(),
                                        right_type.get_label()
                                    )),
                                    &node.get_token().location_info,
                                ),
                                ErrorMessage::new(
                                    Some(format!("'{}'", left_type.get_label())),
                                    &self.ast.get_node(left_pointer).get_token().location_info,
                                ),
                                ErrorMessage::new(
                                    Some(format!("'{}'", right_type.get_label())),
                                    &self.ast.get_node(right_pointer).get_token().location_info,
                                ),
                            ],
                            None,
                        );
                        return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                    } else {
                        return DataType::new(
                            Type::Primitive(Primitive::Bool),
                            Type::Primitive(Primitive::Bool).get_label(),
                        );
                    }
                } else {
                    let left = self.ast.get_node(left_pointer).clone();
                    let right = self.ast.get_node(right_pointer).clone();
                    self.incompatible_type_error_two_operands(
                        &left,
                        &right,
                        &PrimitiveClass::Number,
                        &node,
                    );
                    return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                }
            } else if binary_is_equality(node) {
                // Operands can be anything, result is bool
                if left_type != right_type {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                Some(format!(
                                    "Expressions with types '{}' and '{}' are incompatible",
                                    left_type.get_label(),
                                    right_type.get_label()
                                )),
                                &node.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                Some(format!("'{}'", left_type.get_label())),
                                &self.ast.get_node(left_pointer).get_token().location_info,
                            ),
                            ErrorMessage::new(
                                Some(format!("'{}'", right_type.get_label())),
                                &self.ast.get_node(right_pointer).get_token().location_info,
                            ),
                        ],
                        None,
                    );
                    return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                } else {
                    return DataType::new(
                        Type::Primitive(Primitive::Bool),
                        Type::Primitive(Primitive::Bool).get_label(),
                    );
                }
            } else {
                return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
            }
        }
    }

    pub fn type_check_unary(&mut self, node: &mut UnaryExprNode) -> DataType {
        // Get the type of the node's operand
        let operand_pointer = node.operand;
        let operand_type = self.ast.get_node(node.operand).get_type();

        // If either of the operands is untyped (meaning we've thrown a type error)
        // just leave this node untyped as well
        // to try and capture as many errors as possible before quitting
        if is_untyped(&operand_type) {
            return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
        } else {
            match node.operator {
                UnaryOperator::Minus => {
                    // Operand must be a number, result is a number
                    if is_number(&operand_type) {
                        // If the operand is an unsigned int, it can't be negated
                        if is_unsigned(&operand_type) {
                            self.error.report(
                                ErrorType::IncompatibleTypesError,
                                vec![
                                    ErrorMessage::new(
                                        Some(format!(
                                            "Expression with unsigned type '{}' cannot be negated",
                                            operand_type.get_label()
                                        )),
                                        &node.get_token().location_info,
                                    ),
                                    ErrorMessage::new(
                                        Some(format!("'{}'", operand_type.get_label())),
                                        &self
                                            .ast
                                            .get_node(operand_pointer)
                                            .get_token()
                                            .location_info,
                                    ),
                                ],
                                None,
                            );
                            return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                        } else {
                            // Return a copy of the operand's type
                            return operand_type;
                        }
                    } else {
                        // Throw an incompatible type error
                        self.error.report(
                            ErrorType::IncompatibleTypesError,
                            vec![
                                ErrorMessage::new(
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        operand_type.get_label(),
                                        node.get_operator()
                                    )),
                                    &node.get_token().location_info,
                                ),
                                ErrorMessage::new(
                                    Some(format!("'{}'", operand_type.get_label())),
                                    &self.ast.get_node(operand_pointer).get_token().location_info,
                                ),
                            ],
                            None,
                        );

                        return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                    }
                }
                UnaryOperator::Not => {
                    // Operand must be a bool, result is a bool
                    if is_bool(&operand_type) {
                        // Infer the type of the operator
                        return operand_type.clone();
                    } else {
                        // Throw an incompatible type error
                        self.error.report(
                            ErrorType::IncompatibleTypesError,
                            vec![
                                ErrorMessage::new(
                                    Some(format!(
                                    "Expression with type '{}' is incompatible with operator '{}'",
                                    operand_type.get_label(),
                                    node.get_operator()
                                )),
                                    &node.get_token().location_info,
                                ),
                                ErrorMessage::new(
                                    Some(format!("'{}'", operand_type.get_label())),
                                    &self.ast.get_node(operand_pointer).get_token().location_info,
                                ),
                            ],
                            None,
                        );

                        return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
                    }
                }
            }
        }
    }

    // Try casting in both directions and do so if possible
    pub fn dual_directional_cast(&mut self, left: NodePointer, right: NodePointer) -> bool {
        let left_type = self.ast.get_node(left).get_type();
        let right_type = self.ast.get_node(right).get_type();

        // Check if we can cast the left node into the type of the right node
        match self.can_implicitly_cast(&left_type, &right_type) {
            // If we can, cast the left node into the type of the right node
            CanCast::Yes => {
                self.implicit_cast(left, right);
                return true;
            }
            // If we can't, check if we can cast the right node into the type of the left node
            CanCast::No(_) => match self.can_implicitly_cast(&right_type, &left_type) {
                // If we can, cast the right node into the type of the left node
                CanCast::Yes => {
                    self.implicit_cast(right, left);
                    return true;
                }
                // If we can't, throw an incompatible types error
                CanCast::No(_) => {
                    let error_type = ErrorType::IncompatibleTypesError;
                    self.error.report(
                        error_type,
                        vec![
                            ErrorMessage::new(
                                Some(format!(
                                    "{} '{}' and '{}'",
                                    error_type.description(),
                                    left_type.get_label(),
                                    right_type.get_label(),
                                )),
                                &self.ast.get_node(left).get_token().location_info,
                            ),
                            ErrorMessage::new(
                                None,
                                &self.ast.get_node(right).get_token().location_info,
                            ),
                        ],
                        None,
                    );

                    return false;
                }
            },
        }
    }

    // Test if we can cast one type into another
    pub fn can_implicitly_cast(&self, old_type: &DataType, new_type: &DataType) -> CanCast {
        // We can't implicitly cast a number into a non-number
        if is_number(&old_type) && !is_number(&new_type) {
            return CanCast::No(format!(
                "Cannot implicitly cast number of type '{}' into non-number of type '{}'",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast a non-number into a number
        else if !is_number(&old_type) && is_number(&new_type) {
            return CanCast::No(format!(
                "Cannot implicitly cast non-number of type '{}' into number of type '{}'",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast floats into ints
        else if is_float(&old_type) && is_int(&new_type) {
            return CanCast::No(format!(
                "Cannot implicitly cast number of type '{}' into number of type '{}'",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast signed into unsigned
        else if is_signed(&old_type) && is_unsigned(&new_type) {
            return CanCast::No(format!(
                "Cannot implicitly cast signed integer of type '{}' into unsigned integer of type '{}' due to the possibility of truncation or overflow",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast unsigned into signed
        else if is_unsigned(&old_type) && is_signed(&new_type) {
            return CanCast::No(format!(
                "Cannot implicitly cast unsigned integer of type '{}' into signed integer of type '{}' due to the possibility of truncation or overflow",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast a larger type into a smaller type
        else if old_type.size() > new_type.size() {
            return CanCast::No(format!(
                "Cannot implicitly cast larger type '{}' into smaller type '{}' due to the possibility of truncation",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't implicitly cast a non-number type into a different non-number type
        else if !is_number(&old_type) && !is_number(&new_type) && old_type != new_type {
            return CanCast::No(format!(
                "Cannot implicitly cast value of type '{}' into value of type '{}'",
                old_type.get_label(),
                new_type.get_label(),
            ));
        }
        // We can't cast i64 or u64 into f64
        else {
            match old_type.data_type {
                Type::Primitive(old_primitive) => match old_primitive {
                    Primitive::I64 | Primitive::U64 => match new_type.data_type {
                        Type::Primitive(new_primitive) => match new_primitive {
                            Primitive::F64 => {
                                return CanCast::No(format!(
                                    "Cannot implicitly cast value of type '{}' into value of type '{}'",
                                    old_type.get_label(),
                                    new_type.get_label(),
                                ));
                            }
                            // We can cast!!
                            _ => return CanCast::Yes,
                        },
                        // We can cast!!
                        _ => return CanCast::Yes,
                    },
                    // We can cast!!
                    _ => return CanCast::Yes,
                },
                // We can cast!!
                _ => return CanCast::Yes,
            }
        }
    }

    // Cast node to type
    pub fn implicit_cast(&mut self, old_type_node: NodePointer, new_type_node: NodePointer) {
        // Get the new type we want to cast the old_type_node to
        let new_type = self.ast.get_node(new_type_node).get_type();

        // Replace the type of the old_type_node with a copy of the new type
        self.infer_type(old_type_node, new_type);
    }

    pub fn infer_type(&mut self, node_pointer: NodePointer, new_type: DataType) {
        let node = self.ast.get_mut_node(node_pointer);

        self.logger.log(&format!(
            "Inferring type of {} to be '{}'",
            node.node_to_string(),
            new_type.get_label()
        ));

        node.set_type(new_type.clone());

        // If this node evaluates to a compile-time number,
        // we may need to infer the types of its children as well
        if is_compile_time(&node.get_type()) {
            match node {
                ASTNode::ExpressionNode(expr_node) => {
                    match expr_node {
                        // Leaf node, no children to infer the type of
                        ExpressionNode::Literal(_) => {}
                        ExpressionNode::Binary(binary_node) => {
                            // Infer the type of both children
                            let left_pointer = binary_node.left;
                            let right_pointer = binary_node.right;
                            self.infer_type(left_pointer, new_type.clone());
                            self.infer_type(right_pointer, new_type);
                        }
                        ExpressionNode::Unary(unary_node) => {
                            // Infer the type of child
                            let operand_pointer = unary_node.operand;
                            self.infer_type(operand_pointer, new_type);
                        }
                    }
                }
                ASTNode::RootNode(_) => panic!("Trying to infer type of RootNode"),
                ASTNode::NotANode(_) => panic!("Trying to infer type of NotANode"),
            }
        }
        /*
        // If this is a literal node, we may need to update its value
        match node {
            ASTNode::ExpressionNode(expr_node) => {
                match expr_node {
                    ExpressionNode::Literal(literal_node) => {
                        match literal_node.value {
                            Literal::Int(int_val) => {
                                // If our new type is float
                                let new_type = node.get_type();
                                if is_float(&new_type) {
                                    self.logger.log(&format!(
                                        "Updating value of {}:",
                                        node.node_to_string()
                                    ));
                                    *node = ASTNode::ExpressionNode(ExpressionNode::Literal(
                                        LiteralNode::new(
                                            Literal::Float(int_val as f64),
                                            node.get_token().clone(),
                                        ),
                                    ));
                                    node.set_type(new_type);
                                    self.logger.log(&format!("\t{}", node.node_to_string()));
                                }
                            }
                            Literal::Float(float_val) => {
                                // Check our new type
                                let new_type = node.get_type();
                                if is_int(&new_type) {
                                    self.logger.log(&format!(
                                        "Updating value of {}:",
                                        node.node_to_string()
                                    ));
                                    *node = ASTNode::ExpressionNode(ExpressionNode::Literal(
                                        LiteralNode::new(
                                            Literal::Int(float_val as i128),
                                            node.get_token().clone(),
                                        ),
                                    ));
                                    node.set_type(new_type);
                                    self.logger.log(&format!("\t{}", node.node_to_string()));
                                }
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
        */
    }

    pub fn incompatible_type_error_two_operands(
        &mut self,
        left: &ASTNode,
        right: &ASTNode,
        expected_type: &PrimitiveClass,
        operator: &BinaryExprNode,
    ) {
        let left_type = left.get_type();
        let right_type = right.get_type();

        match expected_type {
            // If the operator expects numbers
            PrimitiveClass::Number => {
                // If the left type is a number but the right type isn't
                if is_number(&left_type) && !is_number(&right_type) {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!(
                                        "{} is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                        ],
                        None,
                    );
                }
                // If the left type isn't a number but the right type is
                else if !is_number(&left_type) && is_number(&right_type) {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!(
                                        "{} is incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                        ],
                        None,
                    );
                }
                // If neither type is a number but they are the same
                else if left_type == right_type {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!(
                                        "{}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expressions with type '{}' are incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &right.get_token().location_info,
                            )
                        ],
                        None,
                    );
                }
                // If neither type is a number and they are different
                else {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) && is_compile_time(&right_type) {
                                    Some(format!(
                                        "{}s and {}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else if is_compile_time(&left_type) && !is_compile_time(&right_type) {
                                    Some(format!(
                                        "{}s and expressions of type '{}' are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else if !is_compile_time(&left_type) && is_compile_time(&right_type) {
                                    Some(format!(
                                        "Expressions of type '{}' and {}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &right.get_token().location_info,
                            )
                        ],
                        None,
                    );
                }
            }
            // If the operator expects bools
            PrimitiveClass::Bool => {
                // If the left type is a bool but the right type isn't
                if is_bool(&left_type) && !is_bool(&right_type) {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!(
                                        "{} is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &right.get_token().location_info,
                            ),
                        ],
                        None,
                    );
                }
                // If the left type isn't a bool but the right type is
                else if !is_bool(&left_type) && is_bool(&right_type) {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!(
                                        "{} is incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expression with type '{}' is incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                        ],
                        None,
                    );
                }
                // If neither type is a bool but they are the same
                else if left_type == right_type {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!(
                                        "{}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                    Some(format!(
                                        "Expressions with type '{}' are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &right.get_token().location_info,
                            )
                        ],
                        None,
                    );
                }
                // If neither type is a bool and they are different
                else {
                    self.error.report(
                        ErrorType::IncompatibleTypesError,
                        vec![
                            ErrorMessage::new(
                                if is_compile_time(&left_type) && !is_compile_time(&right_type) {
                                    Some(format!(
                                        "{}s and expressions of type '{}' are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else if !is_compile_time(&left_type) && is_compile_time(&right_type) {
                                    Some(format!(
                                        "Expressions of type '{}' and {}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else if is_compile_time(&left_type) && is_compile_time(&right_type) {
                                    Some(format!(
                                        "{}s and {}s are incompatible with operator '{}'",
                                        left_type.get_label(),
                                        right_type.get_label(),
                                        &operator.get_operator()
                                    ))
                                } else {
                                Some(format!(
                                    "Expressions with types '{}' and '{}' are incompatible with operator '{}'",
                                    left_type.get_label(),
                                    right_type.get_label(),
                                    &operator.get_operator()
                                ))
                                },
                                &operator.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&left_type) {
                                    Some(format!("{}", left_type.get_label()))
                                } else {
                                    Some(format!("'{}'", left_type.get_label()))
                                },
                                &left.get_token().location_info,
                            ),
                            ErrorMessage::new(
                                if is_compile_time(&right_type) {
                                    Some(format!("{}", right_type.get_label()))
                                } else {
                                    Some(format!("'{}'", right_type.get_label()))
                                },
                                &right.get_token().location_info,
                            )
                        ],
                        None,
                    );
                }
            }
            // If the operator expects strings
            PrimitiveClass::String => {}
        }
    }
}

pub fn binary_is_arithmetic(node: &BinaryExprNode) -> bool {
    match node.operator {
        BinaryOperator::Plus
        | BinaryOperator::Minus
        | BinaryOperator::Times
        | BinaryOperator::Divide
        | BinaryOperator::Modulus => return true,
        _ => return false,
    }
}

pub fn binary_is_comparison(node: &BinaryExprNode) -> bool {
    match node.operator {
        BinaryOperator::LessThan
        | BinaryOperator::LessThanOrEqual
        | BinaryOperator::GreaterThan
        | BinaryOperator::GreaterThanOrEqual => return true,
        _ => return false,
    }
}

pub fn binary_is_boolean(node: &BinaryExprNode) -> bool {
    match node.operator {
        BinaryOperator::And | BinaryOperator::Or => return true,
        _ => return false,
    }
}

pub fn binary_is_equality(node: &BinaryExprNode) -> bool {
    match node.operator {
        BinaryOperator::Equal | BinaryOperator::NotEqual => return true,
        _ => return false,
    }
}

pub fn unary_is_arithmetic(node: &UnaryExprNode) -> bool {
    match node.operator {
        UnaryOperator::Minus => return true,
        _ => return false,
    }
}

pub fn unary_is_boolean(node: &UnaryExprNode) -> bool {
    match node.operator {
        UnaryOperator::Not => return true,
        _ => return false,
    }
}

pub fn is_number(data_type: &DataType) -> bool {
    return is_int(data_type) || is_float(&data_type);
}

pub fn is_int(data_type: &DataType) -> bool {
    return is_signed(data_type) || is_unsigned(data_type) || is_compile_time_int(data_type);
}

pub fn is_compile_time(data_type: &DataType) -> bool {
    return is_compile_time_int(data_type) || is_compile_time_float(data_type);
}

pub fn is_compile_time_int(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::CompileTimeInt => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_compile_time_float(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::CompileTimeFloat => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_i8(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I8 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_i16(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I16 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_i32(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I32 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_i64(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I64 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_i128(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I128 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_u8(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U8 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_u16(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U16 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_u32(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U32 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_u64(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U64 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_u128(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U128 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_f32(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::F32 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_f64(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::F64 => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_float(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::F32 | Primitive::F64 | Primitive::CompileTimeFloat => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_bool(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::Bool => return true,
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_signed(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::I8 | Primitive::I16 | Primitive::I32 | Primitive::I64 | Primitive::I128 => {
                return true;
            }
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_unsigned(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::Primitive(data_type) => match data_type {
            Primitive::U8 | Primitive::U16 | Primitive::U32 | Primitive::U64 | Primitive::U128 => {
                return true;
            }
            _ => return false,
        },
        _ => return false,
    }
}

pub fn is_untyped(data_type: &DataType) -> bool {
    match data_type.data_type {
        Type::UnTyped => return true,
        _ => return false,
    }
}
