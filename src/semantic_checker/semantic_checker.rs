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
    traverse, ASTNode, BinaryExprNode, BinaryOperator, ExpressionNode, Literal, LiteralNode,
    NodePointer, StatementNode, Traversal, UnaryExprNode, UnaryOperator, VariableAssignmentNode,
    VariableDeclarationNode, AST,
};
use crate::semantic_checker::semantic_checker_data::{DataType, Type};

use super::semantic_checker_data::{
    Callback, GlobalDeclarations, IdentifiersPost, IdentifiersPre, MiscellaneousChecks, Primitive,
    PrimitiveClass, Symbol, SymbolTable, TypeChecking, VariableSymbol,
};

pub struct SemanticChecker<'semantic> {
    pub ast: &'semantic mut AST,
    pub symbol_table: &'semantic mut SymbolTable,
    pub logger: &'semantic mut Logger,
    pub error: &'semantic mut ErrorReporter,
}

impl GlobalDeclarations {
    pub fn run(
        &mut self,
        _node: NodePointer,
        _ast: &mut AST,
        _symbol_table: &mut SymbolTable,
        _logger: &mut Logger,
        _error: &mut ErrorReporter,
    ) {
    }
}

impl IdentifiersPre {
    pub fn run(
        &mut self,
        _node: NodePointer,
        _ast: &mut AST,
        _symbol_table: &mut SymbolTable,
        _logger: &mut Logger,
        _error: &mut ErrorReporter,
    ) {
    }
}

impl IdentifiersPost {
    pub fn run(
        &mut self,
        node: NodePointer,
        ast: &mut AST,
        symbol_table: &mut SymbolTable,
        logger: &mut Logger,
        error: &mut ErrorReporter,
    ) {
        match ast.get_mut_node(node) {
            ASTNode::StatementNode(stmt_node) => match stmt_node {
                StatementNode::VariableDeclaration(var_node) => {
                    logger.log(&format!("Found variable declaration {}", &var_node.name));

                    let symbol_pointer = symbol_table.new_symbol(
                        var_node.name.clone(),
                        Symbol::Variable(VariableSymbol::new(
                            var_node.name.clone(),
                            var_node.mutable,
                        )),
                    );
                    var_node.symbol = Some(symbol_pointer);
                }
                StatementNode::VariableAssignment(assmt_node) => {
                    logger.log(&format!("Found variable assignment {}", &assmt_node.name));

                    // See if this variable has been defined before
                    match symbol_table.find_symbol(&assmt_node.name) {
                        Some(symbol) => {
                            logger.log(&format!("Variable {} has a declaration", &assmt_node.name));
                            assmt_node.symbol = Some(symbol.to_owned());
                            match &mut *symbol.borrow_mut() {
                                // We can only assign to a mutable variable
                                Symbol::Variable(var_symbol) => {
                                    if !var_symbol.mutable {
                                        error.report(
                                            ErrorType::MultipleAssignmentError,
                                            vec![ErrorMessage::new(
                                                Some(String::from("Variable cannot be mutated")),
                                                &assmt_node.token.location_info,
                                            )],
                                            Some(format!("Consider declaring this variable as mutable: \"let mut {}\"", assmt_node.name)),
                                        )
                                    }
                                }
                            }
                        }
                        None => {
                            logger.log(&format!(
                                "Variable {} has a does NOT have a declaration",
                                &assmt_node.name
                            ));
                            error.report(
                                ErrorType::UnrecognizedIdentifierError,
                                vec![ErrorMessage::new(
                                    Some(format!(
                                        "Cannot find identifier \"{}\"",
                                        &assmt_node.name
                                    )),
                                    &assmt_node.token.location_info,
                                )],
                                None,
                            );
                        }
                    }
                }
            },
            ASTNode::ExpressionNode(expr_node) => match expr_node {
                ExpressionNode::Variable(var_node) => {
                    logger.log(&format!("Found variable {}", &var_node.name));

                    // See if this variable has been defined before
                    match symbol_table.find_symbol(&var_node.name) {
                        Some(symbol) => {
                            logger.log(&format!("Variable {} has a declaration", &var_node.name));
                            var_node.symbol = Some(symbol.to_owned());
                        }
                        None => {
                            logger.log(&format!(
                                "Variable {} has a does NOT have a declaration",
                                &var_node.name
                            ));
                            error.report(
                                ErrorType::UnrecognizedIdentifierError,
                                vec![ErrorMessage::new(
                                    Some(format!("Cannot find identifier \"{}\"", &var_node.name)),
                                    &var_node.token.location_info,
                                )],
                                None,
                            );
                        }
                    }
                }
                _ => {}
            },
            _ => {}
        }
    }
}

impl TypeChecking {
    pub fn run(
        &mut self,
        node: NodePointer,
        ast: &mut AST,
        symbol_table: &mut SymbolTable,
        logger: &mut Logger,
        error: &mut ErrorReporter,
    ) {
        // Now we can type check
        logger.log(&format!(
            "Type checking node {}",
            ast.get_node(node).node_to_string()
        ));

        let node_clone = ast.get_mut_node(node).clone();
        match node_clone {
            ASTNode::ExpressionNode(expr) => match expr {
                ExpressionNode::Literal(mut literal_node) => {
                    let new_type = type_check_literal(&mut literal_node);
                    infer_type(node, new_type, ast, logger);
                }
                ExpressionNode::Binary(mut binary_node) => {
                    let new_type = type_check_binary(&mut binary_node, ast, error, logger);
                    infer_type(node, new_type, ast, logger);
                }
                ExpressionNode::Unary(mut unary_node) => {
                    let new_type = type_check_unary(&mut unary_node, ast, error);
                    infer_type(node, new_type, ast, logger);
                }
                // Variable expression nodes already have a type in their symbol table entry
                // that was set when type checking the variable declaration nodes
                ExpressionNode::Variable(_) => {}
            },
            ASTNode::StatementNode(node) => match node {
                StatementNode::VariableDeclaration(mut var_node) => {
                    type_check_variable_declaration(&mut var_node, ast, symbol_table, logger);
                }
                StatementNode::VariableAssignment(mut assmt_node) => {
                    type_check_variable_assignment(
                        &mut assmt_node,
                        ast,
                        symbol_table,
                        logger,
                        error,
                    );
                }
            },
            // Already type checked any children
            ASTNode::RootNode(_) => {}
            // No need to type check, already UnTyped
            ASTNode::NotANode(_) => {}
            ASTNode::TypeHintNode(_) => {}
            ASTNode::IdentifierNode(_) => {}
        }
    }
}

impl MiscellaneousChecks {
    pub fn run(
        &mut self,
        _node: NodePointer,
        _ast: &mut AST,
        _symbol_table: &mut SymbolTable,
        _logger: &mut Logger,
        _error: &mut ErrorReporter,
    ) {
    }
}

impl SemanticChecker<'_> {
    pub fn new<'semantic>(
        ast: &'semantic mut AST,
        symbol_table: &'semantic mut SymbolTable,
        logger: &'semantic mut Logger,
        error: &'semantic mut ErrorReporter,
    ) -> SemanticChecker<'semantic> {
        return SemanticChecker {
            ast,
            symbol_table,
            logger,
            error,
        };
    }

    pub fn check(&mut self) {
        self.logger.log("");
        self.logger.log("-----------------------");
        self.logger.log("BEGIN Semantic Checking");
        self.logger.log("-----------------------");

        for new_child in self.ast.get_new_children() {
            // First pass: Global declarations
            self.logger.log("FIRST PASS: GLOBAL DECLARATIONS");
            let mut global_declarations = Callback::GlobalDeclarations(GlobalDeclarations::new());
            traverse(
                &mut Traversal::Preorder(&mut global_declarations),
                new_child,
                self.ast,
                self.symbol_table,
                self.logger,
                self.error,
            );

            // Second pass: Identifiers
            self.logger.log("SECOND PASS: IDENTIFIERS");
            let mut identifiers_pre = Callback::IdentifiersPre(IdentifiersPre::new());
            let mut identifiers_post = Callback::IdentifiersPost(IdentifiersPost::new());
            traverse(
                &mut Traversal::PrePostorder(&mut identifiers_pre, &mut identifiers_post),
                new_child,
                self.ast,
                self.symbol_table,
                self.logger,
                self.error,
            );

            // Third pass: Type checking
            self.logger.log("THIRD PASS: TYPE CHECKING");
            let mut type_checking = Callback::TypeChecking(TypeChecking::new());
            traverse(
                &mut Traversal::Postorder(&mut type_checking),
                new_child,
                self.ast,
                self.symbol_table,
                self.logger,
                self.error,
            );
            // self.type_check(self.ast.root_node);

            // Fourth pass: Miscellaneous checks
            self.logger.log("FOURTH PASS: MISCELLANEOUS CHECKS");

            self.logger.log("\nAST after semantic checking:");
            self.logger.log(&format!(
                "{}",
                self.ast.get_root_node().to_string(&self.ast)
            ));
        }
    }
}

pub fn type_check_literal(node: &mut LiteralNode) -> DataType {
    // Figure out what kind of literal node this is, and set its type for it
    match node.value {
        Literal::Int(_) => {
            let data_type = Type::Primitive(Primitive::CompileTimeInt);
            return DataType::new(data_type);
        }
        Literal::Float(_) => {
            let data_type = Type::Primitive(Primitive::CompileTimeFloat);
            return DataType::new(data_type);
        }
        Literal::True(_) | Literal::False(_) => {
            let data_type = Type::Primitive(Primitive::Bool);
            return DataType::new(data_type);
        }
        Literal::String(_) => {
            let data_type = Type::Primitive(Primitive::String);
            return DataType::new(data_type);
        }
    }
}

pub fn type_check_binary(
    node: &mut BinaryExprNode,
    ast: &mut AST,
    error: &mut ErrorReporter,
    logger: &mut Logger,
) -> DataType {
    // Get the type of the node's children
    let left_pointer = node.left;
    let left_type = ast.get_node(node.left).get_type();
    let right_pointer = node.right;
    let right_type = ast.get_node(node.right).get_type();

    // If either of the operands is untyped (meaning we've thrown a type error)
    // just leave this node untyped as well
    // to try and capture as many errors as possible before quitting
    if is_untyped(&left_type) || is_untyped(&right_type) {
        return DataType::new(Type::UnTyped);
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
                        error.report(
                            ErrorType::IncompatibleTypesError,
                            vec![ErrorMessage::new(
                                Some(format!(
                                    "Expressions of types '{}' and '{}' are incompatible",
                                    left_type.get_label(),
                                    right_type.get_label()
                                )),
                                &node.get_token().location_info,
                            )],
                            Some(if left_type.size() > right_type.size() {
                                format!(
                                    "Consider casting result of right expression into type '{}'",
                                    left_type.get_label()
                                )
                            } else if right_type.size() > left_type.size() {
                                format!(
                                    "Consider casting result of left expression into type '{}'",
                                    right_type.get_label()
                                )
                            } else {
                                format!("Consider casting one type into the other")
                            }),
                        );
                        return DataType::new(Type::UnTyped);
                    }
                } else if !is_compile_time(&left_type) && is_compile_time(&right_type) {
                    // We know the left type but not the right type,
                    // so infer both the right type and the operator type to be the left type
                    infer_type(right_pointer, left_type.clone(), ast, logger);
                    return left_type.clone();
                } else if is_compile_time(&left_type) && !is_compile_time(&right_type) {
                    // We know the right type but not the left type,
                    // so infer both the left type and the operator type
                    // to be the right type
                    infer_type(left_pointer, right_type.clone(), ast, logger);
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
                        error.report(
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
                        return DataType::new(Type::UnTyped);
                    }
                }
            } else {
                let left = ast.get_node(left_pointer).clone();
                let right = ast.get_node(right_pointer).clone();
                incompatible_type_error_two_operands(
                    &left,
                    &right,
                    &PrimitiveClass::Number,
                    &node,
                    error,
                );
                return DataType::new(Type::UnTyped);
            }
        } else if binary_is_boolean(node) {
            // Both operands must be bools, result is bool
            if is_bool(&left_type) && is_bool(&right_type) {
                return DataType::new(Type::Primitive(Primitive::Bool));
            } else {
                let left = ast.get_node(left_pointer).clone();
                let right = ast.get_node(right_pointer).clone();
                incompatible_type_error_two_operands(
                    &left,
                    &right,
                    &PrimitiveClass::Bool,
                    &node,
                    error,
                );
                return DataType::new(Type::UnTyped);
            }
        } else if binary_is_comparison(node) {
            // Operands must be numbers (no need to implicitly cast), result is bool
            if is_number(&left_type) && is_number(&right_type) {
                if left_type != right_type {
                    error.report(
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
                                &ast.get_node(left_pointer).get_token().location_info,
                            ),
                            ErrorMessage::new(
                                Some(format!("'{}'", right_type.get_label())),
                                &ast.get_node(right_pointer).get_token().location_info,
                            ),
                        ],
                        None,
                    );
                    return DataType::new(Type::UnTyped);
                } else {
                    return DataType::new(Type::Primitive(Primitive::Bool));
                }
            } else {
                let left = ast.get_node(left_pointer).clone();
                let right = ast.get_node(right_pointer).clone();
                incompatible_type_error_two_operands(
                    &left,
                    &right,
                    &PrimitiveClass::Number,
                    &node,
                    error,
                );
                return DataType::new(Type::UnTyped);
            }
        } else if binary_is_equality(node) {
            // Operands can be anything, result is bool
            if left_type != right_type {
                error.report(
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
                            &ast.get_node(left_pointer).get_token().location_info,
                        ),
                        ErrorMessage::new(
                            Some(format!("'{}'", right_type.get_label())),
                            &ast.get_node(right_pointer).get_token().location_info,
                        ),
                    ],
                    None,
                );
                return DataType::new(Type::UnTyped);
            } else {
                return DataType::new(Type::Primitive(Primitive::Bool));
            }
        } else {
            return DataType::new(Type::UnTyped);
        }
    }
}

pub fn type_check_unary(
    node: &mut UnaryExprNode,
    ast: &mut AST,
    error: &mut ErrorReporter,
) -> DataType {
    // Get the type of the node's operand
    let operand_pointer = node.operand;
    let operand_type = ast.get_node(node.operand).get_type();

    // If either of the operands is untyped (meaning we've thrown a type error)
    // just leave this node untyped as well
    // to try and capture as many errors as possible before quitting
    if is_untyped(&operand_type) {
        return DataType::new(Type::UnTyped);
    } else {
        match node.operator {
            UnaryOperator::Minus => {
                // Operand must be a number, result is a number
                if is_number(&operand_type) {
                    // If the operand is an unsigned int, it can't be negated
                    if is_unsigned(&operand_type) {
                        error.report(
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
                                    &ast.get_node(operand_pointer).get_token().location_info,
                                ),
                            ],
                            None,
                        );
                        return DataType::new(Type::UnTyped);
                    } else {
                        // Return a copy of the operand's type
                        return operand_type;
                    }
                } else {
                    // Throw an incompatible type error
                    error.report(
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
                                &ast.get_node(operand_pointer).get_token().location_info,
                            ),
                        ],
                        None,
                    );

                    return DataType::new(Type::UnTyped);
                }
            }
            UnaryOperator::Not => {
                // Operand must be a bool, result is a bool
                if is_bool(&operand_type) {
                    // Infer the type of the operator
                    return operand_type.clone();
                } else {
                    // Throw an incompatible type error
                    error.report(
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
                                &ast.get_node(operand_pointer).get_token().location_info,
                            ),
                        ],
                        None,
                    );

                    return DataType::new(Type::UnTyped);
                }
            }
        }
    }
}

pub fn type_check_variable_declaration(
    node: &mut VariableDeclarationNode,
    ast: &mut AST,
    symbol_table: &mut SymbolTable,
    logger: &mut Logger,
) {
    let expression_type = ast.get_node(node.expression).get_type();

    // If this assignment has a type hint,
    // make sure the expression has the same type
    match node.type_hint {
        Some(type_hint_pointer) => {
            let hint_type = ast.get_node(type_hint_pointer).get_type();
            if hint_type != expression_type {
                // If the types aren't equal,
                // the expression could still have a compile-time type
                if is_compile_time(&expression_type) {
                    if (is_int(&hint_type) && is_compile_time_int(&expression_type))
                        || (is_float(&hint_type) && is_compile_time_float(&expression_type))
                    {
                        // update the expression type
                        infer_type(node.expression, hint_type.clone(), ast, logger);
                    }
                }
            }

            // Infer the type of the variable
            match symbol_table.find_symbol(&node.name) {
                None => panic!("Should have already handled case that variable cannot be found"),
                Some(symbol) => match &mut *symbol.borrow_mut() {
                    Symbol::Variable(var_symbol) => var_symbol.data_type = hint_type,
                },
            }
        }
        None => {
            // Infer the type of the variable
            match symbol_table.find_symbol(&node.name) {
                None => panic!("Should have already handled case that variable cannot be found"),
                Some(symbol) => match &mut *symbol.borrow_mut() {
                    Symbol::Variable(var_symbol) => var_symbol.data_type = expression_type,
                },
            }
        }
    }
}

pub fn type_check_variable_assignment(
    node: &mut VariableAssignmentNode,
    ast: &mut AST,
    symbol_table: &mut SymbolTable,
    logger: &mut Logger,
    error: &mut ErrorReporter,
) {
    let expression_type = ast.get_node(node.expression).get_type();

    // Make sure the expression has the same type as the variable
    match symbol_table.find_symbol(&node.name) {
        None => panic!("Should have already handled case that variable cannot be found"),
        Some(symbol) => match &mut *symbol.borrow_mut() {
            Symbol::Variable(var_symbol) => {
                if var_symbol.data_type != expression_type {
                    // If the types aren't equal, the expression could still have a compile-time type
                    if is_compile_time(&expression_type) {
                        if (is_int(&var_symbol.data_type) && is_compile_time_int(&expression_type))
                            || (is_float(&var_symbol.data_type)
                                && is_compile_time_float(&expression_type))
                        {
                            // Update the expression type
                            infer_type(node.expression, var_symbol.data_type.clone(), ast, logger);
                        }
                    } else {
                        // We have an incompatible type error
                        error.report(
                            ErrorType::IncompatibleTypesError,
                            vec![ErrorMessage::new(
                                Some(format!(
                                    "Expected expression of type {}",
                                    var_symbol.data_type.type_label
                                )),
                                &ast.get_node(node.expression).get_token().location_info,
                            )],
                            None,
                        );
                    }
                }
            }
        },
    }
}

pub fn infer_type(
    node_pointer: NodePointer,
    new_type: DataType,
    ast: &mut AST,
    logger: &mut Logger,
) {
    let node = ast.get_mut_node(node_pointer);

    logger.log(&format!(
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
                        infer_type(left_pointer, new_type.clone(), ast, logger);
                        infer_type(right_pointer, new_type, ast, logger);
                    }
                    ExpressionNode::Unary(unary_node) => {
                        // Infer the type of child
                        let operand_pointer = unary_node.operand;
                        infer_type(operand_pointer, new_type, ast, logger);
                    }
                    // Variable expression nodes already have types in their symbol table entries,
                    // as set by type checking the declaration node
                    ExpressionNode::Variable(_) => {}
                }
            }
            ASTNode::RootNode(_) => panic!("Trying to infer type of RootNode"),
            ASTNode::NotANode(_) => panic!("Trying to infer type of NotANode"),
            ASTNode::StatementNode(_) => panic!("Trying to infer type of StatementNode"),
            ASTNode::TypeHintNode(_) => panic!("Trying to infer type of TypeHintNode"),
            ASTNode::IdentifierNode(_) => panic!("Trying to infer type of IdentifierNode"),
        }
    }
}

pub fn incompatible_type_error_two_operands(
    left: &ASTNode,
    right: &ASTNode,
    expected_type: &PrimitiveClass,
    operator: &BinaryExprNode,
    error: &mut ErrorReporter,
) {
    let left_type = left.get_type();
    let right_type = right.get_type();

    match expected_type {
        // If the operator expects numbers
        PrimitiveClass::Number => {
            // If the left type is a number but the right type isn't
            if is_number(&left_type) && !is_number(&right_type) {
                error.report(
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
                error.report(
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
                error.report(
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
                error.report(
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
                error.report(
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
                error.report(
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
                error.report(
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
                error.report(
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
