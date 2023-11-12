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

use std::fmt::Debug;

use crate::{
    scanner::scanner_data::Token,
    semantic_checker::semantic_checker_data::{DataType, Type},
};

pub type NodePointer = usize;

#[derive(Debug, PartialEq)]
pub struct AST {
    nodes: Vec<ASTNode>,
}

impl AST {
    pub fn new() -> AST {
        return AST { nodes: vec![] };
    }

    pub fn get_node(&self, pointer: NodePointer) -> &ASTNode {
        return &self.nodes[pointer];
    }

    pub fn get_mut_node(&mut self, pointer: NodePointer) -> &mut ASTNode {
        return &mut self.nodes[pointer];
    }

    pub fn set_node(&mut self, pointer: NodePointer, node: ASTNode) {
        self.nodes[pointer] = node;
    }

    pub fn new_node(&mut self, node: ASTNode) -> NodePointer {
        self.nodes.push(node);
        return self.nodes.len() - 1;
    }

    pub fn len(&self) -> usize {
        return self.nodes.len();
    }
}

#[derive(Clone, PartialEq)]
pub enum ASTNode {
    RootNode(RootNode),
    ExpressionNode(ExpressionNode),
    NotANode(NotANode),
}

impl ASTNode {
    pub fn get_type(&self) -> DataType {
        match self {
            ASTNode::RootNode(node) => return node.get_type(),
            ASTNode::ExpressionNode(node) => return node.get_type(),
            ASTNode::NotANode(node) => return node.get_type(),
        }
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match self {
            ASTNode::RootNode(_) => panic!("Can not set data type for RootNode"),
            ASTNode::ExpressionNode(node) => node.set_type(data_type),
            ASTNode::NotANode(_) => panic!("Can not set data type for NotANode"),
        }
    }

    pub fn get_token(&self) -> &Token {
        match self {
            ASTNode::RootNode(_) => panic!("RootNode has no token"),
            ASTNode::ExpressionNode(node) => return node.get_token(),
            ASTNode::NotANode(_) => panic!("NotANode has no token"),
        }
    }

    pub fn to_string(&self, ast: &AST) -> String {
        match self {
            ASTNode::RootNode(node) => return node.to_string(ast),
            ASTNode::ExpressionNode(node) => return node.to_string(ast),
            ASTNode::NotANode(node) => return node.to_string(),
        }
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self {
            ASTNode::RootNode(node) => return node.to_string_with_level(level, ast),
            ASTNode::ExpressionNode(node) => return node.to_string_with_level(level, ast),
            ASTNode::NotANode(node) => return node.to_string_with_level(level),
        }
    }

    pub fn node_to_string(&self) -> String {
        match self {
            ASTNode::RootNode(node) => return node.node_to_string(),
            ASTNode::ExpressionNode(node) => return node.node_to_string(),
            ASTNode::NotANode(node) => return node.node_to_string(),
        }
    }
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNode::RootNode(_) => write!(f, "ASTNode::RootNode"),
            ASTNode::ExpressionNode(_) => write!(f, "ASTNode::ExpressionNode"),
            ASTNode::NotANode(_) => write!(f, "ASTNode::NotANode"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct RootNode {
    pub expression: NodePointer,
}

impl RootNode {
    pub fn new(expression: NodePointer) -> RootNode {
        RootNode { expression }
    }

    pub fn set_child(&mut self, new_child: NodePointer) {
        self.expression = new_child;
    }

    pub fn get_type(&self) -> DataType {
        return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
    }

    pub fn to_string(&self, ast: &AST) -> String {
        return self.to_string_with_level(0, ast);
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        return format!(
            "{}\n{}",
            self.node_to_string(),
            ast.get_node(self.expression)
                .to_string_with_level(level + 1, ast)
        );
    }

    pub fn node_to_string(&self) -> String {
        return String::from("{Root}");
    }
}

#[derive(Clone, PartialEq)]
pub struct LiteralNode {
    pub value: Literal,
    pub token: Token,
    pub data_type: DataType,
}

impl LiteralNode {
    pub fn new(value: Literal, token: Token) -> LiteralNode {
        return LiteralNode {
            value,
            token,
            data_type: DataType::new(Type::UnTyped, Type::UnTyped.get_label()),
        };
    }

    pub fn get_type(&self) -> DataType {
        return self.data_type.clone();
    }

    pub fn set_type(&mut self, data_type: DataType) {
        self.data_type = data_type;
    }

    pub fn get_token(&self) -> &Token {
        return &self.token;
    }

    pub fn to_string(&self) -> String {
        return self.to_string_with_level(0);
    }

    pub fn to_string_with_level(&self, level: usize) -> String {
        return format!("{}{}\n", "\t".repeat(level), self.node_to_string());
    }

    pub fn node_to_string(&self) -> String {
        return format!(
            "{{{}{}}}",
            match &self.value {
                Literal::Int(intval) => format!("int: {}", intval),
                Literal::Float(floatval) => format!("float: {}", floatval),
                Literal::True(_) => String::from("true"),
                Literal::False(_) => String::from("false"),
                Literal::String(stringval) => format!("string: \"{}\"", stringval.clone()),
            },
            match self.data_type.data_type {
                Type::UnTyped => String::from(""),
                _ => format!(", type: '{}'", self.data_type.get_label()),
            }
        );
    }
}

#[derive(Clone, PartialEq)]
pub enum Literal {
    Int(i128),
    Float(f64),
    True(bool),
    False(bool),
    String(String),
}

#[derive(Clone, PartialEq)]
pub enum ExpressionNode {
    Binary(BinaryExprNode),
    Unary(UnaryExprNode),
    Literal(LiteralNode),
}

impl ExpressionNode {
    pub fn get_type(&self) -> DataType {
        match self {
            ExpressionNode::Binary(binary_node) => return binary_node.data_type.clone(),
            ExpressionNode::Unary(unary_node) => unary_node.data_type.clone(),
            ExpressionNode::Literal(literal_node) => literal_node.data_type.clone(),
        }
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.set_type(data_type),
            ExpressionNode::Unary(unary_node) => unary_node.set_type(data_type),
            ExpressionNode::Literal(literal_node) => literal_node.set_type(data_type),
        }
    }

    pub fn get_token(&self) -> &Token {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.get_token(),
            ExpressionNode::Unary(unary_node) => unary_node.get_token(),
            ExpressionNode::Literal(literal_node) => literal_node.get_token(),
        }
    }

    pub fn to_string(&self, ast: &AST) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.to_string_with_level(0, ast),
            ExpressionNode::Unary(unary_node) => unary_node.to_string_with_level(0, ast),
            ExpressionNode::Literal(literal_node) => literal_node.to_string_with_level(0),
        }
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.to_string_with_level(level, ast),
            ExpressionNode::Unary(unary_node) => unary_node.to_string_with_level(level, ast),
            ExpressionNode::Literal(literal_node) => literal_node.to_string_with_level(level),
        }
    }

    pub fn node_to_string(&self) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.node_to_string(),
            ExpressionNode::Unary(unary_node) => unary_node.node_to_string(),
            ExpressionNode::Literal(literal_node) => literal_node.node_to_string(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct BinaryExprNode {
    pub operator: BinaryOperator,
    pub token: Token,
    pub data_type: DataType,

    pub left: NodePointer,
    pub right: NodePointer,
}

impl BinaryExprNode {
    pub fn new(
        operator: BinaryOperator,
        token: Token,
        left: NodePointer,
        right: NodePointer,
    ) -> BinaryExprNode {
        return BinaryExprNode {
            operator,
            token,
            data_type: DataType::new(Type::UnTyped, Type::UnTyped.get_label()),
            left,
            right,
        };
    }

    pub fn set_left(&mut self, new_child: NodePointer) {
        self.left = new_child;
    }

    pub fn set_right(&mut self, new_child: NodePointer) {
        self.right = new_child;
    }

    pub fn get_type(&self) -> DataType {
        return self.data_type.clone();
    }

    pub fn set_type(&mut self, data_type: DataType) {
        self.data_type = data_type;
    }

    pub fn get_token(&self) -> &Token {
        return &self.token;
    }

    pub fn to_string(&self, ast: &AST) -> String {
        return self.to_string_with_level(0, ast);
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        return format!(
            "{}{}\n{}{}",
            "\t".repeat(level),
            self.node_to_string(),
            ast.get_node(self.left).to_string_with_level(level + 1, ast),
            ast.get_node(self.right)
                .to_string_with_level(level + 1, ast),
        );
    }

    pub fn node_to_string(&self) -> String {
        return format!(
            "{{{}{}}}",
            self.get_operator(),
            match self.data_type.data_type {
                Type::UnTyped => String::from(""),
                _ => format!(", type: '{}'", self.data_type.get_label()),
            }
        );
    }

    pub fn get_operator(&self) -> String {
        return match self.operator {
            BinaryOperator::Plus => String::from("+"),
            BinaryOperator::Minus => String::from("-"),
            BinaryOperator::Times => String::from("*"),
            BinaryOperator::Divide => String::from("/"),
            BinaryOperator::Modulus => String::from("%"),
            BinaryOperator::NotEqual => String::from("!="),
            BinaryOperator::Equal => String::from("=="),
            BinaryOperator::LessThan => String::from("<"),
            BinaryOperator::LessThanOrEqual => String::from("<="),
            BinaryOperator::GreaterThan => String::from(">"),
            BinaryOperator::GreaterThanOrEqual => String::from(">="),
            BinaryOperator::And => String::from("and"),
            BinaryOperator::Or => String::from("or"),
        };
    }
}

#[derive(Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    NotEqual,
    Equal,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    And,
    Or,
}

#[derive(Clone, PartialEq)]
pub struct UnaryExprNode {
    pub operator: UnaryOperator,
    pub token: Token,
    pub data_type: DataType,

    pub operand: NodePointer,
}

impl UnaryExprNode {
    pub fn new(operator: UnaryOperator, token: Token, operand: NodePointer) -> UnaryExprNode {
        return UnaryExprNode {
            operator,
            token,
            data_type: DataType::new(Type::UnTyped, Type::UnTyped.get_label()),
            operand,
        };
    }

    pub fn set_operand(&mut self, new_child: NodePointer) {
        self.operand = new_child;
    }

    pub fn get_type(&self) -> DataType {
        return self.data_type.clone();
    }

    pub fn set_type(&mut self, data_type: DataType) {
        self.data_type = data_type;
    }

    pub fn get_token(&self) -> &Token {
        return &self.token;
    }

    pub fn to_string(&self, ast: &AST) -> String {
        return self.to_string_with_level(0, ast);
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        return format!(
            "{}{}\n{}",
            "\t".repeat(level),
            self.node_to_string(),
            ast.get_node(self.operand)
                .to_string_with_level(level + 1, ast),
        );
    }

    pub fn node_to_string(&self) -> String {
        return format!(
            "{{{}{}{}}}",
            match self.operator {
                UnaryOperator::Minus => "unary ",
                _ => "",
            },
            self.get_operator(),
            match self.data_type.data_type {
                Type::UnTyped => String::from(""),
                _ => format!(", type: '{}'", self.data_type.get_label()),
            }
        );
    }

    pub fn get_operator(&self) -> String {
        return match self.operator {
            UnaryOperator::Minus => String::from("-"),
            UnaryOperator::Not => String::from("not"),
        };
    }
}

#[derive(Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[derive(Clone, PartialEq)]
pub struct NotANode {}

impl NotANode {
    pub fn new() -> NotANode {
        return NotANode {};
    }

    pub fn get_type(&self) -> DataType {
        return DataType::new(Type::UnTyped, Type::UnTyped.get_label());
    }

    pub fn to_string(&self) -> String {
        return self.to_string_with_level(0);
    }

    pub fn to_string_with_level(&self, level: usize) -> String {
        return format!("{}{}\n", "\t".repeat(level), self.node_to_string());
    }

    pub fn node_to_string(&self) -> String {
        return String::from("{NotANode}");
    }
}
