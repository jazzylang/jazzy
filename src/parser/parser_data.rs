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

use std::{cell::RefCell, fmt::Debug, rc::Rc};

use crate::{
    infrastructure::{error::ErrorReporter, log::Logger},
    scanner::scanner_data::Token,
    semantic_checker::semantic_checker_data::{Callback, DataType, Symbol, SymbolTable, Type},
};

pub type NodePointer = usize;

#[derive(Debug, PartialEq)]
pub struct AST {
    nodes: Vec<ASTNode>,
    pub root_node: NodePointer,
}

impl AST {
    pub fn new() -> AST {
        return AST {
            nodes: vec![ASTNode::RootNode(RootNode::new(vec![]))],
            root_node: 0,
        };
    }

    pub fn get_node(&self, pointer: NodePointer) -> &ASTNode {
        return &self.nodes[pointer];
    }

    pub fn get_mut_node(&mut self, pointer: NodePointer) -> &mut ASTNode {
        return &mut self.nodes[pointer];
    }

    pub fn get_root_node(&self) -> &ASTNode {
        return &self.nodes[self.root_node];
    }

    pub fn get_mut_root_node(&mut self) -> &mut ASTNode {
        return &mut self.nodes[self.root_node];
    }

    pub fn set_node(&mut self, pointer: NodePointer, node: ASTNode) {
        self.nodes[pointer] = node;
    }

    pub fn new_node(&mut self, node: ASTNode) -> NodePointer {
        self.nodes.push(node);
        return self.nodes.len() - 1;
    }

    pub fn get_pointer(&mut self, node: &ASTNode) -> NodePointer {
        let mut pointer = 0;
        for ast_node in &self.nodes {
            if node == ast_node {
                return pointer;
            }
            pointer += 1;
        }

        panic!("ASTNode cannot be found in AST");
    }

    pub fn get_new_children(&self) -> Vec<NodePointer> {
        let root_ast_node = &self.nodes[self.root_node];
        match root_ast_node {
            ASTNode::RootNode(root_node) => {
                return root_node.children[root_node.new_children_start_at..].to_vec();
            }
            _ => panic!("root_node is not a RootNode somehow"),
        }
    }

    pub fn len(&self) -> usize {
        return self.nodes.len();
    }
}

#[derive(Clone, PartialEq)]
pub enum ASTNode {
    RootNode(RootNode),
    ExpressionNode(ExpressionNode),
    StatementNode(StatementNode),
    IdentifierNode(IdentifierStatementNode),
    TypeHintNode(TypeHintNode),
    NotANode(NotANode),
}

impl ASTNode {
    pub fn get_type(&self) -> DataType {
        match self {
            ASTNode::RootNode(node) => return node.get_type(),
            ASTNode::ExpressionNode(node) => return node.get_type(),
            ASTNode::StatementNode(_) => panic!("StatementNode has no type"),
            ASTNode::IdentifierNode(_) => panic!("IdentifierNode has no type"),
            ASTNode::TypeHintNode(node) => return node.get_type(),
            ASTNode::NotANode(node) => return node.get_type(),
        }
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match self {
            ASTNode::RootNode(_) => panic!("Can not set data type for RootNode"),
            ASTNode::ExpressionNode(node) => node.set_type(data_type),
            ASTNode::StatementNode(_) => panic!("StatementNode has no type"),
            ASTNode::IdentifierNode(_) => panic!("IdentifierNode has no type"),
            ASTNode::TypeHintNode(_) => panic!("TypeHintNode has no type"),
            ASTNode::NotANode(_) => panic!("Can not set data type for NotANode"),
        }
    }

    pub fn get_token(&self) -> &Token {
        match self {
            ASTNode::RootNode(_) => panic!("RootNode has no token"),
            ASTNode::ExpressionNode(node) => return node.get_token(),
            ASTNode::StatementNode(node) => return node.get_token(),
            ASTNode::IdentifierNode(node) => return node.get_token(),
            ASTNode::TypeHintNode(node) => return node.get_token(),
            ASTNode::NotANode(_) => panic!("NotANode has no token"),
        }
    }

    pub fn to_string(&self, ast: &AST) -> String {
        match self {
            ASTNode::RootNode(node) => return node.to_string(ast),
            ASTNode::ExpressionNode(node) => return node.to_string(ast),
            ASTNode::StatementNode(node) => return node.to_string(ast),
            ASTNode::IdentifierNode(node) => return node.to_string(),
            ASTNode::TypeHintNode(node) => return node.to_string(),
            ASTNode::NotANode(node) => return node.to_string(),
        }
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self {
            ASTNode::RootNode(node) => return node.to_string_with_level(level, ast),
            ASTNode::ExpressionNode(node) => return node.to_string_with_level(level, ast),
            ASTNode::StatementNode(node) => return node.to_string_with_level(level, ast),
            ASTNode::IdentifierNode(node) => return node.to_string_with_level(level),
            ASTNode::TypeHintNode(node) => return node.to_string_with_level(level),
            ASTNode::NotANode(node) => return node.to_string_with_level(level),
        }
    }

    pub fn node_to_string(&self) -> String {
        match self {
            ASTNode::RootNode(node) => return node.node_to_string(),
            ASTNode::ExpressionNode(node) => return node.node_to_string(),
            ASTNode::StatementNode(node) => return node.node_to_string(),
            ASTNode::IdentifierNode(node) => return node.node_to_string(),
            ASTNode::TypeHintNode(node) => return node.node_to_string(),
            ASTNode::NotANode(node) => return node.node_to_string(),
        }
    }
}

impl Debug for ASTNode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ASTNode::RootNode(_) => write!(f, "ASTNode::RootNode"),
            ASTNode::ExpressionNode(_) => write!(f, "ASTNode::ExpressionNode"),
            ASTNode::StatementNode(_) => write!(f, "ASTNode::StatementNode"),
            ASTNode::IdentifierNode(_) => write!(f, "ASTNode::IdentifierNode"),
            ASTNode::TypeHintNode(_) => write!(f, "ASTNode::TypeHintNode"),
            ASTNode::NotANode(_) => write!(f, "ASTNode::NotANode"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct RootNode {
    pub children: Vec<NodePointer>,
    pub new_children_start_at: usize,
}

impl RootNode {
    pub fn new(children: Vec<NodePointer>) -> RootNode {
        RootNode {
            children,
            new_children_start_at: 0,
        }
    }

    pub fn set_children(&mut self, new_children: Vec<NodePointer>) {
        self.children = new_children;
    }

    pub fn add_child(&mut self, new_child: NodePointer) {
        self.new_children_start_at = self.children.len();
        self.children.push(new_child);
    }

    pub fn add_children(&mut self, mut new_children: Vec<NodePointer>) {
        self.new_children_start_at = self.children.len();
        self.children.append(&mut new_children);
    }

    pub fn get_type(&self) -> DataType {
        return DataType::new(Type::UnTyped);
    }

    pub fn to_string(&self, ast: &AST) -> String {
        return self.to_string_with_level(0, ast);
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        let mut format = format!("{}\n", self.node_to_string());

        for child in self.children.clone() {
            format.push_str(&ast.get_node(child).to_string_with_level(level + 1, ast))
        }

        return format;
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
            data_type: DataType::new(Type::UnTyped),
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
    Variable(VariableExprNode),
    Literal(LiteralNode),
}

impl ExpressionNode {
    pub fn get_type(&self) -> DataType {
        match self {
            ExpressionNode::Binary(binary_node) => return binary_node.data_type.clone(),
            ExpressionNode::Unary(unary_node) => unary_node.data_type.clone(),
            ExpressionNode::Variable(variable_node) => variable_node.get_type(),
            ExpressionNode::Literal(literal_node) => literal_node.data_type.clone(),
        }
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.set_type(data_type),
            ExpressionNode::Unary(unary_node) => unary_node.set_type(data_type),
            ExpressionNode::Variable(variable_node) => variable_node.set_type(data_type),
            ExpressionNode::Literal(literal_node) => literal_node.set_type(data_type),
        }
    }

    pub fn get_token(&self) -> &Token {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.get_token(),
            ExpressionNode::Unary(unary_node) => unary_node.get_token(),
            ExpressionNode::Variable(variable_node) => variable_node.get_token(),
            ExpressionNode::Literal(literal_node) => literal_node.get_token(),
        }
    }

    pub fn to_string(&self, ast: &AST) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.to_string_with_level(0, ast),
            ExpressionNode::Unary(unary_node) => unary_node.to_string_with_level(0, ast),
            ExpressionNode::Variable(variable_node) => variable_node.to_string_with_level(0),
            ExpressionNode::Literal(literal_node) => literal_node.to_string_with_level(0),
        }
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.to_string_with_level(level, ast),
            ExpressionNode::Unary(unary_node) => unary_node.to_string_with_level(level, ast),
            ExpressionNode::Variable(variable_node) => variable_node.to_string_with_level(level),
            ExpressionNode::Literal(literal_node) => literal_node.to_string_with_level(level),
        }
    }

    pub fn node_to_string(&self) -> String {
        match self {
            ExpressionNode::Binary(binary_node) => binary_node.node_to_string(),
            ExpressionNode::Unary(unary_node) => unary_node.node_to_string(),
            ExpressionNode::Variable(variable_node) => variable_node.node_to_string(),
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
            data_type: DataType::new(Type::UnTyped),
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
            data_type: DataType::new(Type::UnTyped),
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
pub struct VariableExprNode {
    pub name: String,
    pub symbol: Option<Rc<RefCell<Symbol>>>,
    pub token: Token,
}

impl VariableExprNode {
    pub fn new(name: String, token: Token) -> VariableExprNode {
        return VariableExprNode {
            name,
            symbol: None,
            token,
        };
    }
    pub fn get_type(&self) -> DataType {
        match &self.symbol {
            Some(symbol) => {
                return symbol.borrow().get_type().clone();
            }
            None => return DataType::new(Type::UnTyped),
        };
    }

    pub fn set_type(&mut self, data_type: DataType) {
        match &self.symbol {
            Some(symbol) => {
                return symbol.borrow_mut().set_type(data_type);
            }
            None => panic!("Trying to set type of variable that has no symbol"),
        }
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
        // return format!("{{Variable: {}}}", self.name);
        return format!(
            "{{Variable: {}{}}}",
            self.name,
            match self.get_type().data_type {
                Type::UnTyped => String::from(""),
                _type => format!(", type: '{}'", _type.get_label()),
            }
        );
    }
}

#[derive(Clone, PartialEq)]
pub enum StatementNode {
    VariableDeclaration(VariableDeclarationNode),
    VariableAssignment(VariableAssignmentNode),
}

impl StatementNode {
    pub fn get_token(&self) -> &Token {
        match self {
            StatementNode::VariableDeclaration(var_node) => var_node.get_token(),
            StatementNode::VariableAssignment(assmt_node) => assmt_node.get_token(),
        }
    }

    pub fn to_string(&self, ast: &AST) -> String {
        match self {
            StatementNode::VariableDeclaration(var_node) => var_node.to_string_with_level(0, ast),
            StatementNode::VariableAssignment(assmt_node) => {
                assmt_node.to_string_with_level(0, ast)
            }
        }
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self {
            StatementNode::VariableDeclaration(var_node) => {
                var_node.to_string_with_level(level, ast)
            }
            StatementNode::VariableAssignment(assmt_node) => {
                assmt_node.to_string_with_level(level, ast)
            }
        }
    }

    pub fn node_to_string(&self) -> String {
        match self {
            StatementNode::VariableDeclaration(var_node) => var_node.node_to_string(),
            StatementNode::VariableAssignment(assmt_node) => assmt_node.node_to_string(),
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct VariableDeclarationNode {
    pub mutable: bool,
    pub name: String,
    pub identifier: NodePointer,
    pub type_hint: Option<NodePointer>,
    pub expression: NodePointer,
    pub token: Token,
    pub symbol: Option<Rc<RefCell<Symbol>>>,
}

impl VariableDeclarationNode {
    pub fn new(
        mutable: bool,
        name: String,
        identifier: NodePointer,
        type_hint: Option<NodePointer>,
        expression: NodePointer,
        token: Token,
    ) -> VariableDeclarationNode {
        return VariableDeclarationNode {
            mutable,
            name,
            identifier,
            type_hint,
            expression,
            token,
            symbol: None,
        };
    }

    pub fn get_token(&self) -> &Token {
        return &self.token;
    }

    pub fn to_string(&self, ast: &AST) -> String {
        return self.to_string_with_level(0, ast);
    }

    pub fn to_string_with_level(&self, level: usize, ast: &AST) -> String {
        match self.type_hint {
            Some(type_hint) => {
                return format!(
                    "{}{}\n{}{}{}",
                    "\t".repeat(level),
                    self.node_to_string(),
                    ast.get_node(self.identifier)
                        .to_string_with_level(level + 1, ast),
                    ast.get_node(type_hint).to_string_with_level(level + 1, ast),
                    ast.get_node(self.expression)
                        .to_string_with_level(level + 1, ast)
                );
            }
            None => {
                return format!(
                    "{}{}\n{}{}",
                    "\t".repeat(level),
                    self.node_to_string(),
                    ast.get_node(self.identifier)
                        .to_string_with_level(level + 1, ast),
                    ast.get_node(self.expression)
                        .to_string_with_level(level + 1, ast)
                );
            }
        }
    }

    pub fn node_to_string(&self) -> String {
        return format!(
            "{{Declaration{}}}",
            if self.mutable { " (mut)" } else { "" }
        );
    }
}

#[derive(Clone, PartialEq)]
pub struct VariableAssignmentNode {
    pub name: String,
    pub identifier: NodePointer,
    pub expression: NodePointer,
    pub token: Token,
    pub symbol: Option<Rc<RefCell<Symbol>>>,
}

impl VariableAssignmentNode {
    pub fn new(
        name: String,
        identifier: NodePointer,
        expression: NodePointer,
        token: Token,
    ) -> VariableAssignmentNode {
        return VariableAssignmentNode {
            name,
            identifier,
            expression,
            token,
            symbol: None,
        };
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
            ast.get_node(self.identifier)
                .to_string_with_level(level + 1, ast),
            ast.get_node(self.expression)
                .to_string_with_level(level + 1, ast)
        );
    }

    pub fn node_to_string(&self) -> String {
        return String::from("{Assignment}");
    }
}

#[derive(Clone, PartialEq)]
pub struct IdentifierStatementNode {
    pub name: String,
    pub token: Token,
}

impl IdentifierStatementNode {
    pub fn new(name: String, token: Token) -> IdentifierStatementNode {
        return IdentifierStatementNode { name, token };
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
        return format!("{{ID: {}}}", self.name);
    }
}

#[derive(Clone, PartialEq)]
pub struct TypeHintNode {
    data_type: DataType,
    token: Token,
}

impl TypeHintNode {
    pub fn new(data_type: DataType, token: Token) -> TypeHintNode {
        return TypeHintNode { data_type, token };
    }

    pub fn get_type(&self) -> DataType {
        return self.data_type.clone();
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
        return format!("{{Type: {}}}", self.data_type.get_label());
    }
}

#[derive(Clone, PartialEq)]
pub struct NotANode {}

impl NotANode {
    pub fn new() -> NotANode {
        return NotANode {};
    }

    pub fn get_type(&self) -> DataType {
        return DataType::new(Type::UnTyped);
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

pub enum Traversal<'traversal> {
    Preorder(&'traversal mut Callback),
    Postorder(&'traversal mut Callback),
    PrePostorder(&'traversal mut Callback, &'traversal mut Callback),
}

pub fn traverse(
    traversal: &mut Traversal,
    node: NodePointer,
    ast: &mut AST,
    symbol_table: &mut SymbolTable,
    logger: &mut Logger,
    error: &mut ErrorReporter,
) {
    // If we're doing pre or prepost,
    // run the callback before we traverse to our children
    match traversal {
        Traversal::Preorder(&mut ref mut callback_pre) => {
            callback_pre.run(node, ast, symbol_table, logger, error)
        }
        Traversal::Postorder(_) => {}
        Traversal::PrePostorder(&mut ref mut callback_pre, _) => {
            callback_pre.run(node, ast, symbol_table, logger, error)
        }
    }

    match ast.get_node(node) {
        ASTNode::RootNode(root_node) => {
            for child in root_node.children.clone() {
                traverse(traversal, child, ast, symbol_table, logger, error);
            }
        }
        ASTNode::ExpressionNode(expr_node) => match expr_node {
            ExpressionNode::Binary(bin_node) => {
                let left = bin_node.left;
                let right = bin_node.right;
                traverse(traversal, left, ast, symbol_table, logger, error);
                traverse(traversal, right, ast, symbol_table, logger, error);
            }
            ExpressionNode::Unary(un_node) => {
                traverse(traversal, un_node.operand, ast, symbol_table, logger, error);
            }
            // Leaf node
            ExpressionNode::Literal(_) => {}
            ExpressionNode::Variable(_) => {}
        },
        ASTNode::StatementNode(stat_node) => match stat_node {
            StatementNode::VariableDeclaration(var_node) => {
                let expression = var_node.expression;
                match var_node.type_hint {
                    Some(type_hint) => {
                        traverse(traversal, type_hint, ast, symbol_table, logger, error)
                    }
                    None => {}
                }
                traverse(traversal, expression, ast, symbol_table, logger, error);
            }
            StatementNode::VariableAssignment(assmt_node) => {
                let expression = assmt_node.expression;
                traverse(traversal, expression, ast, symbol_table, logger, error);
            }
        },
        // Leaf node
        ASTNode::IdentifierNode(_) => {}
        ASTNode::TypeHintNode(_) => {}
        ASTNode::NotANode(_) => {}
    }

    // If we're doing post or prepost,
    // run the callback before we traverse to our children
    match traversal {
        Traversal::Preorder(_) => {}
        Traversal::Postorder(callback_post) => {
            callback_post.run(node, ast, symbol_table, logger, error)
        }
        Traversal::PrePostorder(_, callback_post) => {
            callback_post.run(node, ast, symbol_table, logger, error)
        }
    }
}
