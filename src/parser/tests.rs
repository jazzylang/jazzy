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
        BinaryExprNode, BinaryOperator, ExpressionNode, UnaryExprNode, UnaryOperator, AST,
    },
    scanner::scanner_data::{Token, TokenType},
};

use super::{
    parser::Parser,
    parser_data::{ASTNode, Literal, LiteralNode},
};

#[test]
fn test_equality_not_equal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 != 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(5);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::NotEqual,
            Some(String::from("!=")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.equality();

    // {!=}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::NotEqual, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_equality_equal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 == 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(5);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Equal,
            Some(String::from("==")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.equality();

    // {==}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Equal, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_comparison_less_than_or_equal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 <= 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(5);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::LessThanOrEqual,
            Some(String::from("<=")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    // {<=}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(
            BinaryOperator::LessThanOrEqual,
            tokens[1].clone(),
            left,
            right,
        ),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_comparison_greater_than_or_equal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 >= 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(5);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::GreaterThanOrEqual,
            Some(String::from(">=")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    // {>=}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(
            BinaryOperator::GreaterThanOrEqual,
            tokens[1].clone(),
            left,
            right,
        ),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_comparison_less_than() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 < 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::LessThan,
            Some(String::from("<")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    // {<}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::LessThan, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_comparison_greater_than() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 > 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::GreaterThan,
            Some(String::from(">")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    // {>}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::GreaterThan, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_term_subtract() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 - 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Minus,
            Some(String::from("-")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.term();

    // {-}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Minus, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_term_add() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 + 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Plus,
            Some(String::from("+")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.term();

    // {+}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Plus, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_factor_divide() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 / 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Slash,
            Some(String::from("/")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.factor();

    // {/}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Divide, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_factor_times() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 * 2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Star,
            Some(String::from("*")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            true,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.factor();

    // {*}
    //     {1}
    //     {2}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(2), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Times, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_boolean_or() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("true or false");

    let mut true_info = LocationInfo::new();
    true_info.repl_str = Some(repl_str.clone());
    true_info.offset = Some(0);
    true_info.length = Some(4);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(5);
    operator_info.length = Some(2);

    let mut false_info = LocationInfo::new();
    false_info.repl_str = Some(repl_str.clone());
    false_info.offset = Some(8);
    false_info.length = Some(5);

    let tokens = vec![
        Token::new(
            TokenType::True,
            Some(String::from("true")),
            true,
            false,
            true_info,
        ),
        Token::new(
            TokenType::Or,
            Some(String::from("or")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::False,
            Some(String::from("false")),
            true,
            true,
            false_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, false_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.boolean();

    // {or}
    //     {true}
    //     {false}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::True(true), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::False(false), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::Or, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_boolean_and() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("true and false");

    let mut true_info = LocationInfo::new();
    true_info.repl_str = Some(repl_str.clone());
    true_info.offset = Some(0);
    true_info.length = Some(4);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(5);
    operator_info.length = Some(3);

    let mut false_info = LocationInfo::new();
    false_info.repl_str = Some(repl_str.clone());
    false_info.offset = Some(9);
    false_info.length = Some(5);

    let tokens = vec![
        Token::new(
            TokenType::True,
            Some(String::from("true")),
            true,
            false,
            true_info,
        ),
        Token::new(
            TokenType::And,
            Some(String::from("and")),
            true,
            true,
            operator_info,
        ),
        Token::new(
            TokenType::False,
            Some(String::from("false")),
            true,
            true,
            false_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, false_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.boolean();

    // {and}
    //     {true}
    //     {false}
    //
    // Define nodes and add them to the AST arena
    // (note that the operands will be parsed before the operator)
    let left = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::True(true), tokens[0].clone()),
    )));
    let right = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::False(false), tokens[2].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Binary(
        BinaryExprNode::new(BinaryOperator::And, tokens[1].clone(), left, right),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
    assert_eq!(ast_actual.get_node(2), ast_expected.get_node(2));
}

#[test]
fn test_unary_minus() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("-1");

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(0);
    operator_info.length = Some(1);

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(1);
    one_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::Minus,
            Some(String::from("-")),
            false,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, one_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.unary();

    // {-}
    //     {1}
    //
    // Define nodes and add them to the AST arena
    // (note that the operand will be parsed before the operator)
    let operand = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[1].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Unary(
        UnaryExprNode::new(UnaryOperator::Minus, tokens[0].clone(), operand),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
}

#[test]
fn test_unary_not() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("not true");

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(0);
    operator_info.length = Some(3);

    let mut true_info = LocationInfo::new();
    true_info.repl_str = Some(repl_str.clone());
    true_info.offset = Some(4);
    true_info.length = Some(4);

    let tokens = vec![
        Token::new(
            TokenType::Not,
            Some(String::from("not")),
            true,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::True,
            Some(String::from("true")),
            true,
            true,
            true_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, true_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.unary();

    // {not}
    //     {true}
    //
    // Define nodes and add them to the AST arena
    // (note that the operand will be parsed before the operator)
    let operand = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::True(true), tokens[1].clone()),
    )));
    let _operator = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Unary(
        UnaryExprNode::new(UnaryOperator::Not, tokens[0].clone(), operand),
    )));

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
    assert_eq!(ast_actual.get_node(1), ast_expected.get_node(1));
}

#[test]
fn test_primary_true() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("true"));
    location_info.offset = Some(0);
    location_info.length = Some(4);
    let tokens = vec![
        Token::new(
            TokenType::True,
            Some(String::from("true")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::True(true), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_false() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("false"));
    location_info.offset = Some(0);
    location_info.length = Some(5);
    let tokens = vec![
        Token::new(
            TokenType::False,
            Some(String::from("false")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::False(false), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_intlit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("1"));
    location_info.offset = Some(0);
    location_info.length = Some(1);
    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_floatlit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("3.14"));
    location_info.offset = Some(0);
    location_info.length = Some(4);
    let tokens = vec![
        Token::new(
            TokenType::FloatLit,
            Some(String::from("3.14")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Float(3.14), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_positive_scientific_lit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("2.5e10"));
    location_info.offset = Some(0);
    location_info.length = Some(6);
    let tokens = vec![
        Token::new(
            TokenType::FloatLit,
            Some(String::from("2.5e10")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Float(25000000000.0), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_negative_scientific_lit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("1e-2"));
    location_info.offset = Some(0);
    location_info.length = Some(4);
    let tokens = vec![
        Token::new(
            TokenType::FloatLit,
            Some(String::from("1e-2")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Float(0.01), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_hexlit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("0xFF"));
    location_info.offset = Some(0);
    location_info.length = Some(4);
    let tokens = vec![
        Token::new(
            TokenType::HexLit,
            Some(String::from("0xFF")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(255), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_binarylit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("0b1111"));
    location_info.offset = Some(0);
    location_info.length = Some(6);
    let tokens = vec![
        Token::new(
            TokenType::BinaryLit,
            Some(String::from("0b1111")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(15), tokens[0].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_stringlit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let mut location_info = LocationInfo::new();
    location_info.repl_str = Some(String::from("\"test string\\n\\r\\t\\\\\\0\\\""));
    location_info.offset = Some(0);
    location_info.length = Some(6);
    let tokens = vec![
        Token::new(
            TokenType::StringLit,
            Some(String::from("test string\\n\\r\\t\\\\\\0\\\"")),
            true,
            false,
            location_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, location_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(
            Literal::String(String::from("test string\\n\\r\\t\\\\\\0\\\"")),
            tokens[0].clone(),
        ),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

#[test]
fn test_primary_parenthesized_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("(1)");

    let mut left_paren_info = LocationInfo::new();
    left_paren_info.repl_str = Some(repl_str.clone());
    left_paren_info.offset = Some(0);
    left_paren_info.length = Some(1);

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(1);
    one_info.length = Some(1);

    let mut right_paren_info = LocationInfo::new();
    right_paren_info.repl_str = Some(repl_str.clone());
    right_paren_info.offset = Some(2);
    right_paren_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::LeftParen,
            Some(String::from("1")),
            false,
            false,
            left_paren_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            false,
            false,
            one_info,
        ),
        Token::new(
            TokenType::RightParen,
            Some(String::from(")")),
            false,
            true,
            right_paren_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, right_paren_info),
    ];

    let mut ast_actual = AST::new();
    let mut ast_expected = AST::new();

    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    let _literal_pointer = ast_expected.new_node(ASTNode::ExpressionNode(ExpressionNode::Literal(
        LiteralNode::new(Literal::Int(1), tokens[1].clone()),
    )));

    // No need to hook up to the root node, since this parser function doesn't either

    assert_eq!(ast_actual.get_node(0), ast_expected.get_node(0));
}

// -------------
// SYNTAX ERRORS
// -------------
#[test]
fn test_error_equality_expected_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 ==");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Equal,
            Some(String::from("==")),
            true,
            true,
            operator_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, operator_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.equality();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::ExpectedExpressionError)
            )
        }
    }
}

#[test]
fn test_error_equality_missing_padding_around_operator() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1==2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(2);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(4);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Equal,
            Some(String::from("==")),
            false,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            false,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.equality();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MissingPaddingAroundOperatorError)
            )
        }
    }
}

#[test]
fn test_error_comparison_expected_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 <");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::LessThan,
            Some(String::from("<")),
            true,
            true,
            operator_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, operator_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::ExpectedExpressionError)
            )
        }
    }
}

#[test]
fn test_error_comparison_missing_padding_around_operator() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1<2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(3);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::LessThan,
            Some(String::from("<")),
            false,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            false,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.comparison();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MissingPaddingAroundOperatorError)
            )
        }
    }
}

#[test]
fn test_error_term_expected_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 +");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Plus,
            Some(String::from("+")),
            true,
            true,
            operator_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, operator_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.term();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::ExpectedExpressionError)
            )
        }
    }
}

#[test]
fn test_error_term_missing_padding_around_operator() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1+2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(3);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Plus,
            Some(String::from("+")),
            false,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            false,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.term();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MissingPaddingAroundOperatorError)
            )
        }
    }
}

#[test]
fn test_error_factor_expected_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1 *");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Star,
            Some(String::from("*")),
            true,
            true,
            operator_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, operator_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.factor();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::ExpectedExpressionError)
            )
        }
    }
}

#[test]
fn test_error_factor_missing_padding_around_operator() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("1*2");

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(0);
    one_info.length = Some(1);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(2);
    operator_info.length = Some(1);

    let mut two_info = LocationInfo::new();
    two_info.repl_str = Some(repl_str.clone());
    two_info.offset = Some(3);
    two_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            true,
            false,
            one_info,
        ),
        Token::new(
            TokenType::Star,
            Some(String::from("*")),
            false,
            false,
            operator_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("2")),
            true,
            false,
            two_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, two_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.factor();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MissingPaddingAroundOperatorError)
            )
        }
    }
}

#[test]
fn test_error_boolean_expected_expression() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("true and");

    let mut true_info = LocationInfo::new();
    true_info.repl_str = Some(repl_str.clone());
    true_info.offset = Some(0);
    true_info.length = Some(4);

    let mut operator_info = LocationInfo::new();
    operator_info.repl_str = Some(repl_str.clone());
    operator_info.offset = Some(5);
    operator_info.length = Some(3);

    let tokens = vec![
        Token::new(
            TokenType::True,
            Some(String::from("true")),
            true,
            false,
            true_info,
        ),
        Token::new(
            TokenType::And,
            Some(String::from("and")),
            true,
            true,
            operator_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, operator_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.boolean();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::ExpectedExpressionError)
            )
        }
    }
}

#[test]
fn test_error_primary_unmatched_parenthesis() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));

    let repl_str = String::from("(1");

    let mut left_paren_info = LocationInfo::new();
    left_paren_info.repl_str = Some(repl_str.clone());
    left_paren_info.offset = Some(0);
    left_paren_info.length = Some(1);

    let mut one_info = LocationInfo::new();
    one_info.repl_str = Some(repl_str.clone());
    one_info.offset = Some(1);
    one_info.length = Some(1);

    let tokens = vec![
        Token::new(
            TokenType::LeftParen,
            Some(String::from("1")),
            false,
            false,
            left_paren_info,
        ),
        Token::new(
            TokenType::IntLit,
            Some(String::from("1")),
            false,
            false,
            one_info.clone(),
        ),
        Token::new(TokenType::EOF, None, false, false, one_info),
    ];

    let mut ast_actual = AST::new();
    let mut parser = Parser::new(tokens.clone(), &mut ast_actual, &mut logger, &mut error);
    parser.primary();

    match parser.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => assert_eq!(
            true,
            console_error.error_was_reported(&ErrorType::UnMatchedParenError)
        ),
    }
}
