use crate::infrastructure::error::{ConsoleErrorReporter, ErrorReporter, ErrorType, WarningType};
use crate::infrastructure::file::{get_graphemes_from_line, LocationInfo};
use crate::infrastructure::log::Logger;
use crate::scanner::scanner::Scanner;
use crate::scanner::scanner_data::TokenType;

#[test]
fn test_identifiers() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("id a-2-Z");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    assert_eq!(scanner.tokens[0].token_type, TokenType::ID);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("id")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(2));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, true);

    assert_eq!(scanner.tokens[1].token_type, TokenType::ID);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from("a-2-Z")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(3));
    assert_eq!(scanner.tokens[1].location_info.length, Some(5));
    assert_eq!(scanner.tokens[1].whitespace_precedes, true);
    assert_eq!(scanner.tokens[1].whitespace_follows, true);
}

#[test]
fn test_keywords() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("let mut i8 f32");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    assert_eq!(scanner.tokens[0].token_type, TokenType::Let);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("let")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(3));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, true);

    assert_eq!(scanner.tokens[1].token_type, TokenType::Mut);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from("mut")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(4));
    assert_eq!(scanner.tokens[1].location_info.length, Some(3));
    assert_eq!(scanner.tokens[1].whitespace_precedes, true);
    assert_eq!(scanner.tokens[1].whitespace_follows, true);

    assert_eq!(scanner.tokens[2].token_type, TokenType::I8);
    assert_eq!(scanner.tokens[2].lexeme, Some(String::from("i8")));
    assert_eq!(scanner.tokens[2].location_info.offset, Some(8));
    assert_eq!(scanner.tokens[2].location_info.length, Some(2));
    assert_eq!(scanner.tokens[2].whitespace_precedes, true);
    assert_eq!(scanner.tokens[2].whitespace_follows, true);

    assert_eq!(scanner.tokens[3].token_type, TokenType::F32);
    assert_eq!(scanner.tokens[3].lexeme, Some(String::from("f32")));
    assert_eq!(scanner.tokens[3].location_info.offset, Some(11));
    assert_eq!(scanner.tokens[3].location_info.length, Some(3));
    assert_eq!(scanner.tokens[3].whitespace_precedes, true);
    assert_eq!(scanner.tokens[3].whitespace_follows, true);
}

#[test]
fn test_numbers() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from(
        "1 0123456789 0x01aA2bB3cC4dD5eE6fF789 0b00101101100 3.14 1e-5 2.34e200 1_000_000_000",
    );
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    assert_eq!(scanner.tokens[0].token_type, TokenType::IntLit);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("1")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(1));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, true);

    assert_eq!(scanner.tokens[1].token_type, TokenType::IntLit);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from("0123456789")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(2));
    assert_eq!(scanner.tokens[1].location_info.length, Some(10));
    assert_eq!(scanner.tokens[1].whitespace_precedes, true);
    assert_eq!(scanner.tokens[1].whitespace_follows, true);

    assert_eq!(scanner.tokens[2].token_type, TokenType::HexLit);
    assert_eq!(
        scanner.tokens[2].lexeme,
        Some(String::from("0x01aA2bB3cC4dD5eE6fF789"))
    );
    assert_eq!(scanner.tokens[2].location_info.offset, Some(13));
    assert_eq!(scanner.tokens[2].location_info.length, Some(24));
    assert_eq!(scanner.tokens[2].whitespace_precedes, true);
    assert_eq!(scanner.tokens[2].whitespace_follows, true);

    assert_eq!(scanner.tokens[3].token_type, TokenType::BinaryLit);
    assert_eq!(
        scanner.tokens[3].lexeme,
        Some(String::from("0b00101101100"))
    );
    assert_eq!(scanner.tokens[3].location_info.offset, Some(38));
    assert_eq!(scanner.tokens[3].location_info.length, Some(13));
    assert_eq!(scanner.tokens[3].whitespace_precedes, true);
    assert_eq!(scanner.tokens[3].whitespace_follows, true);

    assert_eq!(scanner.tokens[4].token_type, TokenType::FloatLit);
    assert_eq!(scanner.tokens[4].lexeme, Some(String::from("3.14")));
    assert_eq!(scanner.tokens[4].location_info.offset, Some(52));
    assert_eq!(scanner.tokens[4].location_info.length, Some(4));
    assert_eq!(scanner.tokens[4].whitespace_precedes, true);
    assert_eq!(scanner.tokens[4].whitespace_follows, true);

    assert_eq!(scanner.tokens[5].token_type, TokenType::FloatLit);
    assert_eq!(scanner.tokens[5].lexeme, Some(String::from("1e-5")));
    assert_eq!(scanner.tokens[5].location_info.offset, Some(57));
    assert_eq!(scanner.tokens[5].location_info.length, Some(4));
    assert_eq!(scanner.tokens[5].whitespace_precedes, true);
    assert_eq!(scanner.tokens[5].whitespace_follows, true);

    assert_eq!(scanner.tokens[6].token_type, TokenType::FloatLit);
    assert_eq!(scanner.tokens[6].lexeme, Some(String::from("2.34e200")));
    assert_eq!(scanner.tokens[6].location_info.offset, Some(62));
    assert_eq!(scanner.tokens[6].location_info.length, Some(8));
    assert_eq!(scanner.tokens[6].whitespace_precedes, true);
    assert_eq!(scanner.tokens[6].whitespace_follows, true);

    assert_eq!(scanner.tokens[7].token_type, TokenType::IntLit);
    assert_eq!(scanner.tokens[7].lexeme, Some(String::from("1000000000")));
    assert_eq!(scanner.tokens[7].location_info.offset, Some(71));
    assert_eq!(scanner.tokens[7].location_info.length, Some(13));
    assert_eq!(scanner.tokens[7].whitespace_precedes, true);
    assert_eq!(scanner.tokens[7].whitespace_follows, true);
}

#[test]
fn test_string_lit() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("\"Test string \\n \\r \\t \\\\ \\0 \\\"\"");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    println!("{}", scanner.tokens[0].to_string());

    assert_eq!(scanner.tokens[0].token_type, TokenType::StringLit);
    assert_eq!(
        scanner.tokens[0].lexeme,
        Some(String::from(
            "Test string \\\\n \\\\r \\\\t \\\\\\\\ \\\\0 \\\\\""
        ))
    );
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(31));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, true);
}

#[test]
fn test_separators() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("(){},.;:");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    // Test each token
    assert_eq!(scanner.tokens[0].token_type, TokenType::LeftParen);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("(")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(1));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, false);

    assert_eq!(scanner.tokens[1].token_type, TokenType::RightParen);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from(")")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(1));
    assert_eq!(scanner.tokens[1].location_info.length, Some(1));
    assert_eq!(scanner.tokens[1].whitespace_precedes, false);
    assert_eq!(scanner.tokens[1].whitespace_follows, false);

    assert_eq!(scanner.tokens[2].token_type, TokenType::LeftBrace);
    assert_eq!(scanner.tokens[2].lexeme, Some(String::from("{")));
    assert_eq!(scanner.tokens[2].location_info.offset, Some(2));
    assert_eq!(scanner.tokens[2].location_info.length, Some(1));
    assert_eq!(scanner.tokens[2].whitespace_precedes, false);
    assert_eq!(scanner.tokens[2].whitespace_follows, false);

    assert_eq!(scanner.tokens[3].token_type, TokenType::RightBrace);
    assert_eq!(scanner.tokens[3].lexeme, Some(String::from("}")));
    assert_eq!(scanner.tokens[3].location_info.offset, Some(3));
    assert_eq!(scanner.tokens[3].location_info.length, Some(1));
    assert_eq!(scanner.tokens[3].whitespace_precedes, false);
    assert_eq!(scanner.tokens[3].whitespace_follows, false);

    assert_eq!(scanner.tokens[4].token_type, TokenType::Comma);
    assert_eq!(scanner.tokens[4].lexeme, Some(String::from(",")));
    assert_eq!(scanner.tokens[4].location_info.offset, Some(4));
    assert_eq!(scanner.tokens[4].location_info.length, Some(1));
    assert_eq!(scanner.tokens[4].whitespace_precedes, false);
    assert_eq!(scanner.tokens[4].whitespace_follows, false);

    assert_eq!(scanner.tokens[5].token_type, TokenType::Dot);
    assert_eq!(scanner.tokens[5].lexeme, Some(String::from(".")));
    assert_eq!(scanner.tokens[5].location_info.offset, Some(5));
    assert_eq!(scanner.tokens[5].location_info.length, Some(1));
    assert_eq!(scanner.tokens[5].whitespace_precedes, false);
    assert_eq!(scanner.tokens[5].whitespace_follows, false);

    assert_eq!(scanner.tokens[6].token_type, TokenType::Semicolon);
    assert_eq!(scanner.tokens[6].lexeme, Some(String::from(";")));
    assert_eq!(scanner.tokens[6].location_info.offset, Some(6));
    assert_eq!(scanner.tokens[6].location_info.length, Some(1));
    assert_eq!(scanner.tokens[6].whitespace_precedes, false);
    assert_eq!(scanner.tokens[6].whitespace_follows, false);
}

#[test]
fn test_operators() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("--=++=**=^^=/ /=%%=:===<<=>>=");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    // Test each token
    assert_eq!(scanner.tokens[0].token_type, TokenType::Minus);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("-")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(1));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, false);

    assert_eq!(scanner.tokens[1].token_type, TokenType::MinusEqual);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from("-=")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(1));
    assert_eq!(scanner.tokens[1].location_info.length, Some(2));
    assert_eq!(scanner.tokens[1].whitespace_precedes, false);
    assert_eq!(scanner.tokens[1].whitespace_follows, false);

    assert_eq!(scanner.tokens[2].token_type, TokenType::Plus);
    assert_eq!(scanner.tokens[2].lexeme, Some(String::from("+")));
    assert_eq!(scanner.tokens[2].location_info.offset, Some(3));
    assert_eq!(scanner.tokens[2].location_info.length, Some(1));
    assert_eq!(scanner.tokens[2].whitespace_precedes, false);
    assert_eq!(scanner.tokens[2].whitespace_follows, false);

    assert_eq!(scanner.tokens[3].token_type, TokenType::PlusEqual);
    assert_eq!(scanner.tokens[3].lexeme, Some(String::from("+=")));
    assert_eq!(scanner.tokens[3].location_info.offset, Some(4));
    assert_eq!(scanner.tokens[3].location_info.length, Some(2));
    assert_eq!(scanner.tokens[3].whitespace_precedes, false);
    assert_eq!(scanner.tokens[3].whitespace_follows, false);

    assert_eq!(scanner.tokens[4].token_type, TokenType::Star);
    assert_eq!(scanner.tokens[4].lexeme, Some(String::from("*")));
    assert_eq!(scanner.tokens[4].location_info.offset, Some(6));
    assert_eq!(scanner.tokens[4].location_info.length, Some(1));
    assert_eq!(scanner.tokens[4].whitespace_precedes, false);
    assert_eq!(scanner.tokens[4].whitespace_follows, false);

    assert_eq!(scanner.tokens[5].token_type, TokenType::StarEqual);
    assert_eq!(scanner.tokens[5].lexeme, Some(String::from("*=")));
    assert_eq!(scanner.tokens[5].location_info.offset, Some(7));
    assert_eq!(scanner.tokens[5].location_info.length, Some(2));
    assert_eq!(scanner.tokens[5].whitespace_precedes, false);
    assert_eq!(scanner.tokens[5].whitespace_follows, false);

    assert_eq!(scanner.tokens[6].token_type, TokenType::Power);
    assert_eq!(scanner.tokens[6].lexeme, Some(String::from("^")));
    assert_eq!(scanner.tokens[6].location_info.offset, Some(9));
    assert_eq!(scanner.tokens[6].location_info.length, Some(1));
    assert_eq!(scanner.tokens[6].whitespace_precedes, false);
    assert_eq!(scanner.tokens[6].whitespace_follows, false);

    assert_eq!(scanner.tokens[7].token_type, TokenType::PowerEqual);
    assert_eq!(scanner.tokens[7].lexeme, Some(String::from("^=")));
    assert_eq!(scanner.tokens[7].location_info.offset, Some(10));
    assert_eq!(scanner.tokens[7].location_info.length, Some(2));
    assert_eq!(scanner.tokens[7].whitespace_precedes, false);
    assert_eq!(scanner.tokens[7].whitespace_follows, false);

    assert_eq!(scanner.tokens[8].token_type, TokenType::Slash);
    assert_eq!(scanner.tokens[8].lexeme, Some(String::from("/")));
    assert_eq!(scanner.tokens[8].location_info.offset, Some(12));
    assert_eq!(scanner.tokens[8].location_info.length, Some(1));
    assert_eq!(scanner.tokens[8].whitespace_precedes, false);
    assert_eq!(scanner.tokens[8].whitespace_follows, true);

    assert_eq!(scanner.tokens[9].token_type, TokenType::SlashEqual);
    assert_eq!(scanner.tokens[9].lexeme, Some(String::from("/=")));
    assert_eq!(scanner.tokens[9].location_info.offset, Some(14));
    assert_eq!(scanner.tokens[9].location_info.length, Some(2));
    assert_eq!(scanner.tokens[9].whitespace_precedes, true);
    assert_eq!(scanner.tokens[9].whitespace_follows, false);

    assert_eq!(scanner.tokens[10].token_type, TokenType::Modulo);
    assert_eq!(scanner.tokens[10].lexeme, Some(String::from("%")));
    assert_eq!(scanner.tokens[10].location_info.offset, Some(16));
    assert_eq!(scanner.tokens[10].location_info.length, Some(1));
    assert_eq!(scanner.tokens[10].whitespace_precedes, false);
    assert_eq!(scanner.tokens[10].whitespace_follows, false);

    assert_eq!(scanner.tokens[11].token_type, TokenType::ModuloEqual);
    assert_eq!(scanner.tokens[11].lexeme, Some(String::from("%=")));
    assert_eq!(scanner.tokens[11].location_info.offset, Some(17));
    assert_eq!(scanner.tokens[11].location_info.length, Some(2));
    assert_eq!(scanner.tokens[11].whitespace_precedes, false);
    assert_eq!(scanner.tokens[11].whitespace_follows, false);

    assert_eq!(scanner.tokens[12].token_type, TokenType::Assign);
    assert_eq!(scanner.tokens[12].lexeme, Some(String::from(":=")));
    assert_eq!(scanner.tokens[12].location_info.offset, Some(19));
    assert_eq!(scanner.tokens[12].location_info.length, Some(2));
    assert_eq!(scanner.tokens[12].whitespace_precedes, false);
    assert_eq!(scanner.tokens[12].whitespace_follows, false);

    assert_eq!(scanner.tokens[13].token_type, TokenType::Equal);
    assert_eq!(scanner.tokens[13].lexeme, Some(String::from("==")));
    assert_eq!(scanner.tokens[13].location_info.offset, Some(21));
    assert_eq!(scanner.tokens[13].location_info.length, Some(2));
    assert_eq!(scanner.tokens[13].whitespace_precedes, false);
    assert_eq!(scanner.tokens[13].whitespace_follows, false);

    assert_eq!(scanner.tokens[14].token_type, TokenType::LessThan);
    assert_eq!(scanner.tokens[14].lexeme, Some(String::from("<")));
    assert_eq!(scanner.tokens[14].location_info.offset, Some(23));
    assert_eq!(scanner.tokens[14].location_info.length, Some(1));
    assert_eq!(scanner.tokens[14].whitespace_precedes, false);
    assert_eq!(scanner.tokens[14].whitespace_follows, false);

    assert_eq!(scanner.tokens[15].token_type, TokenType::LessThanOrEqual);
    assert_eq!(scanner.tokens[15].lexeme, Some(String::from("<=")));
    assert_eq!(scanner.tokens[15].location_info.offset, Some(24));
    assert_eq!(scanner.tokens[15].location_info.length, Some(2));
    assert_eq!(scanner.tokens[15].whitespace_precedes, false);
    assert_eq!(scanner.tokens[15].whitespace_follows, false);

    assert_eq!(scanner.tokens[16].token_type, TokenType::GreaterThan);
    assert_eq!(scanner.tokens[16].lexeme, Some(String::from(">")));
    assert_eq!(scanner.tokens[16].location_info.offset, Some(26));
    assert_eq!(scanner.tokens[16].location_info.length, Some(1));
    assert_eq!(scanner.tokens[16].whitespace_precedes, false);
    assert_eq!(scanner.tokens[16].whitespace_follows, false);

    assert_eq!(scanner.tokens[17].token_type, TokenType::GreaterThanOrEqual);
    assert_eq!(scanner.tokens[17].lexeme, Some(String::from(">=")));
    assert_eq!(scanner.tokens[17].location_info.offset, Some(27));
    assert_eq!(scanner.tokens[17].location_info.length, Some(2));
    assert_eq!(scanner.tokens[17].whitespace_precedes, false);
    assert_eq!(scanner.tokens[17].whitespace_follows, true);
}

#[test]
fn test_keyword_operators() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("not and or");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    // Test each token
    assert_eq!(scanner.tokens[0].token_type, TokenType::Not);
    assert_eq!(scanner.tokens[0].lexeme, Some(String::from("not")));
    assert_eq!(scanner.tokens[0].location_info.offset, Some(0));
    assert_eq!(scanner.tokens[0].location_info.length, Some(3));
    assert_eq!(scanner.tokens[0].whitespace_precedes, false);
    assert_eq!(scanner.tokens[0].whitespace_follows, true);

    assert_eq!(scanner.tokens[1].token_type, TokenType::And);
    assert_eq!(scanner.tokens[1].lexeme, Some(String::from("and")));
    assert_eq!(scanner.tokens[1].location_info.offset, Some(4));
    assert_eq!(scanner.tokens[1].location_info.length, Some(3));
    assert_eq!(scanner.tokens[1].whitespace_precedes, true);
    assert_eq!(scanner.tokens[1].whitespace_follows, true);

    assert_eq!(scanner.tokens[2].token_type, TokenType::Or);
    assert_eq!(scanner.tokens[2].lexeme, Some(String::from("or")));
    assert_eq!(scanner.tokens[2].location_info.offset, Some(8));
    assert_eq!(scanner.tokens[2].location_info.length, Some(2));
    assert_eq!(scanner.tokens[2].whitespace_precedes, true);
    assert_eq!(scanner.tokens[2].whitespace_follows, true);
}

// -------------
// SYNTAX ERRORS
// -------------
#[test]
fn test_error_non_terminated_comment() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("[[");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::NonTerminatedCommentError)
            )
        }
    }
}

#[test]
fn test_error_malformed_hex_literal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("0x");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MalformedHexLiteralError)
            )
        }
    }
}

#[test]
fn test_error_malformed_binary_literal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("0b");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MalformedBinaryLiteralError)
            )
        }
    }
}

#[test]
fn test_error_malformed_sci_notation_literal() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("1e");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::MalformedSciNotationLiteralError)
            )
        }
    }
}

#[test]
fn test_error_non_terminated_string() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("\"");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::NonTerminatedStringError)
            )
        }
    }
}

#[test]
fn test_error_invalid_escape_sequence() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("\"\\w\"");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.error_was_reported(&ErrorType::InvalidEscapeSequenceError)
            )
        }
    }
}

#[test]
fn test_warning_unknown_character() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("~");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.warning_was_reported(&WarningType::UnknownCharacterWarning)
            )
        }
    }
}

#[test]
fn test_warning_snake_case_identifier() {
    let mut logger = Logger::new(None);
    let mut error = ErrorReporter::ConsoleErrorReporter(ConsoleErrorReporter::new(
        false, false, false, false, 0,
    ));
    let mut scanner = Scanner::new(vec![], LocationInfo::new(), &mut logger, &mut error);

    let source = String::from("test_id");
    scanner.location_info.repl_str = Some(source.clone());
    scanner.source = get_graphemes_from_line(source);

    scanner.scan();

    match scanner.error {
        ErrorReporter::ConsoleErrorReporter(console_error) => {
            assert_eq!(
                true,
                console_error.warning_was_reported(&WarningType::SnakeCaseIdentifierWarning)
            )
        }
    }
}
