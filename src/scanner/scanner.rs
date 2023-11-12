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

extern crate unicode_xid;

use std::collections::HashMap;

use crate::infrastructure::error::{ErrorMessage, ErrorReporter, ErrorType, WarningType};
use crate::infrastructure::file::LocationInfo;
use crate::infrastructure::log::Logger;

use crate::scanner::scanner_data::{Token, TokenType};

// This is the scanner, the part of the pipeline that takes a vector of characters and outputs a vector of Tokens
pub struct Scanner<'scan> {
    pub source: Vec<String>,
    pub tokens: Vec<Token>,
    pub location_info: LocationInfo,
    pub logger: &'scan mut Logger,
    pub error: &'scan mut ErrorReporter,

    start: usize,
    current: usize,
    keywords: HashMap<String, TokenType>,
}

impl Scanner<'_> {
    pub fn new<'scan>(
        source: Vec<String>,
        location_info: LocationInfo,
        logger: &'scan mut Logger,
        error: &'scan mut ErrorReporter,
    ) -> Scanner<'scan> {
        return Scanner {
            source,
            tokens: Vec::new(),
            location_info,
            logger,
            error,

            start: 0,
            current: 0,
            keywords: HashMap::new(),
        };
    }

    // Perform the scanning
    pub fn scan(&mut self) -> Vec<Token> {
        self.logger.log("");
        self.logger.log("--------------");
        self.logger.log("BEGIN Scanning");
        self.logger.log("--------------");

        // Populate the keyword map
        self.populate_keywords();

        // Scan a vector of tokens from the vector of characters
        self.scan_tokens();

        // Return the vector
        self.tokens.clone()
    }

    fn scan_tokens(&mut self) {
        while !self.is_at_end() {
            // We are at the beginning of the next token
            self.start = self.current;

            // Get the next token
            self.scan_token();
        }

        // Add an EOF token to the end of the vector to help make the parsing a little easier
        self.tokens.push(Token::new(
            TokenType::EOF,
            None,
            false,
            false,
            self.location_info.clone(),
        ));

        // Update EOF location info for error reporting purposes
        if self.tokens.len() > 1 {
            let previous_token = &self.tokens[self.tokens.len() - 2];
            let num_tokens = self.tokens.len();
            self.tokens[num_tokens - 1].location_info.offset = Some(
                match previous_token.location_info.offset {
                    None => 0,
                    Some(offset) => offset,
                } + match previous_token.location_info.length {
                    None => 0,
                    Some(length) => length,
                },
            );
            self.tokens[num_tokens - 1].location_info.length = Some(1)
        }
    }

    fn scan_token(&mut self) {
        match self.current_grapheme() {
            None => {
                panic!("No current grapheme");
            }
            Some(g) => {
                if self.is_id_start(&g) {
                    self.get_id_or_reserved()
                } else if self.is_digit(&g) {
                    self.get_number();
                } else if g == "\"" {
                    self.get_string_lit();
                } else if g == "(" {
                    self.add_token(TokenType::LeftParen)
                } else if g == ")" {
                    self.add_token(TokenType::RightParen)
                } else if g == "{" {
                    self.add_token(TokenType::LeftBrace)
                } else if g == "}" {
                    self.add_token(TokenType::RightBrace)
                } else if g == "," {
                    self.add_token(TokenType::Comma)
                } else if g == "." {
                    self.add_token(TokenType::Dot)
                } else if g == ";" {
                    self.add_token(TokenType::Semicolon)
                } else if g == ":" {
                    if self.check("=") {
                        self.add_token(TokenType::Assign);
                    } else {
                        self.add_token(TokenType::Colon);
                    }
                } else if g == "-" {
                    // If the next grapheme is an equals sign, we have a -= sign
                    if self.check("=") {
                        self.add_token(TokenType::MinusEqual);
                    } else {
                        self.add_token(TokenType::Minus);
                    }
                } else if g == "+" {
                    if self.check("=") {
                        self.add_token(TokenType::PlusEqual);
                    } else {
                        self.add_token(TokenType::Plus);
                    }
                } else if g == "/" {
                    if self.check("/") {
                        self.discard_one_line_comment();
                    } else if self.check("=") {
                        self.add_token(TokenType::SlashEqual);
                    } else {
                        self.add_token(TokenType::Slash);
                    }
                } else if g == "%" {
                    if self.check("=") {
                        self.add_token(TokenType::ModuloEqual);
                    } else {
                        self.add_token(TokenType::Modulo);
                    }
                } else if g == "*" {
                    if self.check("=") {
                        self.add_token(TokenType::StarEqual);
                    } else {
                        self.add_token(TokenType::Star);
                    }
                } else if g == "^" {
                    if self.check("=") {
                        self.add_token(TokenType::PowerEqual);
                    } else {
                        self.add_token(TokenType::Power);
                    }
                } else if g == "!" {
                    // If the next grapheme is an equals sign, we have a != token
                    if self.check("=") {
                        self.add_token(TokenType::NotEqual);
                    } else {
                        let error_type = ErrorType::UnrecognizedTokenError;

                        self.error.report(
                            error_type,
                            vec![ErrorMessage::new(
                                Some(format!(
                                    "{} \"{}\"",
                                    error_type.description(),
                                    self.get_lexeme()
                                )),
                                &self.new_location_info(1),
                            )],
                            None,
                        );
                    }
                } else if g == "=" {
                    // If the next grapheme is an equals sign, we have a == token
                    if self.check("=") {
                        self.add_token(TokenType::Equal);
                    } else {
                        let error_type = ErrorType::UnrecognizedTokenError;

                        self.error.report(
                            error_type,
                            vec![ErrorMessage::new(
                                Some(format!(
                                    "{} \"{}\"",
                                    error_type.description(),
                                    self.get_lexeme()
                                )),
                                &self.new_location_info(1),
                            )],
                            Some(String::from("Did you mean \":=\" or \"==\"?")),
                        );
                    }
                } else if g == "<" {
                    // If the next grapheme is an equals sign, we have a <= token
                    if self.check("=") {
                        self.add_token(TokenType::LessThanOrEqual);
                    } else {
                        self.add_token(TokenType::LessThan);
                    }
                } else if g == ">" {
                    // If the next grapheme is an equals sign, we have a >= token
                    if self.check("=") {
                        self.add_token(TokenType::GreaterThanOrEqual);
                    } else {
                        self.add_token(TokenType::GreaterThan);
                    }
                } else if g == "[" {
                    // If the next grapheme is also [, we are opening a multiline comment
                    if self.check("[") {
                        self.discard_multiline_comment();
                    }
                } else if self.is_whitespace(&g) {
                    self.discard_whitespace()
                } else {
                    let warning_type = WarningType::UnknownCharacterWarning;

                    self.error.warn(
                        warning_type,
                        vec![ErrorMessage::new(
                            Some(format!("{} '{}'", warning_type.description(), g)),
                            &self.new_location_info(1),
                        )],
                        None,
                    );
                }
            }
        }

        self.consume()
    }

    fn add_token(&mut self, token_type: TokenType) {
        let token = Token::new(
            token_type,
            Some(self.get_lexeme()),
            self.whitespace_follows(),
            self.whitespace_precedes(),
            self.new_location_info(self.get_length()),
        );

        self.logger.log(&format!("Token: {}", token.to_string()));

        self.tokens.push(token);
    }

    fn get_id_or_reserved(&mut self) {
        // Loop until we reach a non-id character or the end of the source
        loop {
            match self.current_grapheme() {
                None => break,
                Some(current) => {
                    if !self.is_id_continue(&current) {
                        break;
                    }
                }
            }

            self.consume();
        }
        // Since we looped until we got a non-id character,
        // we need to retreat back to the last id character
        self.retreat();

        // If this is a keyword, create a token for it,
        // but otherwise create an ID token
        let id_lexeme = self.get_lexeme();
        if !self.get_keyword(&id_lexeme) {
            if id_lexeme.contains("_") {
                self.error.warn(
                    WarningType::SnakeCaseIdentifierWarning,
                    vec![ErrorMessage::new(
                        Some(format!(
                            "Consider renaming this identifier to \"{}\"",
                            self.snake_to_kebab(id_lexeme)
                        )),
                        &self.new_location_info(self.get_length()),
                    )],
                    None,
                );
            }
            self.add_token(TokenType::ID);
        }
    }

    fn snake_to_kebab(&self, snake: String) -> String {
        let mut kebab_vec = vec![];

        for c in snake.chars() {
            if c == '_' {
                kebab_vec.push('-');
            } else {
                kebab_vec.push(c);
            }
        }

        let kebab: String = kebab_vec.into_iter().collect();

        return kebab;
    }

    fn get_keyword(&mut self, id: &str) -> bool {
        match self.keywords.get(id) {
            None => return false,
            Some(keyword) => {
                self.add_token(keyword.clone());
                return true;
            }
        }
    }

    fn populate_keywords(&mut self) {
        self.keywords.insert(String::from("let"), TokenType::Let);
        self.keywords.insert(String::from("mut"), TokenType::Mut);
        self.keywords.insert(String::from("i8"), TokenType::I8);
        self.keywords.insert(String::from("i16"), TokenType::I16);
        self.keywords.insert(String::from("i32"), TokenType::I32);
        self.keywords.insert(String::from("i64"), TokenType::I64);
        self.keywords.insert(String::from("i128"), TokenType::I128);
        self.keywords.insert(String::from("f32"), TokenType::F32);
        self.keywords.insert(String::from("f64"), TokenType::F64);
        self.keywords.insert(String::from("true"), TokenType::True);
        self.keywords
            .insert(String::from("false"), TokenType::False);
        self.keywords.insert(String::from("not"), TokenType::Not);
        self.keywords.insert(String::from("and"), TokenType::And);
        self.keywords.insert(String::from("or"), TokenType::Or);
    }

    fn get_number(&mut self) {
        match self.current_grapheme() {
            None => panic!("Scanner::get_number() called with no current grapheme"),
            Some(first) => {
                // If the first digit is a 0,
                // there's the possibility that we have a hex or binary literal
                if first == "0" {
                    match self.next_grapheme() {
                        None => {
                            // If this single 0 is the last grapheme in the source,
                            // we just have a simple integer literal token
                            self.add_token(TokenType::IntLit);
                        }
                        Some(second) => {
                            if second == "x" {
                                // If the first two graphemes are "0x",
                                // this is a hexadecimal literal
                                //
                                // Consume the 0x
                                self.consume();
                                self.consume();
                                self.get_hex_lit();
                            } else if second == "b" {
                                // If the first two graphemes are "0b",
                                // this is a binary literal
                                //
                                // Consume the 0b
                                self.consume();
                                self.consume();
                                self.get_binary_lit();
                            } else if self.is_digit(&second) || second == "_" {
                                // If the second grapheme is also a digit or an underscore,
                                // we either have an integer or float literal that starts with 0
                                self.get_int_or_float_lit();
                            } else {
                                // Otherwise, this must be a character from some other token,
                                // so we just have a 0
                                self.add_token(TokenType::IntLit);
                            }
                        }
                    }
                } else {
                    // Otherwise, we just have either an integer or float literal
                    self.get_int_or_float_lit();
                }
            }
        }

        // If the last token is a number literal, go through it and remove any underscores,
        // since they are only for the user's visibility
        if self.tokens.len() > 0 {
            let last_token = &self.tokens[self.tokens.len() - 1];
            if last_token.token_type == TokenType::IntLit
                || last_token.token_type == TokenType::FloatLit
                || last_token.token_type == TokenType::HexLit
                || last_token.token_type == TokenType::BinaryLit
            {
                match &last_token.lexeme {
                    None => {}
                    Some(lexeme) => {
                        let mut has_underscore = false;
                        let mut lexeme_vec = vec![];
                        for c in lexeme.chars() {
                            if c != '_' {
                                lexeme_vec.push(c);
                            } else {
                                has_underscore = true;
                            }
                        }
                        let new_lexeme: String = lexeme_vec.into_iter().collect();
                        let num_tokens = self.tokens.len();

                        // It's important that we don't update the length of the token,
                        // because we need that to reflect the length of the literal
                        // in the source for error/warning reporting
                        self.tokens[num_tokens - 1].lexeme = Some(new_lexeme);

                        if has_underscore {
                            self.logger.log(&format!(
                                "\tStripped underscore(s): {}",
                                self.tokens[self.tokens.len() - 1].to_string()
                            ));
                        }
                    }
                }
            }
        }
    }

    fn get_hex_lit(&mut self) {
        // Make sure we actually scan a digit,
        // so we don't end up with something like "0x" or "0x_____"
        let mut contains_digit = false;

        // Consume digits until we reach a non-hex digit or the end of the source
        loop {
            match self.current_grapheme() {
                None => break,
                Some(g) => {
                    if !self.is_hex_digit(&g) && g != "_" {
                        break;
                    } else {
                        if self.is_hex_digit(&g) {
                            contains_digit = true;
                        }
                    }
                }
            }

            self.consume();
        }

        // We consumed until we reached a non-hex grapheme, so retreat by one
        self.retreat();

        if contains_digit {
            self.add_token(TokenType::HexLit);
        } else {
            self.error.report(
                ErrorType::MalformedHexLiteralError,
                vec![ErrorMessage::new(
                    Some(String::from(
                        "Try including at least one hex digit (0-9 or A-F) after the \"0x\"",
                    )),
                    &self.new_location_info(self.get_length()),
                )],
                None,
            );
        }
    }

    fn get_binary_lit(&mut self) {
        // Make sure we actually scan a digit,
        // so we don't end up with something like "0b" or "0b_____"
        let mut contains_digit = false;

        // Consume digits until we reach a non-binary digit or the end of the source
        loop {
            match self.current_grapheme() {
                None => break,
                Some(g) => {
                    if !self.is_binary_digit(&g) && g != "_" {
                        break;
                    } else {
                        if self.is_binary_digit(&g) {
                            contains_digit = true;
                        }
                    }
                }
            }

            self.consume();
        }

        // We consumed until we reached a non-binary grapheme, so retreat by one
        self.retreat();

        if contains_digit {
            self.add_token(TokenType::BinaryLit);
        } else {
            self.error.report(
                ErrorType::MalformedBinaryLiteralError,
                vec![ErrorMessage::new(
                    Some(String::from(
                        "Try including at least one binary digit (0 or 1) after the \"0b\"",
                    )),
                    &self.new_location_info(self.get_length()),
                )],
                None,
            );
        }
    }

    fn get_int_or_float_lit(&mut self) {
        let mut is_int = true;

        // Consume graphemes until we reach a non-digit or the end of the source
        loop {
            match self.current_grapheme() {
                None => break,
                Some(g) => {
                    if !self.is_digit(&g) && g != "_" {
                        if g == "." {
                            is_int = false;
                            self.consume();
                            continue;
                        } else if g == "e" || g == "E" {
                            is_int = false;
                            self.consume();
                            match self.current_grapheme() {
                                None => {
                                    self.error.report(
                                        ErrorType::MalformedSciNotationLiteralError,
                                        vec![ErrorMessage::new(
                                            Some(String::from(
                                                "Try including a positive or negative integer (e.g. 2 or -50) after the \"e\"",
                                            )),
                                            &self.new_location_info(self.get_length()),
                                        )],
                                        None,
                                    );
                                }
                                Some(next) => {
                                    if next == "-" || self.is_digit(&next) {
                                        self.consume();
                                        continue;
                                    } else {
                                        self.error.report(
                                            ErrorType::MalformedSciNotationLiteralError,
                                            vec![ErrorMessage::new(
                                                Some(String::from(
                                                    "Try including a positive or negative integer (e.g. 2 or -50) after the \"e\"",
                                                )),
                                                &self.new_location_info(self.get_length()),
                                            )],
                                            None,
                                        );
                                    }
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
            }

            self.consume();
        }

        // We consumed until we reached a non-digit grapheme, so retreat by one
        self.retreat();

        if is_int {
            self.add_token(TokenType::IntLit);
        } else {
            self.add_token(TokenType::FloatLit);
        }
    }

    fn get_string_lit(&mut self) {
        // The current grapheme is an opening quotation mark
        self.consume();

        // We're going to build up our own lexeme so we can handle escape characters
        let mut lexeme_vec = vec![];

        // Loop until we reach the closing quotation mark
        loop {
            match self.current_grapheme() {
                None => {
                    // If we reach the end of the source, we have a unterminated string
                    let mut new_location_info = self.new_location_info(1);
                    new_location_info.offset = Some(self.current - 1);
                    self.error.report(
                        ErrorType::NonTerminatedStringError,
                        vec![
                            ErrorMessage::new(
                                Some(String::from("Expected '\"'")),
                                &new_location_info,
                            ),
                            ErrorMessage::new(
                                Some(String::from("String opened here")),
                                &self.new_location_info(1),
                            ),
                        ],
                        Some(String::from("Try closing the string with another '\"'")),
                    );

                    self.retreat();

                    break;
                }
                Some(g) => {
                    // If we've reached the closing quotation mark,
                    // break out of the loop
                    if g == "\"" {
                        break;
                    } else {
                        lexeme_vec.push(g.clone());

                        // If the character is a backslash,
                        // the user may be trying to create an escape character
                        if g == "\\" {
                            match self.next_grapheme() {
                                None => {
                                    // If we reach the end of the source, we have a unterminated string
                                    let mut new_location_info = self.new_location_info(1);
                                    new_location_info.offset = Some(self.current - 1);
                                    self.error.report(
                                        ErrorType::NonTerminatedStringError,
                                        vec![
                                            ErrorMessage::new(
                                                Some(String::from("Expected '\"'")),
                                                &new_location_info,
                                            ),
                                            ErrorMessage::new(
                                                Some(String::from("String opened here")),
                                                &self.new_location_info(1),
                                            ),
                                        ],
                                        Some(String::from(
                                            "Try closing the string with another '\"'",
                                        )),
                                    );

                                    self.retreat();

                                    break;
                                }
                                Some(next) => {
                                    if next == "n"
                                        || next == "r"
                                        || next == "t"
                                        || next == "\\"
                                        || next == "0"
                                        || next == "\""
                                    {
                                        lexeme_vec.push(String::from("\\"));
                                        if next == "\\" {
                                            lexeme_vec.push(String::from("\\"))
                                        }
                                        lexeme_vec.push(next);
                                        self.consume();
                                    } else {
                                        lexeme_vec.push(next.clone());
                                        let mut new_location_info = self.new_location_info(2);
                                        new_location_info.offset = Some(self.current);
                                        self.error.report(
                                            ErrorType::InvalidEscapeSequenceError,
                                            vec![ErrorMessage::new(
                                                Some(format!(
                                                    "Invalid escape sequence \"\\{}\"",
                                                    next
                                                )),
                                                &new_location_info,
                                            )],
                                            None,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }

            self.consume();
        }

        self.add_token(TokenType::StringLit);
        // Manually update the lexeme to the one we created
        let lexeme: String = lexeme_vec.join("");
        let num_tokens = self.tokens.len();
        self.tokens[num_tokens - 1].lexeme = Some(lexeme);
        self.logger.log(&format!(
            "\tUpdated string lexeme: {}",
            self.tokens[num_tokens - 1].to_string()
        ));
    }

    fn get_lexeme(&self) -> String {
        let lexeme_vec = &self.source[self.start..self.current + 1];
        return lexeme_vec.join("");
    }

    fn discard_one_line_comment(&mut self) {
        // Continue looping until a newline
        loop {
            match self.current_grapheme() {
                None => break,
                Some(current) => {
                    if current == "\n" {
                        break;
                    }
                }
            }

            // Consume and discard the comment grapheme
            self.consume();
        }

        self.logger.log(&format!(
            "Discarding one line comment, length {} including newline",
            self.get_length()
        ));
    }

    fn discard_multiline_comment(&mut self) {
        // Keep track of the nested multiline comment depth
        // (initially 1 because we had to match [[ to be here to begin with)
        let mut depth = 1;

        // Loop until we find the outermost ]]
        loop {
            match self.current_grapheme() {
                // If we've reached the end of the source, break out of the loop
                None => break,
                Some(current) => {
                    // If the current grapheme is a left bracket, check the next grapheme
                    if current == "[" {
                        match self.next_grapheme() {
                            None => self.error.report(
                                ErrorType::NonTerminatedCommentError,
                                vec![ErrorMessage::new(
                                    Some(format!(
                                        "The comment opened here has not been terminated"
                                    )),
                                    &self.new_location_info(2),
                                )],
                                Some(String::from("Try terminating the comment with \"]]\"")),
                            ),
                            Some(next) => {
                                if next == "[" {
                                    self.logger.log(&format!(
                                        "Found nested multiline comment, entering comment depth {}",
                                        depth + 1
                                    ));
                                    // Increment the comment depth
                                    depth += 1;
                                    // Consume the first grapheme (we'll consume the second one at
                                    // the end of the loop)
                                    self.consume();
                                }
                            }
                        }
                    } else if current == "]" {
                        match self.next_grapheme() {
                            None => self.error.report(
                                ErrorType::NonTerminatedCommentError,
                                vec![ErrorMessage::new(
                                    Some(format!(
                                        "The comment opened here has not been terminated"
                                    )),
                                    &self.new_location_info(2),
                                )],
                                Some(String::from("Try terminating the comment with \"]]\"")),
                            ),
                            Some(next) => {
                                if next == "]" {
                                    // Decrement the comment depth
                                    depth -= 1;
                                    // Consume the first grapheme (we'll consume the second one at
                                    // the end of the loop)
                                    self.consume();
                                }
                            }
                        }
                    }
                }
            }
            self.consume();

            // If we've exited the outermost comment, so break out of the loop
            if depth == 0 {
                // Since we need to be pointing to the last grapheme of the token
                // when done lexing it (or in this case, the grapheme before
                // the next token), we need to retreat one grapheme
                self.retreat();

                break;
            }
        }

        // If we exited out of the loop because we reached the end of the source
        // and not because we exited the outermost comment,
        // throw an error for an unterminated comment
        if depth > 0 {
            self.error.report(
                ErrorType::NonTerminatedCommentError,
                vec![ErrorMessage::new(
                    Some(format!("The comment opened here has not been terminated")),
                    &self.new_location_info(2),
                )],
                Some(String::from("Try terminating the comment with \"]]\"")),
            )
        } else {
            self.logger.log(&format!(
                "Discarding multiline comment, length {}",
                self.get_length()
            ));
        }
    }

    fn discard_whitespace(&mut self) {
        // Loop until the whitespace has been consumed,
        // or we reach the end of the source,
        // whichever comes first
        loop {
            match self.current_grapheme() {
                None => break,
                Some(current) => {
                    if !self.is_whitespace(&current) {
                        break;
                    }
                }
            }
            // Consume and discard the whitespace grapheme
            self.consume();
        }
        // Since we had to consume until we reach a non-whitespace character, we need to retreat by
        // one grapheme so that we're pointing to the last whitespace character
        self.retreat();
    }

    fn new_location_info(&self, length: usize) -> LocationInfo {
        let mut location_info = self.location_info.clone();

        location_info.offset = Some(self.start);
        location_info.length = Some(length);

        return location_info;
    }

    fn is_whitespace(&self, g: &str) -> bool {
        let c = g.chars().next().unwrap();

        return c == '\u{0009}'  // \t
            || c == '\u{000A}'  // \n
            || c == '\u{000B}'  // Vertical tab
            || c == '\u{000C}'  // Form feed
            || c == '\u{000D}'  // \r
            || c == '\u{0020}'  // Space

            // NEXT LINE from latin1
            || c == '\u{0085}'

            // Bidi markers
            || c == '\u{200E}'  // Left-to-right mark
            || c == '\u{200F}'  // Right-to-left mark

            // Dedicated whitespace characters from Unicode
            || c == '\u{2028}'  // Line separator
            || c == '\u{2029}'; // Paragraph separator
    }

    fn is_id_start(&self, g: &str) -> bool {
        // If the grapheme is a unicode XID_Start character
        return unicode_xid::UnicodeXID::is_xid_start(g.chars().next().unwrap());
    }

    fn is_id_continue(&self, g: &str) -> bool {
        let c = g.chars().next().unwrap();

        // If the grapheme is a unicode XID_Continue character or a hyphen
        return unicode_xid::UnicodeXID::is_xid_continue(c) || self.is_hyphen(c);
        // Various hyphen characters
    }

    fn is_hyphen(&self, c: char) -> bool {
        return c == '\u{002D}'
            || c == '\u{058A}'
            || c == '\u{05BE}'
            || c == '\u{1400}'
            || c == '\u{1806}'
            || c == '\u{2010}'
            || c == '\u{2011}'
            || c == '\u{2012}'
            || c == '\u{2013}'
            || c == '\u{2014}'
            || c == '\u{2015}'
            || c == '\u{2E17}'
            || c == '\u{2E1A}'
            || c == '\u{2E3A}'
            || c == '\u{2E3B}'
            || c == '\u{2E40}'
            || c == '\u{301C}'
            || c == '\u{3030}'
            || c == '\u{30A0}'
            || c == '\u{FE31}'
            || c == '\u{FE32}'
            || c == '\u{FE58}'
            || c == '\u{FE63}'
            || c == '\u{FF0D}'
            || c == '\u{10EAD}';
    }

    fn is_digit(&self, g: &str) -> bool {
        return g == "0"
            || g == "1"
            || g == "2"
            || g == "3"
            || g == "4"
            || g == "5"
            || g == "6"
            || g == "7"
            || g == "8"
            || g == "9";
    }

    fn is_hex_digit(&self, g: &str) -> bool {
        return self.is_digit(g)
            || g == "a"
            || g == "b"
            || g == "c"
            || g == "d"
            || g == "e"
            || g == "f"
            || g == "A"
            || g == "B"
            || g == "C"
            || g == "D"
            || g == "E"
            || g == "F";
    }

    fn is_binary_digit(&self, g: &str) -> bool {
        return g == "0" || g == "1";
    }

    fn whitespace_follows(&self) -> bool {
        match self.next_grapheme() {
            // If there is no next grapheme, obviously whitespace does not follow this token
            None => return false,
            Some(next) => return self.is_whitespace(&next),
        }
    }

    fn whitespace_precedes(&self) -> bool {
        let mut whitespace_precedes = false;
        // If this token starts at index 1 or higher, check to see if the grapheme
        // directly proceeding it is whitespace. If this token starts at 0,
        // there are no graphemes proceeding this token, so of course whitespace doesn't
        // precede this token
        if self.start >= 1 {
            // If the grapheme directly before the first grapheme of the current token
            // is whitespace, set whitespace_precedes to true
            whitespace_precedes = self.is_whitespace(&self.source[self.start - 1]);
        }

        return whitespace_precedes;
    }

    fn consume(&mut self) {
        // Move on to the next grapheme
        self.current += 1;
    }

    fn check(&mut self, g: &str) -> bool {
        // Probe the next grapheme
        match self.next_grapheme() {
            None => {
                // If there is no next grapheme, clearly it doesn't match the given one
                return false;
            }
            Some(next) => {
                // If the next grapheme doesn't match the given one
                if next != g {
                    return false;
                }

                // Otherwise, the next grapheme does match the given one,
                // so consume it and return true
                self.consume();
                return true;
            }
        }
    }

    fn retreat(&mut self) {
        // Retreat to the previous grapheme
        self.current -= 1;
    }

    fn current_grapheme(&self) -> Option<String> {
        if !self.is_at_end() {
            return Some(self.source[self.current].clone());
        } else {
            return None;
        }
    }

    fn next_grapheme(&self) -> Option<String> {
        if !self.is_next_at_end() {
            return Some(self.source[self.current + 1].clone());
        } else {
            return None;
        }
    }

    fn get_length(&self) -> usize {
        return self.current + 1 - self.start;
    }

    fn is_at_end(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn is_next_at_end(&self) -> bool {
        return self.current + 1 >= self.source.len();
    }
}
