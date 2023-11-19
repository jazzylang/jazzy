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

use crate::infrastructure::file::LocationInfo;

#[derive(Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: Option<String>,
    pub whitespace_follows: bool,
    pub whitespace_precedes: bool,
    pub location_info: LocationInfo,
}

impl Token {
    pub fn new(
        token_type: TokenType,
        lexeme: Option<String>,
        whitespace_follows: bool,
        whitespace_precedes: bool,
        location_info: LocationInfo,
    ) -> Token {
        return Token {
            token_type,
            lexeme,
            whitespace_follows,
            whitespace_precedes,
            location_info,
        };
    }

    pub fn to_string(&self) -> String {
        match &self.lexeme {
            Some(lexeme) => match self.location_info.offset {
                Some(offset) => match self.location_info.length {
                    Some(length) => match &self.location_info.filename {
                        Some(filename) => {
                            return format!(
                                "{{{:?}: \"{}\", offset: {}, len: {}, wspace_follows: {}, wspace_precedes: {}, \"{}\"}}",
                                self.token_type,
                                self.get_lexeme(lexeme),
                                offset,
                                length,
                                self.whitespace_follows,
                                self.whitespace_precedes,
                                filename
                            );
                        }
                        None => match &self.location_info.repl_str {
                            Some(repl_str) => {
                                return format!(
                                    "{{{:?}: \"{}\", offset: {}, len: {}, wspace_follows: {}, wspace_precedes: {}, \"{}\"}}",
                                    self.token_type,
                                    self.get_lexeme(lexeme),
                                    offset,
                                    length,
                                    self.whitespace_follows,
                                    self.whitespace_precedes,
                                    repl_str
                                );
                            }
                            None => {
                                panic!("impl Token to_string(): Either filename or repl_str should be non-None, but neither are");
                            }
                        },
                    },
                    None => {
                        panic!("impl Token to_string(): No length even though you have offset, you should have both or none");
                    }
                },
                None => {
                    return format!("{{{:?}: \"{}\"}}", self.token_type, self.get_lexeme(lexeme));
                }
            },
            None => {
                return format!("{{{:?}}}", self.token_type);
            }
        }
    }

    fn get_lexeme(&self, lexeme: &str) -> String {
        let mut lexeme_vec = Vec::new();

        for c in lexeme.chars() {
            if c == '\n' {
                lexeme_vec.push(String::from("\\n"));
            } else if c == '\t' {
                lexeme_vec.push(String::from("\\t"));
            } else if c == '\r' {
                lexeme_vec.push(String::from("\\r"));
            } else {
                lexeme_vec.push(c.to_string());
            }
        }

        return lexeme_vec.join("");
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum TokenType {
    ID,
    Let,
    Mut,
    I8,
    I16,
    I32,
    I64,
    I128,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
    Bool,
    Str,
    True,
    False,
    IntLit,
    FloatLit,
    HexLit,
    BinaryLit,
    StringLit,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Minus,
    MinusEqual,
    Plus,
    PlusEqual,
    Slash,
    SlashEqual,
    Modulo,
    ModuloEqual,
    Star,
    StarEqual,
    Power,
    PowerEqual,
    Not,
    NotEqual,
    And,
    Or,
    Assign,
    BadAssign,
    Equal,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    EOF,
}
