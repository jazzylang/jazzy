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

use std::{
    cmp::Ordering,
    collections::{BTreeMap, HashMap},
};

use colored::Colorize;

use crate::infrastructure::file::{get_graphemes_from_line, read_lines, LocationInfo};

#[derive(Clone, Copy)]
pub enum ErrorOrWarningType {
    ErrorType(ErrorType),
    WarningType(WarningType),
}

impl ErrorOrWarningType {
    // Error or warning identifier, e.g.
    //     Error: [E0011] Incompatible types
    pub fn identifier(&self, warnings_as_errors: bool) -> String {
        match self {
            ErrorOrWarningType::ErrorType(error_type) => {
                return format!("{} {}", "Error:".red(), error_type.message());
            }
            ErrorOrWarningType::WarningType(warning_type) => {
                if warnings_as_errors {
                    return format!("{} {}", "Warning:".red(), warning_type.message());
                } else {
                    return format!("{} {}", "Warning:".yellow(), warning_type.message());
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum ErrorType {
    NonTerminatedCommentError,
    UnrecognizedTokenError,
    MalformedHexLiteralError,
    MalformedBinaryLiteralError,
    MalformedSciNotationLiteralError,
    NonTerminatedStringError,
    InvalidEscapeSequenceError,
    UnMatchedParenError,
    MissingPaddingAroundOperatorError,
    ExpectedExpressionError,
    IllegalImplicitCastError,
    IncompatibleTypesError,
    OverflowError,
}

impl ErrorType {
    pub fn message(&self) -> String {
        match self {
            // These error codes (e.g. E0000) shall never be changed,
            // just to make everyone's lives easier when searching them up
            //
            // It doesn't really matter what number any given error is as long as it stays the same,
            // so don't worry if some seemingly really important or fundamental error
            // has a larger code than some seemingly niche less important error
            ErrorType::NonTerminatedCommentError => format!("[E0001] {}", self.description()),
            ErrorType::UnrecognizedTokenError => format!("[E0002] {}", self.description()),
            ErrorType::MalformedHexLiteralError => format!("[E0003] {}", self.description()),
            ErrorType::MalformedBinaryLiteralError => format!("[E0004] {}", self.description()),
            ErrorType::MalformedSciNotationLiteralError => {
                format!("[E0005] {}", self.description())
            }
            ErrorType::NonTerminatedStringError => format!("[E0005] {}", self.description()),
            ErrorType::InvalidEscapeSequenceError => format!("[E0006] {}", self.description()),
            ErrorType::UnMatchedParenError => format!("[E0007] {}", self.description()),
            ErrorType::MissingPaddingAroundOperatorError => {
                format!("[E0008] {}", self.description())
            }
            ErrorType::ExpectedExpressionError => format!("[E0009] {}", self.description()),
            ErrorType::IllegalImplicitCastError => format!("[E0010] {}", self.description()),
            ErrorType::IncompatibleTypesError => format!("[E0011] {}", self.description()),
            ErrorType::OverflowError => format!("[E0012] {}", self.description()),
        }
    }

    pub fn description(&self) -> String {
        match self {
            ErrorType::NonTerminatedCommentError => String::from("Non-terminated comment"),
            ErrorType::UnrecognizedTokenError => String::from("Unrecognized token"),
            ErrorType::MalformedHexLiteralError => String::from("Malformed hexadecimal literal"),
            ErrorType::MalformedBinaryLiteralError => String::from("Malformed binary literal"),
            ErrorType::MalformedSciNotationLiteralError => {
                String::from("Malformed scientific notation literal")
            }
            ErrorType::NonTerminatedStringError => String::from("Non-terminated string"),
            ErrorType::InvalidEscapeSequenceError => String::from("Invalid escape sequence"),
            ErrorType::UnMatchedParenError => String::from("Un-matched parenthesis"),
            ErrorType::MissingPaddingAroundOperatorError => {
                String::from("Missing whitespace padding around binary operator")
            }
            ErrorType::ExpectedExpressionError => String::from("Expected expression"),
            ErrorType::IllegalImplicitCastError => String::from("Illegal implicit cast"),
            ErrorType::IncompatibleTypesError => String::from("Incompatible types"),
            ErrorType::OverflowError => String::from("Integer overflow"),
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum WarningType {
    UnknownCharacterWarning,
    SnakeCaseIdentifierWarning,
}

impl WarningType {
    pub fn message(&self) -> String {
        match self {
            // These warning codes (e.g. W0000) shall never be changed,
            // just to make everyone's lives easier when searching them up
            //
            // It doesn't really matter what number any given warning is as long as it stays the same,
            // so don't worry if some seemingly really important or fundamental warning
            // has a larger code than some seemingly niche less important warning
            WarningType::UnknownCharacterWarning => format!("[W0001] {}", self.description()),
            WarningType::SnakeCaseIdentifierWarning => format!("[W0002] {}", self.description()),
        }
    }

    pub fn description(&self) -> String {
        match self {
            WarningType::UnknownCharacterWarning => String::from("Unknown character"),
            WarningType::SnakeCaseIdentifierWarning => String::from("Snake case in identifier"),
        }
    }
}

pub enum ErrorReporter {
    ConsoleErrorReporter(ConsoleErrorReporter),
}

impl ErrorReporter {
    pub fn report(
        &mut self,
        error_type: ErrorType,
        messages: Vec<ErrorMessage>,
        hint: Option<String>,
    ) {
        match self {
            ErrorReporter::ConsoleErrorReporter(console_error) => {
                console_error.report(error_type, messages, hint);
            }
        }
    }

    pub fn warn(
        &mut self,
        warning_type: WarningType,
        messages: Vec<ErrorMessage>,
        hint: Option<String>,
    ) {
        match self {
            ErrorReporter::ConsoleErrorReporter(console_error) => {
                console_error.warn(warning_type, messages, hint);
            }
        }
    }
}

pub struct ConsoleErrorReporter {
    pub has_error: bool,
    pub error_num: usize,
    pub warning_num: usize,
    pub max_errors_or_warnings: usize,
    pub warnings_as_errors: bool,
    pub show_all_errors: bool,
    pub show_all_warnings: bool,
    pub show_all_errors_and_warnings: bool,

    pub errors_reported: Vec<ErrorType>,
    pub warnings_reported: Vec<WarningType>,
}

impl ConsoleErrorReporter {
    pub fn new(
        warnings_as_errors: bool,
        show_all_errors: bool,
        show_all_warnings: bool,
        show_all_errors_and_warnings: bool,
        max_errors_or_warnings: usize,
    ) -> ConsoleErrorReporter {
        return ConsoleErrorReporter {
            has_error: false,
            error_num: 0,
            warning_num: 0,
            max_errors_or_warnings,
            warnings_as_errors,
            show_all_errors,
            show_all_warnings,
            show_all_errors_and_warnings,
            errors_reported: vec![],
            warnings_reported: vec![],
        };
    }

    pub fn error_was_reported(&self, error_type: &ErrorType) -> bool {
        return self.errors_reported.contains(error_type);
    }

    pub fn warning_was_reported(&self, warning_type: &WarningType) -> bool {
        return self.warnings_reported.contains(warning_type);
    }

    pub fn clear_for_next_execution(&mut self) {
        self.has_error = false;
        self.error_num = 0;
        self.warning_num = 0;
    }

    fn report(&mut self, error_type: ErrorType, messages: Vec<ErrorMessage>, hint: Option<String>) {
        self.errors_reported.push(error_type);

        self.has_error = true;

        self.error_num += 1;
        if self.error_num + self.warning_num > self.max_errors_or_warnings {
            return;
        }

        self.report_error_or_warning(ErrorOrWarningType::ErrorType(error_type), messages, hint);
    }

    fn warn(
        &mut self,
        warning_type: WarningType,
        messages: Vec<ErrorMessage>,
        hint: Option<String>,
    ) {
        self.warnings_reported.push(warning_type);

        if self.warnings_as_errors {
            self.has_error = true;
        }

        self.warning_num += 1;
        if self.error_num + self.warning_num > self.max_errors_or_warnings {
            return;
        }

        self.report_error_or_warning(
            ErrorOrWarningType::WarningType(warning_type),
            messages,
            hint,
        );
    }

    pub fn report_error_or_warning(
        &mut self,
        error_or_warning_type: ErrorOrWarningType,
        messages: Vec<ErrorMessage>,
        hint: Option<String>,
    ) {
        // An error or warning consists of three sections:
        // 1. The error or warning identifier
        //        Error: [E0011] Incompatible types
        //
        // 2. One or more error snippets, one for each REPL line or source file, each of which consist of:
        //    a. An optional message describing the file path and location
        //    where the error or warning occurred
        //           In /some/path/main.jzy:5:1:
        //    b. One or more lines of source code
        //           4 | let val := true;
        //           5 | val + 1
        //    c. One or more location indicators and optional messages
        //               ^^^ ^ Expression with type 'bool' is incompatible with expression '+'
        //               ||| 'bool'
        //
        // 3. An optional hint
        //        Hint: idk, do better

        // 1. The error or warning identifier
        eprintln!(""); // Extra newline
        eprintln!(
            "{}",
            error_or_warning_type.identifier(self.warnings_as_errors)
        );
        // We put the extra newline right before each snippet, not here

        // 2. One or more error snippets, one for each REPL line or source file
        // Sort the messages into structured snippet data
        let snippet_data = SnippetData::new(messages);
        self.display_snippets(snippet_data);

        // 3. An optional hint
        eprintln!(""); // Extra newline
        match hint {
            Some(hint) => eprintln!("{} {}", "Hint:".green(), hint),
            None => {}
        };
        eprintln!(""); // Extra newline
    }

    pub fn display_snippets(&self, snippet_data: SnippetData) {
        // If we have any REPL snippets, those need to be displayed first
        self.display_repl_snippets(snippet_data.repl_snippets);

        // Next, we need to display any file snippets
        self.display_file_snippets(snippet_data.file_snippets);
    }

    pub fn display_repl_snippets(&self, snippets: HashMap<String, REPLSnippet>) {
        // Loop through all of the REPL snippets
        for (repl_line, repl_snippet) in snippets.iter() {
            eprintln!(""); // Extra newline

            // Each REPLSnippet is a vector of REPLMessages
            // which appear on a single REPL line,
            // so the first thing to do is display that line!
            eprintln!("{} {}", "|".bright_black(), repl_line);

            // Next, we need to display all of the location indicators and optional messages
            //
            // Since the REPLMessages are already sorted by the column number
            // that they start on, we want to iterate through them backwards
            // so we get an effect like this:
            // | gross problem line
            //   ^^^^ ^^ ^ ^^^^ message with largest column val
            //   |||| || |
            //   |||| ||
            //   |||| message with smallest column val

            // We will start with the first line
            let mut first_indicator_row = String::from("");

            // Loop forwards through the snippet
            for other_file_message in repl_snippet {
                // First, add the correct amount of whitespace between the previous indicator
                // (or the beginning of the line) and this one
                //
                // That is equal to the offset of this token from the beginning of the line
                // (which is column - 1)
                // minus the current length of the indicator row
                first_indicator_row.push_str(
                    &" ".repeat((other_file_message.column - 1) - first_indicator_row.len()),
                );

                // Next, if there is a message attached to this indicator,
                // we just need a single arrow, but otherwise we want to underline it fully
                match other_file_message.message {
                    None => {
                        first_indicator_row.push_str(&"^".repeat(other_file_message.length));
                    }
                    Some(_) => {
                        first_indicator_row.push_str("^");
                        first_indicator_row.push_str(&" ".repeat(other_file_message.length - 1));
                    }
                }
            }

            // Print the first indicator row
            eprintln!("  {}", first_indicator_row.bright_black());

            // Now do the rest of the lines
            for repl_message in repl_snippet.iter().rev() {
                // We only need a row if there's a message
                match &repl_message.message {
                    None => continue,
                    Some(message) => {
                        // Initialize a row to hold the indicators and messages
                        let mut indicator_row = String::from("");

                        // Loop forwards through the snippet until we reach the current message
                        for other_repl_message in repl_snippet {
                            // First, add the correct amount of whitespace between the previous indicator
                            // (or the beginning of the line) and this one
                            //
                            // That is equal to the offset of this token from the beginning of the line
                            // (which is column - 1)
                            // minus the current length of the indicator row
                            indicator_row.push_str(
                                &" ".repeat((other_repl_message.column - 1) - indicator_row.len()),
                            );

                            // Add a bar for the arrow if this one has a message
                            match other_repl_message.message {
                                Some(_) => indicator_row.push('|'),
                                None => {}
                            }
                            // If we've displayed all of the indicators, move on to print the message
                            if other_repl_message == repl_message {
                                break;
                            } else {
                                // Otherwise, add enough whitespace to take up the rest of the characters
                                // in the token
                                indicator_row.push_str(&" ".repeat(other_repl_message.length - 1));
                            }
                        }

                        // Finally, add the message to the end of the row!
                        indicator_row.push_str(&format!(" {}", message));

                        // Now we have a fully formed indicator line, so print it
                        // (add two spaces at the beginning to make it line up with the REPL line,
                        // which started after the "| ")
                        eprintln!("  {}", indicator_row.bright_black());
                    }
                }
            }
        }
    }

    pub fn display_file_snippets(&self, snippets: HashMap<String, FileSnippet>) {
        // Loop through all of the file snippets
        for (filename, file_snippet) in snippets.iter() {
            eprintln!(""); // Extra newline

            // Each FileSnippet is a BTreeMap of vectors of FileMessages and contains
            // all of the error or warnings messages contained within a single file
            //
            // Display the file path:
            eprintln!("In file {}:", filename);

            let (largest_line_num, _) = file_snippet.iter().next_back().unwrap();
            let largest_line_num = *largest_line_num;

            // Now we need to iterate through all of the problem lines in this file
            let mut last_line = None;
            for (line_num, messages_on_line) in file_snippet.iter() {
                let line_num = *line_num;
                // Since we only ever instantiated these lists with at least one
                // message in them, we can always safely get the first value
                // in order to look at the previous and problem lines
                let first_message = &messages_on_line[0];
                let previous_line = &first_message.previous_line;
                let problem_line = &first_message.problem_line;

                // Before we display the problem line, we need to figure out
                // whether we need to display the previous line first
                match last_line {
                    None => {
                        // This is the first line in the snippet,
                        // so if there is a previous line, display it
                        match previous_line {
                            Some(previous_line) => {
                                self.display_line(line_num - 1, previous_line, largest_line_num);
                            }
                            None => {}
                        }
                    }
                    Some(last_line) => {
                        // We have already displayed a line before this one
                        //
                        // If the last problem line was two lines before our current problem line,
                        // we can display the previous line in between to connect the two
                        if last_line + 2 == line_num {
                            match previous_line {
                                Some(previous_line) => {
                                    self.display_line(line_num - 1, previous_line, largest_line_num)
                                }
                                None => panic!(
                                    "No previous line when problem line is line {}",
                                    line_num
                                ),
                            }
                        }
                        // If the last problem line was more than two lines before our current
                        // problem line, we can display an ellipsis and then the previous line in
                        // between to connect the two
                        else if last_line + 2 < line_num {
                            eprintln!("{}", "...".bright_black());
                            match previous_line {
                                Some(previous_line) => {
                                    self.display_line(line_num - 1, previous_line, largest_line_num)
                                }
                                None => panic!(
                                    "No previous line when problem line is line {}",
                                    line_num
                                ),
                            }
                        }
                        // Otherwise, last_line = previous_line, so it's already been displayed
                    }
                }

                // Now we can display the problem line
                self.display_line(line_num, problem_line, largest_line_num);

                // Next, we need to display all of the location indicators and optional messages
                //
                // Since the FileMessages are already sorted by the column number
                // that they start on, we want to iterate through them backwards
                // so we get an effect like this:
                // 4 | this is the previous line
                // 5 | gross problem line
                //     ^^^^^ ^       ^
                //           |       | message with largest column val
                //           | message with smallest column val

                // We will start with the first line
                let mut first_indicator_row = String::from("");

                // Loop forwards through the snippet
                for other_file_message in messages_on_line {
                    // First, add the correct amount of whitespace between the previous indicator
                    // (or the beginning of the line) and this one
                    //
                    // That is equal to the offset of this token from the beginning of the line
                    // (which is column - 1)
                    // minus the current length of the indicator row
                    first_indicator_row.push_str(
                        &" ".repeat((other_file_message.column - 1) - first_indicator_row.len()),
                    );

                    // Next, if there is a message attached to this indicator,
                    // we just need a single arrow, but otherwise we want to underline it fully
                    match other_file_message.message {
                        None => {
                            first_indicator_row.push_str(&"^".repeat(other_file_message.length));
                        }
                        Some(_) => {
                            first_indicator_row.push_str("^");
                            first_indicator_row
                                .push_str(&" ".repeat(other_file_message.length - 1));
                        }
                    }
                }

                // Print the first indicator row
                eprintln!(
                    "{}   {}",
                    " ".repeat(largest_line_num.to_string().len()),
                    first_indicator_row.bright_black()
                );

                // Now do the rest of the lines
                for file_message in messages_on_line.iter().rev() {
                    // We only display rows that have messages
                    match &file_message.message {
                        None => continue,
                        Some(message) => {
                            // Initialize a string for our current row
                            let mut indicator_row = String::from("");

                            // Loop forwards through the snippet until we reach the current message
                            for other_file_message in messages_on_line {
                                // First, add the correct amount of whitespace between the previous indicator
                                // (or the beginning of the line) and this one
                                //
                                // That is equal to the offset of this token from the beginning of the line
                                // (which is column - 1)
                                // minus the current length of the indicator row
                                indicator_row.push_str(
                                    &" ".repeat(
                                        (other_file_message.column - 1) - indicator_row.len(),
                                    ),
                                );

                                // Add a bar for the arrow if this one has a message
                                match other_file_message.message {
                                    Some(_) => indicator_row.push('|'),
                                    None => {}
                                }
                                // If we've printed all of the indicators, move on to printing the
                                // message
                                if other_file_message == file_message {
                                    break;
                                } else {
                                    // We're not done yet, so add spaces for the rest of the
                                    // characters in the token
                                    indicator_row
                                        .push_str(&" ".repeat(other_file_message.length - 1));
                                }
                            }

                            // Finally, add the message to the end of the row!
                            indicator_row.push_str(&format!(" {}", message));

                            // Now we have a fully formed indicator line, so print it
                            // (add enough spaces at the beginning to make it line up with the REPL line,
                            // which started after the "[largest_line_num] | ")
                            eprintln!(
                                "{}   {}",
                                " ".repeat(largest_line_num.to_string().len()),
                                indicator_row.bright_black()
                            );
                        }
                    }
                }

                // Update the last line
                last_line = Some(line_num);
            }
        }
    }

    pub fn display_line(&self, line_num: usize, line_str: &str, largest_line_num: usize) {
        let largest_line_num_len = largest_line_num.to_string().len();
        let difference = largest_line_num_len - line_num.to_string().len();

        eprintln!(
            "{}{} {} {}",
            " ".repeat(difference), // Extra whitespace to ensure all of the lines are aligned
            line_num.to_string().bright_black(), // Line number
            "|".bright_black(),
            line_str // Line
        );
    }
}

#[derive(Clone, PartialEq, Eq, Ord)]
pub enum ErrorMessage {
    FileMessage(FileMessage),
    REPLMessage(REPLMessage),
}

impl ErrorMessage {
    pub fn new(message: Option<String>, location_info: &LocationInfo) -> ErrorMessage {
        match location_info.length {
            Some(length) => match location_info.offset {
                Some(offset) => match location_info.filename.clone() {
                    Some(filename) => {
                        // We want to instantiate a FileMessage
                        let mut index = 0;
                        let mut line_num = 1;
                        let mut column = 1;
                        let mut previous_line = None;

                        let lines = read_lines(filename.clone());

                        for line in lines.clone() {
                            for _ in get_graphemes_from_line(line.clone()) {
                                // If we have reached the start of our problem token
                                if index == offset {
                                    // If the current line number is greater than one, we can collect the
                                    // previous line to give more context to where the problem is
                                    if line_num > 1 {
                                        previous_line = Some(lines[line_num - 2].clone());
                                    }

                                    return ErrorMessage::FileMessage(FileMessage {
                                        filename,
                                        line_num,
                                        column,
                                        length,
                                        previous_line,
                                        problem_line: line,
                                        message,
                                    });
                                }
                                index += 1;
                                column += 1;
                            }
                            line_num += 1;
                            column = 1;
                        }

                        panic!("File does not contain offset {}", offset);
                    }
                    None => {
                        // We want to instantiate a REPLMessage
                        match location_info.repl_str.clone() {
                            Some(repl_str) => {
                                return ErrorMessage::REPLMessage(REPLMessage::new(
                                    repl_str,
                                    offset + 1,
                                    length,
                                    message,
                                ))
                            }
                            None => panic!("No repl_str"),
                        }
                    }
                },
                None => panic!("No length"),
            },
            None => panic!("No offset"),
        }
    }
}

impl PartialOrd for ErrorMessage {
    fn partial_cmp(&self, second_message: &ErrorMessage) -> Option<std::cmp::Ordering> {
        match self {
            ErrorMessage::REPLMessage(repl_self) => match second_message {
                ErrorMessage::REPLMessage(repl_second) => {
                    return repl_self.partial_cmp(repl_second);
                }
                ErrorMessage::FileMessage(_) => {
                    return Some(Ordering::Less);
                }
            },
            ErrorMessage::FileMessage(file_self) => match second_message {
                ErrorMessage::FileMessage(file_second) => {
                    return file_self.partial_cmp(file_second);
                }
                ErrorMessage::REPLMessage(_) => {
                    return Some(Ordering::Greater);
                }
            },
        }
    }
}

#[derive(Clone, PartialEq, Eq, Ord)]
pub struct FileMessage {
    pub filename: String,
    pub line_num: usize,
    pub column: usize,
    pub length: usize,

    pub previous_line: Option<String>,
    pub problem_line: String,

    pub message: Option<String>,
}

impl FileMessage {
    pub fn new(
        filename: String,
        line_num: usize,
        column: usize,
        length: usize,
        previous_line: Option<String>,
        problem_line: String,
        message: Option<String>,
    ) -> FileMessage {
        return FileMessage {
            filename,
            line_num,
            column,
            length,
            previous_line,
            problem_line,
            message,
        };
    }
}

impl PartialOrd for FileMessage {
    fn partial_cmp(&self, second_message: &FileMessage) -> Option<std::cmp::Ordering> {
        if self.line_num < second_message.line_num {
            return Some(Ordering::Less);
        } else if self.line_num > second_message.line_num {
            return Some(Ordering::Greater);
        } else {
            // If the messages are on the same line, compare their column number
            if self.column < second_message.column {
                return Some(Ordering::Less);
            } else if self.column > second_message.column {
                return Some(Ordering::Greater);
            } else {
                return Some(Ordering::Equal);
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Ord)]
pub struct REPLMessage {
    pub repl_str: String,

    pub column: usize,
    pub length: usize,

    pub message: Option<String>,
}

impl REPLMessage {
    pub fn new(
        repl_str: String,
        column: usize,
        length: usize,
        message: Option<String>,
    ) -> REPLMessage {
        return REPLMessage {
            repl_str,
            column,
            length,
            message,
        };
    }
}

impl PartialOrd for REPLMessage {
    fn partial_cmp(&self, second_message: &REPLMessage) -> Option<std::cmp::Ordering> {
        if self.column < second_message.column {
            return Some(Ordering::Less);
        } else if self.column > second_message.column {
            return Some(Ordering::Greater);
        } else {
            return Some(Ordering::Equal);
        }
    }
}

// There will be one REPLSnippet for every repl_str that has an error or warning
// Each REPLMessage in this vector has the same repl_str
// and all will be displayed together as a single snippet
pub type REPLSnippet = Vec<REPLMessage>;

// There will be one FileSnippet for every source file that has an error or warning
// We use the line number of the message as the key
// (BTreeMaps are nice because they automatically sort pairs by the key),
// and the value is a vector because there can be multiple messages on the same line
pub type FileSnippet = BTreeMap<usize, Vec<FileMessage>>;

pub struct SnippetData {
    // Each vector
    repl_snippets: HashMap<String, REPLSnippet>,
    file_snippets: HashMap<String, FileSnippet>,
}

impl SnippetData {
    pub fn new(messages: Vec<ErrorMessage>) -> SnippetData {
        let mut repl_snippets = HashMap::new();
        let mut file_snippets: HashMap<String, FileSnippet> = HashMap::new();

        for message in messages {
            match message {
                ErrorMessage::FileMessage(file_message) => {
                    match file_snippets.get_mut(&file_message.filename) {
                        None => {
                            // This is the first message appearing in this file
                            // Create a new snippet for this file
                            let mut new_file_map = BTreeMap::new();
                            // Make a new vector of messages that appear in this file on this line
                            let messages_in_file_on_line = vec![file_message.clone()];
                            // Add the vector to the snippet, indexed by the line_num of the message
                            new_file_map.insert(file_message.line_num, messages_in_file_on_line);
                            // Add the snippet to the map of snippets
                            file_snippets.insert(file_message.filename.clone(), new_file_map);
                        }
                        Some(file_snippet) => {
                            // We already have at least one message that appears in this file
                            // Check if there's already at least one message that appears on the same line
                            let mut file_snippet_clone = file_snippet.clone();
                            match file_snippet_clone.get_mut(&file_message.line_num) {
                                None => {
                                    // This is the first message appearing on this line
                                    // Create a new list of messages which appear on this line
                                    let messages_on_line = vec![file_message.clone()];
                                    // Insert the list into the snippet, indexed by its line_num
                                    file_snippet.insert(file_message.line_num, messages_on_line);
                                }
                                Some(messages_on_line_clone) => {
                                    // We already have at least one message
                                    // that appears on this line
                                    // Add the new message to the list
                                    messages_on_line_clone.push(file_message.clone());
                                    // Sort the list by column number
                                    messages_on_line_clone.sort();
                                    // Reinsert list back into the snippet
                                    file_snippet.insert(
                                        file_message.line_num,
                                        messages_on_line_clone.to_vec(),
                                    );
                                }
                            }
                        }
                    }
                }
                ErrorMessage::REPLMessage(repl_message) => {
                    let mut repl_snippets_clone = repl_snippets.clone();
                    match repl_snippets_clone.get_mut(&repl_message.repl_str) {
                        None => {
                            // This is the first message appearing in this REPL line
                            // Create a new vector of messages (AKA snippet) which appear on this line
                            let messages_on_line = vec![repl_message.clone()];
                            // Add the snippet to the map of snippets, indexed by its REPL line
                            repl_snippets.insert(repl_message.repl_str, messages_on_line);
                        }
                        Some(repl_snippet_clone) => {
                            // We already have at least one message that appears on this REPL line
                            // Add the new message to the list
                            repl_snippet_clone.push(repl_message.clone());
                            // Sort the snippet by column number
                            repl_snippet_clone.sort();
                            // Reinsert snippet back into the map of snippets
                            repl_snippets
                                .insert(repl_message.repl_str, repl_snippet_clone.to_vec());
                        }
                    };
                }
            };
        }

        return SnippetData {
            repl_snippets,
            file_snippets,
        };
    }
}
