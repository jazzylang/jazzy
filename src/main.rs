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

use std::collections::HashMap;
use std::env;
use std::process::exit;

use clap::Parser as ArgParser;
use colored::Colorize;
use interpreter::interpreter_data::Value;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use semantic_checker::semantic_checker_data::SymbolTable;

pub mod infrastructure;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod semantic_checker;

use crate::infrastructure::error::{ConsoleErrorReporter, ErrorReporter};
use crate::infrastructure::file::{get_graphemes, get_graphemes_from_line, LocationInfo};
use crate::infrastructure::log::Logger;
use crate::interpreter::interpreter::Interpreter;
use crate::parser::parser::Parser;
use crate::parser::parser_data::AST;
use crate::scanner::scanner::Scanner;
use crate::semantic_checker::semantic_checker::SemanticChecker;

#[derive(ArgParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// The source file to be executed
    filename: Option<String>,

    #[arg(short, long)]
    logfile: Option<String>,

    #[arg(short = 'd', long)]
    disable_warnings_as_errors: bool,

    #[arg(short = 'e', long)]
    show_all_errors: bool,

    #[arg(short = 'w', long)]
    show_all_warnings: bool,

    #[arg(short = 's', long)]
    show_all_errors_and_warnings: bool,

    #[arg(long)]
    emit_ast: bool,
}

fn main() {
    // Get the command line arguments
    let args = Args::parse();

    // Set some error-based options
    // Provided the current error handler we have is a console error handler, check if the program has an error
    let console_error = ConsoleErrorReporter::new(
        !args.disable_warnings_as_errors,
        args.show_all_errors,
        args.show_all_warnings,
        args.show_all_errors_and_warnings,
        10,
    );

    // Determine whether we're in REPL mode or file mode
    let repl_mode;
    if args.filename == None {
        repl_mode = false;
    } else {
        repl_mode = true;
    }

    // Instantiate the compiler
    let mut jazzy = Jazzy::new(
        repl_mode,
        args.logfile,
        ErrorReporter::ConsoleErrorReporter(console_error),
        args.emit_ast,
    );

    // Check if we passed a source filename as a command line argument
    match args.filename {
        // If we did, compile it
        Some(filename) => jazzy.run_file(&filename),
        // If we didn't, enter the REPL
        None => jazzy.repl(),
    }
}

// This is the compiler!
pub struct Jazzy {
    _repl_mode: bool,
    location_info: LocationInfo,
    logger: Logger,
    error: ErrorReporter,
    emit_ast: bool,
    variables: HashMap<String, Value>,
    ast: AST,
    symbol_table: SymbolTable,
}

impl Jazzy {
    pub fn new(
        _repl_mode: bool,
        log_file: Option<String>,
        error: ErrorReporter,
        emit_ast: bool,
    ) -> Jazzy {
        return Jazzy {
            _repl_mode,
            location_info: LocationInfo::new(),
            logger: Logger::new(log_file),
            error,
            emit_ast,
            variables: HashMap::new(),
            ast: AST::new(),
            symbol_table: SymbolTable::new(),
        };
    }

    // Run the REPL
    fn repl(&mut self) {
        let mut editor = DefaultEditor::new().unwrap();

        // Print the header
        self.print_header();

        let mut exit = false;
        while !exit {
            let readline = editor.readline(&format!("=^{}^= ", "..".blue()));
            match readline {
                Ok(line) => {
                    editor.add_history_entry(line.as_str()).unwrap();

                    // Quit the program
                    if line == "quit" || line == "q" || line == "exit" || line == "e" {
                        exit = true;
                    } else if line == "" {
                        continue;
                    } else {
                        // Run the line
                        self.run_line(line);
                    }
                }
                Err(ReadlineError::Interrupted) => {
                    break;
                }
                Err(ReadlineError::Eof) => {
                    break;
                }
                Err(err) => {
                    println!("error: {:?}", err);
                }
            }
        }
    }

    // Run a file (a thin wrapper around run())
    fn run_file(&mut self, file: &str) {
        // Keep track of the name of the file we're about to compile for later
        self.location_info.filename = Some(file.to_string());

        let file_log = format!("Received source file: \"{}\"", file);
        let file_log_len = file_log.len();
        self.logger.log(&format!("{}", "-".repeat(file_log_len)));
        self.logger.log("BEGIN COMPILATION");
        self.logger.log(&file_log);
        self.logger.log(&format!("{}", "-".repeat(file_log_len)));

        // Convert the file into a vector of characters and run it
        self.run(get_graphemes(file));
    }

    // Run a line (a thin wrapper around run())
    fn run_line(&mut self, line: String) {
        // Keep track of the line we're about to compile for later
        self.location_info.repl_str = Some(line.to_string());

        let repl_log = format!("Received REPL line: \"{}\"", &line);
        let repl_log_len = repl_log.len();
        self.logger.log(&format!("{}", "-".repeat(repl_log_len)));
        self.logger.log("BEGIN COMPILATION");
        self.logger.log(&repl_log);
        self.logger.log(&format!("{}", "-".repeat(repl_log_len)));

        // Convert the line into a vector of characters and run it
        self.run(get_graphemes_from_line(line));
    }

    // Take a vector of characters and run the compiler on it
    fn run(&mut self, chars: Vec<String>) {
        // Get the previous size of the AST,
        // so we can know if we've added to it
        let old_ast_size = self.ast.len();

        // Instantiate the scanner, passing in the vector and the error reporter
        let mut scanner = Scanner::new(
            chars,
            self.location_info.clone(),
            &mut self.logger,
            &mut self.error,
        );

        // Run the scanner and collect the list of tokens
        let tokens = scanner.scan();

        // If our list of tokens only consists of a single EOF token, return to the REPL
        if tokens.len() == 1 {
            return;
        }

        let mut parser = Parser::new(tokens, &mut self.ast, &mut self.logger, &mut self.error);
        parser.parse();

        // If we haven't added any nodes to our AST, return to the REPL
        if self.ast.len() == old_ast_size {
            return;
        }

        let mut semantic_checker = SemanticChecker::new(
            &mut self.ast,
            &mut self.symbol_table,
            &mut self.logger,
            &mut self.error,
        );
        semantic_checker.check();

        let root_node = self.ast.get_root_node();
        // If the user just wants to emit the AST,
        // pretty-print it and then return to the REPL
        if self.emit_ast {
            println!("{}", root_node.to_string(&self.ast));
            // Return to the REPL
            return;
        }

        // Provided the current error handler we have is a console error handler, check if the program has an error
        match &mut self.error {
            ErrorReporter::ConsoleErrorReporter(console_error) => {
                if console_error.error_num > console_error.max_errors_or_warnings {
                    eprintln!(
                        "Only {} messages displayed out of {} errors and {} warnings",
                        console_error.max_errors_or_warnings,
                        console_error.errors_reported.len(),
                        console_error.warnings_reported.len()
                    );
                    eprintln!("To see all errors, pass -e or --show-all-errors");
                    eprintln!("To see all warnings, pass -w or --show-all-warnings");
                    eprintln!("To see all errors, pass -s or --show-all-errors-and-warnings\n");
                }

                if console_error.warnings_reported.len() > 0 && console_error.warnings_as_errors {
                    eprintln!("Warnings are treated as errors by default");
                    eprintln!("If you don't want warnings to halt execution, pass -d or --disable-warnings-as-errors")
                }

                if console_error.has_error {
                    // If the program does have an error, don't start executing the code
                    match self.location_info.repl_str {
                        None => exit(1), // If we're in file mode, exit with an error code
                        Some(_) => {
                            // If we're in REPL mode, clear the error info and return to the REPL
                            console_error.clear_for_next_execution();
                            return;
                        }
                    }
                }
            }
        }

        // We didn't have any compiler errors, so run the interpreter
        let mut interpreter = Interpreter::new(
            &mut self.ast,
            &mut self.variables,
            &mut self.logger,
            &mut self.error,
        );
        interpreter.run();
    }

    // Print the header that is displayed when the REPL is first run
    fn print_header(&self) {
        // Get the current version of the compiler
        let version_number = env!("CARGO_PKG_VERSION");

        // Print the language name and version
        println!("The jazzy programming language v{}", version_number);
        println!("Created by Dylan Tuttle");
        println!();

        // Print ASCII art
        println!("                 /`--'\\");
        println!("              ==({} w {} )==", "'".blue(), "'".blue());
        println!("                /       \\");
        println!("    {}           || |  _  \\", "_".bright_black());
        println!("   {}          || |_/    |", "(_)".bright_black());
        println!("    {}((_/_____  /", "_  __ _ ___".bright_black());
        println!("   {}| |", "| |/ _` |_  /_  / | ".bright_black());
        println!("   {}| |", "| | (_| |/ / / /| |_".bright_black());
        println!("   {}| |", "| |\\__,_/___/___|\\__".bright_black());
        println!("  {}               __/ |", "_/ |".bright_black());
        println!(" {}               (___/ ", "|__/".bright_black());
        println!();
        /*
        println!("                 /`--'\\");
        println!("              ==({} w {} )==", "'".blue(), "'".blue());
        println!("                /       \\");
        println!("    _           || |  _  \\");
        println!("   (_)          || |_/    |");
        println!("    _  __ _ ___((_/_____  /");
        println!("   | |/ _` |_  /_  / | | |");
        println!("   | | (_| |/ / / /| |_| |");
        println!("   | |\\__,_/___/___|\\__| |");
        println!("  _/ |               __/ |");
        println!(" |__/               (___/ ");
        println!();
        */

        // Print the instruction line
        println!("Write a line of code below and press enter to evaluate it:");
    }
}
