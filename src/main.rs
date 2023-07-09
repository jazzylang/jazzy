use std::env;
use std::io;
use std::io::{BufRead, Write};

use colored::Colorize;

fn main() {
    // Get the command line arguments
    let args: Vec<String> = env::args().collect();

    // If we haven't passed any arguments in, we can simply start the REPL
    if args.len() < 2 {
        // Print the header
        print_header();

        let mut exit = false;
        while !exit {
            // Print prompt
            print!("=^{}^= ", "..".blue());
            io::stdout().flush().unwrap();

            // Get user input
            let line = io::stdin().lock().lines().next().unwrap().unwrap();

            // Quit the program
            if line == "quit" || line == "q" || line == "exit" || line == "e" {
                exit = true;
            } else {
                // For now, just repeat the line back
                println!("{}", line);
            }
        }
    }
}

// This function prints the header that is displayed when the REPL is first run
fn print_header() {
    // Get the current version of the compiler
    let version_number = env!("CARGO_PKG_VERSION");

    // Print the language name and version
    println!("The jazzy programming language v{}", version_number);
    println!();

    // Print ASCII art
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

    // Print the instruction line
    println!("Write a line of code below and press enter to evaluate it:");
}
