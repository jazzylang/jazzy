use std::env;
use std::io;
use std::io::{BufRead, Write};

fn main() {
    // Get the command line arguments
    let args: Vec<String> = env::args().collect();

    // If we haven't passed any arguments in, we can simply start the REPL
    if args.len() < 2 {
        println!("jazzy v0.1.0");
        println!("Enter a line of code and press enter to evaluate it:");

        let mut exit = false;
        while !exit {
            // Print prompt
            print!("> ");
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
