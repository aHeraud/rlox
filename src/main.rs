mod token;
mod scanner;
mod error;
mod parser;
mod ast;
mod interpreter;

use std::path::Path;
use std::process::exit;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};

use scanner::Scanner;
use crate::interpreter::Interpreter;
use crate::parser::Parser;

struct Lox {
    had_error: bool,
    had_runtime_error: bool,
    interpreter: Interpreter,
}

impl Lox {
    fn new() -> Lox {
        Lox {
            had_error: false,
            had_runtime_error: false,
            interpreter: Interpreter::new()
        }
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) {
        let source = read_to_string(path).expect("Could not read file");
        self.run(source);

        if self.had_error {
            exit(65);
        }
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let _= stdout().flush();
            let mut line = String::new();
            stdin().read_line(&mut line).expect("Could not read line");
            self.run(line);
        }
    }

    pub fn run(&mut self, source: String) {
        let scanner = Scanner::new(source);
        let (tokens, errors) = scanner.scan_tokens();

        if errors.len() > 0 {
            for error in errors {
                self.error(error.line, &error.message);
            }
            return;
        }

        match Parser::new(tokens).parse() {
            Ok(statements) => {
                if let Err(e) = self.interpreter.interpret(&statements) {
                    self.runtime_error(e);
                }
            },
            Err(e) => {
                e.errors.iter().for_each(|e|self.error(e.line, &e.message));;;
            }
        }
    }

    fn error(&mut self, line: usize, message: &str) {
        self.report(line, "", message);
    }

    fn report(&mut self, line: usize, location: &str, message: &str) {
        eprintln!("[line {}] Error{}: {}", line, location, message);
        self.had_error = true;
    }

    fn runtime_error(&mut self, error: error::RuntimeError) {
        eprintln!("{}", error);
        self.had_runtime_error = true;
    }
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() > 1 {
        println!("Usage: lox [script]");
        exit(64)
    } else {
        let mut lox = Lox::new();
        if args.len() == 1 {
            lox.run_prompt();
        } else {
            lox.run_file(&args[1]);
        }

        if lox.had_runtime_error {
            exit(70);
        } else if lox.had_error {
            exit(65);
        }
    }
}
