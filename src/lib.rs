use std::error::Error;
use std::fs::read_to_string;
use std::io::{stdin, stdout, Write};
use std::path::Path;
use crate::interpreter::Interpreter;
use crate::parser::Parser;
use crate::resolver::Resolver;
use crate::scanner::Scanner;

mod token;
mod scanner;
mod error;
mod parser;
mod ast;
mod interpreter;
mod resolver;

pub struct Lox {
    interpreter: Interpreter,
    resolver: Resolver,
}

impl Lox {
    pub fn new() -> Lox {
        Lox {
            interpreter: Interpreter::new(),
            resolver: Resolver::new(),
        }
    }

    pub fn run_file<P: AsRef<Path>>(&mut self, path: P) -> Result<(), Box<dyn Error>> {
        let source = read_to_string(path).expect("Could not read file");
        self.run(source)
    }

    pub fn run_prompt(&mut self) {
        loop {
            print!("> ");
            let _= stdout().flush();
            let mut line = String::new();
            stdin().read_line(&mut line).expect("Could not read line");
            if let Err(err) = self.run(line) {
                eprintln!("{}", err);
            };
        }
    }

    pub fn run(&mut self, source: String) -> Result<(), Box<dyn Error>> {
        let scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens()?;

        let ast = Parser::new(tokens).parse()?;
        let program = self.resolver.resolve(ast)?;
        self.interpreter.interpret(&program)?;
        Ok(())
    }
}

#[cfg(target_arch = "wasm32")]
use wasm_bindgen::prelude::*;

#[cfg(target_arch = "wasm32")]
#[wasm_bindgen]
pub fn run(source: String) {
    use web_sys::console;

    let mut lox = Lox::new();
    if let Err(e) = lox.run(source) {
        console::error_1(&e.to_string().into());
    }
}
