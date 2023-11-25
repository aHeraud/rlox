use crate::token::Token;
use std::error::Error;
use std::fmt;
use std::fmt::{Display,Formatter};


#[derive(Debug)]
pub struct LoxError {
    pub line: usize,
    pub location: String,
    pub message: String
}

impl LoxError {
    pub fn new(line: usize, location: String, message: String) -> LoxError {
        LoxError {
            line,
            location,
            message
        }
    }
}

impl Display for LoxError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.line, self.message)
    }
}

impl Error for LoxError {}

#[derive(Debug)]
pub struct RuntimeError {
    pub token: Token,
    pub message: String
}

impl RuntimeError {
    pub fn new(token: Token, message: String) -> RuntimeError {
        RuntimeError {
            token,
            message
        }
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "[line {}] Error: {}", self.token.line, self.message)
    }
}

impl Error for RuntimeError {}