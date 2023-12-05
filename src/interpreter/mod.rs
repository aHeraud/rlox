use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use crate::ast::statements::*;
use crate::error::RuntimeError;
use crate::interpreter::environment::Environment;
use crate::token::Token;

mod expressions;
mod statements;
mod environment;
mod value;
mod function;

use value::Value;

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Environment::new()
        }
    }

    pub fn interpret(&mut self, statements: &[Statement]) -> Result<(),RuntimeError> {
        for statement in statements {
            statement.evaluate(&mut self.environment)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
enum InterpreterError {
    Runtime(RuntimeError),
    Return(Token,Value)
}

impl From<RuntimeError> for InterpreterError {
    fn from(e: RuntimeError) -> Self {
        InterpreterError::Runtime(e)
    }
}

impl From<InterpreterError> for RuntimeError {
    fn from(e: InterpreterError) -> Self {
        match e {
            InterpreterError::Runtime(e) => e,
            InterpreterError::Return(t, _) => RuntimeError::new(t, format!("Unexpected return"))
        }
    }
}

impl Display for InterpreterError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            InterpreterError::Runtime(e) => write!(f, "{}", e),
            InterpreterError::Return(_t,v) => write!(f, "return {}", v),
        }
    }
}

impl Error for InterpreterError {}

trait Evaluate<T> {
    fn evaluate(&self, environment: &mut Environment) -> Result<T,InterpreterError>;
}
