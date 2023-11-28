use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::ast::statements::*;
use crate::error::RuntimeError;
use crate::interpreter::environment::Environment;
use crate::token::Token;

mod expressions;
mod statements;
mod environment;

#[derive(Clone,Debug,PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<Box<dyn Function>>),
    Nil
}

impl Value {
    fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true
        }
    }

    fn is_equal(&self, other: &Value) -> bool {
        match (self, other) {
            (Value::Nil, Value::Nil) => true,
            (Value::Number(n1), Value::Number(n2)) => n1 == n2,
            (Value::String(s1), Value::String(s2)) => s1 == s2,
            (Value::Boolean(b1), Value::Boolean(b2)) => b1 == b2,
            _ => false
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::Boolean(b) => write!(f, "{}", b),
            Value::Number(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Function(fun) => write!(f, "{}", fun),
            Value::Nil => write!(f, "nil"),
        }
    }
}

pub trait Function: Debug {
    fn arity(&self) -> usize;
    fn name(&self) -> &str;
    fn call(&self, arguments: &[Value]) -> Result<Value, RuntimeError>;
}

impl PartialEq for dyn Function {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Display for dyn Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.name())
    }
}

struct Closure {
    function: FunctionStatement,
    environment: Environment,
}

impl Debug for Closure {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<fn {}>", self.function.name)
    }
}

impl Function for Closure {
    fn arity(&self) -> usize {
        self.function.params.len()
    }

    fn name(&self) -> &str {
        &self.function.name.lexeme
    }

    fn call(&self, arguments: &[Value]) -> Result<Value,RuntimeError> {
        let mut env = Environment::new().with_enclosing(self.environment.clone());

        // Define the function parameters in the new environment
        for (param, arg) in self.function.params.iter().zip(arguments.iter()) {
            env.define(&param.lexeme, arg.clone());
        }

        match (|| {
            for statement in &self.function.body {
                statement.evaluate(&mut env)?;
            }
            Ok(())
        })() {
            Ok(()) => Ok(Value::Nil),
            Err(InterpreterError::Return(_t, v)) => Ok(v),
            Err(InterpreterError::Runtime(e)) => Err(e),
        }
    }
}

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
