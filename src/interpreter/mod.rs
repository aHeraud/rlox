use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::ops::Neg;
use std::rc::Rc;
use crate::ast::statements::*;
use crate::error::RuntimeError;
use crate::token::{Token, TokenType::*};

mod expressions;
mod statements;

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

impl fmt::Display for Value {
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
    fn call(&self, environment: &mut Environment, arguments: &[Value]) -> Result<Value,RuntimeError>;
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

impl Function for FunctionStatement {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn name(&self) -> &str {
        &self.name.lexeme
    }

    fn call(&self, environment: &mut Environment, arguments: &[Value]) -> Result<Value,RuntimeError> {
        // Create a new environment for the function, which is a child of the root (e
        let mut global = Box::new(Environment::default());
        std::mem::swap(global.as_mut(), environment.root());
        let mut env = Box::new(Environment::new());
        env.enclosing = Some(global);

        // Define the function parameters in the new environment
        for (param, arg) in self.params.iter().zip(arguments.iter()) {
            env.define(&param.lexeme, arg.clone());
        }

        let result = match (|| {
            for statement in &self.body {
                statement.evaluate(&mut env)?;
            }
            Ok(())
        })() {
            Ok(()) => Ok(Value::Nil),
            Err(InterpreterError::Return(_t, v)) => Ok(v),
            Err(InterpreterError::Runtime(e)) => Err(e),
        };

        // Undo the changes to the environment.
        let mut global = std::mem::replace(&mut env.enclosing, None);
        std::mem::swap(environment.root(), &mut global.unwrap());

        // If the function returns a value, return it. Otherwise return nil.
        result
    }
}

#[derive(Default,Debug)]
pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String,Value>,
}

impl Environment {
    pub fn new() -> Environment {
        Environment::default()
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> Result<(),RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        } else if let Some(parent) = &mut self.enclosing {
            parent.assign(name, value)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme)
            ))
        }
    }

    pub fn get_var(&mut self, name: &Token) -> Result<&mut Value,RuntimeError> {
        if let Some(value) = self.values.get_mut(&name.lexeme) {
            Ok(value)
        } else if let Some(parent) = &mut self.enclosing {
            parent.get_var(name)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme)
            ))
        }
    }

    pub fn root(&mut self) -> &mut Environment {
        if self.enclosing.is_none() {
            return self;
        } else {
            self.enclosing.as_mut().unwrap().root()
        }
    }
}

pub struct Interpreter {
    environment: Box<Environment>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: Box::new(Environment::new())
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
            InterpreterError::Return(t,v) => RuntimeError::new(t, format!("Unexpected return"))
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
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<T,InterpreterError>;
}
