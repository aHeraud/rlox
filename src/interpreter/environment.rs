use std::collections::HashMap;
use std::rc::Rc;
use std::cell::UnsafeCell;
use std::fmt::{Debug, Formatter};
use crate::error::RuntimeError;
use crate::token::Token;

use super::Value;

pub struct Environment(Rc<UnsafeCell<InternalEnvironment>>);

impl Environment {
    pub fn new() -> Self {
        Self(Rc::new(UnsafeCell::new(InternalEnvironment::new())))
    }

    pub fn with_enclosing(self, parent: Environment) -> Self {
        self.get_mut().enclosing = Some(parent);
        self
    }

    /// Define a new variable in the environment, or re-define an existing one.
    pub fn define(&mut self, name: &str, value: Value) {
        self.get_mut().define(name, value);
    }

    /// Assign a value to an existing variable.
    /// If the variable is not defined in this environment, the enclosing environments
    /// will be recursively searched.
    pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
        self.get_mut().assign(name, value)
    }

    /// Get a mutable reference to a variable defined in this environment.
    /// If the variable is not defined in this environment, the enclosing environments
    /// will be recursively searched.
    pub fn get_var(&mut self, name: &Token) -> Result<&mut Value, RuntimeError> {
        self.get_mut().get_var(name)
    }

    fn get(&self) -> &InternalEnvironment {
        unsafe { &*self.0.get() }
    }

    fn get_mut(&self) -> &mut InternalEnvironment {
        unsafe { &mut *self.0.get() }
    }
}

/// Creates a shallow copy of the environment.
impl Clone for Environment {
    fn clone(&self) -> Self {
        Self(Rc::clone(&self.0))
    }
}

impl Debug for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.get())
    }
}

#[derive(Debug, Default)]
struct InternalEnvironment {
    values: HashMap<String, Value>,
    enclosing: Option<Environment>,
}

impl InternalEnvironment {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define(&mut self, name: &str, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn assign(&mut self, name: &Token, value: Value) -> Result<(), RuntimeError> {
        if self.values.contains_key(&name.lexeme) {
            self.values.insert(name.lexeme.clone(), value);
            Ok(())
        } else if let Some(parent) = &mut self.enclosing {
            parent.get_mut().assign(name, value)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme)
            ))
        }
    }

    pub fn get_var(&mut self, name: &Token) -> Result<&mut Value, RuntimeError> {
        if let Some(value) = self.values.get_mut(&name.lexeme) {
            Ok(value)
        } else if let Some(parent) = &mut self.enclosing {
            parent.get_mut().get_var(name)
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined variable '{}'", name.lexeme)
            ))
        }
    }
}
