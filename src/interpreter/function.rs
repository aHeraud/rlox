use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::ast::statements::FunctionStatement;
use crate::error::RuntimeError;
use crate::interpreter::environment::Environment;
use crate::interpreter::{Evaluate, InterpreterError};
use crate::interpreter::value::{ClassDefinition, Instance, InstanceReference, Value};

fn run(
    function: &FunctionStatement,
    environment: &mut Environment,
    arguments: &[Value]
) -> Result<Value, RuntimeError> {

    // Define the function parameters in the new environment
    for (param, arg) in function.params.iter().zip(arguments.iter()) {
        environment.define(&param.lexeme, arg.clone());
    }

    match (|| {
        for statement in &function.body {
            statement.evaluate(environment)?;
        }
        Ok(())
    })() {
        Ok(()) => Ok(Value::Nil),
        Err(InterpreterError::Return(_t, v)) => Ok(v),
        Err(InterpreterError::Runtime(e)) => Err(e),
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

pub struct Closure {
    pub function: FunctionStatement,
    pub environment: Environment,
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
        run(&self.function, &mut env, arguments)
    }
}

impl PartialEq for Closure {
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Function for ClassDefinition {
    fn arity(&self) -> usize {
        println!("Methods: {:?}", self.0.methods);
        self.find_method("init").map_or(0, |(m,_c)| m.arity())
    }

    fn name(&self) -> &str {
        &self.0.name
    }

    fn call(&self, arguments: &[Value]) -> Result<Value, RuntimeError> {
        let instance = Instance::new(self.clone());
        if let Some((initializer, _)) = self.find_method("init") {
            let initializer = Method::new(InstanceReference::new(instance), self.clone(), Rc::clone(initializer));
            initializer.call(arguments)
        } else {
            Ok(Value::Instance(InstanceReference::new(instance)))
        }
    }
}

#[derive(Debug)]
pub struct Method {
    this: InstanceReference,
    class: ClassDefinition,
    closure: Rc<Box<Closure>>
}

impl Method {
    pub fn new(this: InstanceReference, class: ClassDefinition, closure: Rc<Box<Closure>>) -> Method {
        Method {
            this,
            class,
            closure
        }
    }
}

impl Function for Method {
    fn arity(&self) -> usize {
        self.closure.arity()
    }

    fn name(&self) -> &str {
        self.closure.name()
    }

    fn call(&self, arguments: &[Value]) -> Result<Value, RuntimeError> {
        let mut env = Environment::new().with_enclosing(self.closure.environment.clone());
        env.define("this", Value::Instance(self.this.clone()));
        env.define("super", Value::Class(self.class.clone()));
        let return_value = run(&self.closure.function, &mut env, arguments)?;

        if self.name() == "init" {
            Ok(Value::Instance(self.this.clone()))
        } else {
            Ok(return_value)
        }
    }
}

impl Clone for Method {
    fn clone(&self) -> Self {
        Method {
            this: self.this.clone(),
            class: self.class.clone(),
            closure: Rc::clone(&self.closure)
        }
    }
}
