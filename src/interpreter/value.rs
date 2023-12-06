use std::cell::UnsafeCell;
use std::collections::HashMap;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::rc::Rc;
use crate::error::RuntimeError;
use crate::interpreter::function::{Closure, Function, Method};
use crate::token::Token;

#[derive(Clone,Debug,PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
    Function(Rc<Box<dyn Function>>),
    Class(ClassDefinition),
    Instance(InstanceReference),
    Nil
}

impl Value {
    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            Value::Nil => false,
            Value::Boolean(b) => *b,
            _ => true
        }
    }

    pub(crate) fn is_equal(&self, other: &Value) -> bool {
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
            Value::Class(class) => write!(f, "{}", class),
            Value::Instance(instance) => write!(f, "{}", instance),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug,PartialEq)]
pub struct ClassDefinition(pub(crate) Rc<Class>);

impl ClassDefinition {
    pub fn new(class: Rc<Class>) -> ClassDefinition {
        ClassDefinition(class)
    }

    pub fn find_method(&self, name: &str) -> Option<(&Rc<Box<Closure>>, &ClassDefinition)> {
        if let Some(method) = self.0.methods.get(name) {
            return Some((method, self));
        } else if let Some(super_class) = &self.0.super_class {
            return super_class.find_method(name);
        } else {
            None
        }
    }
}

impl Clone for ClassDefinition {
    fn clone(&self) -> Self {
        ClassDefinition(Rc::clone(&self.0))
    }
}

impl Display for ClassDefinition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.name)
    }
}

#[derive(Clone,Debug,PartialEq)]
pub struct Class {
    pub name: String,
    pub super_class: Option<ClassDefinition>,
    pub methods: HashMap<String, Rc<Box<Closure>>>,
}

impl Class {
    pub fn new(name: String, super_class: Option<ClassDefinition>, methods: HashMap<String, Rc<Box<Closure>>>) -> Class {
        Class {
            name,
            super_class,
            methods
        }
    }
}

impl Display for Class {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "<class {}>", self.name)
    }
}

#[derive(Debug)]
pub struct InstanceReference(pub(crate) Rc<UnsafeCell<Instance>>);

impl InstanceReference {
    pub fn new(instance: Instance) -> InstanceReference {
        InstanceReference(Rc::new(UnsafeCell::new(instance)))
    }

    pub fn get(&self, name: &Token) -> Result<Value, RuntimeError> {
        unsafe {
            (*self.0.get()).get(name, self)
        }
    }

    pub fn set(&self, name: &Token, value: Value) {
        unsafe {
            (*self.0.get()).set(name, value)
        }
    }
}

impl Clone for InstanceReference {
    fn clone(&self) -> Self {
        InstanceReference(Rc::clone(&self.0))
    }
}

impl Display for InstanceReference {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        unsafe {
            write!(f, "{}", (*self.0.get()))
        }
    }
}

impl PartialEq for InstanceReference {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            (*self.0.get()).eq(&(*other.0.get()))
        }
    }
}

#[derive(Clone,Debug)]
pub struct Instance {
    class: ClassDefinition,
    fields: HashMap<String, Value>,
}

impl Instance {
    pub fn new(class: ClassDefinition) -> Instance {
        Instance {
            class,
            fields: HashMap::new(),
        }
    }

    pub fn get(&self, name: &Token, reference: &InstanceReference) -> Result<Value, RuntimeError> {
        if let Some(value) = self.fields.get(&name.lexeme) {
            Ok(value.clone())
        } else if let Some((closure, class)) = self.class.find_method(&name.lexeme) {
            let method = Method::new(reference.clone(), class.clone(), closure.clone());
            Ok(Value::Function(Rc::new(Box::new(method))))
        } else {
            Err(RuntimeError::new(
                name.clone(),
                format!("Undefined property '{}'", name.lexeme)
            ))
        }
    }

    pub fn set(&mut self, name: &Token, value: Value) {
        self.fields.insert(name.lexeme.clone(), value);
    }
}

impl PartialEq for Instance {
    // TODO: equality for instances?
    fn eq(&self, _other: &Self) -> bool {
        false
    }
}

impl Display for Instance {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} instance", self.class.0.name)
    }
}
