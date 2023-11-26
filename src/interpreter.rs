use std::collections::HashMap;
use std::fmt;
use std::fmt::Formatter;
use std::ops::Neg;
use crate::ast::{expressions::*, statements::*};
use crate::error::RuntimeError;
use crate::token::{Token, TokenType::*};

#[derive(Clone,Debug,PartialEq)]
pub enum Value {
    Number(f64),
    String(String),
    Boolean(bool),
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
            Value::Nil => write!(f, "nil"),
        }
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

    pub fn execute(&mut self, statement: &Statement) -> Result<(),RuntimeError> {
        statement.evaluate(&mut self.environment)
    }
}

fn get_double(operator: &Token, value: &Value) -> Result<f64,RuntimeError> {
    match value {
        Value::Number(n) => Ok(*n),
        _ => Err(RuntimeError::new(
            operator.clone(),
            "Operand must be a number".to_string()
        ))
    }
}

trait Evaluate<T> {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<T,RuntimeError>;
}

impl Evaluate<Value> for LiteralExpression {
    fn evaluate(&self, _: &mut Box<Environment>) -> Result<Value,RuntimeError> {
        Ok(match &self.value {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil
        })
    }
}

impl Evaluate<Value> for AssignmentExpression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        let value = self.value.evaluate(env)?;
        env.assign(&self.name, value.clone())?;
        Ok(value)
    }
}

impl Evaluate<Value> for GroupingExpression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        self.expression.evaluate(env)
    }
}

impl Evaluate<Value> for UnaryExpression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        let right = self.right.evaluate(env)?;
        match self.operator.token_type {
            MINUS => {
                let double = get_double(&self.operator, &right)?;
                Ok(Value::Number(double.neg()))
            },
            BANG => Ok(Value::Boolean(!right.is_truthy())),
            _ => panic!("Unexpected operator") // unreachable
        }
    }
}

impl Evaluate<Value> for BinaryExpression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        let left = self.left.evaluate(env)?;
        let right = self.right.evaluate(env)?;

        match self.operator.token_type {
            PLUS => {
                match (left, right) {
                    (Value::Number(n1), Value::Number(n2)) => Ok(Value::Number(n1 + n2)),
                    (Value::String(s1), Value::String(s2)) => Ok(Value::String(format!("{}{}", s1, s2))),
                    _ => Err(RuntimeError::new(
                        self.operator.clone(),
                        "Operands must be two numbers or two strings".to_string()
                    ))
                }
            },
            EQUAL_EQUAL => Ok(Value::Boolean(left.is_equal(&right))),
            BANG_EQUAL => Ok(Value::Boolean(!left.is_equal(&right))),
            _ => {
                let l = get_double(&self.operator, &left)?;
                let r = get_double(&self.operator, &right)?;
                match self.operator.token_type {
                    MINUS => Ok(Value::Number(l - r)),
                    SLASH => Ok(Value::Number(l / r)),
                    STAR => Ok(Value::Number(l * r)),
                    GREATER => Ok(Value::Boolean(l > r)),
                    GREATER_EQUAL => Ok(Value::Boolean(l >= r)),
                    LESS => Ok(Value::Boolean(l < r)),
                    LESS_EQUAL => Ok(Value::Boolean(l <= r)),
                    _ => panic!("Unexpected operator") // unreachable
                }
            }
        }
    }
}

impl Evaluate<Value> for VariableExpression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        env.get_var(&self.name)
            .map(|v| v.clone())
    }
}

impl Evaluate<Value> for Expression {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<Value,RuntimeError> {
        match self {
            Expression::Assign(a) => a.evaluate(env),
            Expression::Literal(l) => l.evaluate(env),
            Expression::Unary(u) => u.evaluate(env),
            Expression::Binary(b) => b.evaluate(env),
            Expression::Grouping(g) => g.evaluate(env),
            Expression::Variable(v) => v.evaluate(env),
            _ => panic!("Unexpected expression") // unreachable
        }
    }
}

impl Evaluate<()> for ExpressionStatement {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<(),RuntimeError> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Evaluate<()> for PrintStatement {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<(),RuntimeError> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}

impl Evaluate<()> for VarStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(), RuntimeError> {
        let value = match &self.initializer {
            Some(e) => e.evaluate(environment)?,
            None => Value::Nil
        };
        environment.define(&self.name.lexeme, value);
        Ok(())
    }
}

impl Evaluate<()> for BlockStatement {
    fn evaluate<'a>(&self, environment: &mut Box<Environment>) -> Result<(), RuntimeError> {
        // swap out the environment for a new one
        let parent = std::mem::replace(environment, Box::new(Environment::new()));
        environment.enclosing = Some(parent);

        let result = (|| {
            for statement in &self.statements {
                statement.evaluate(environment)?;
            }
            Ok(())
        })();

        // swap the environment back
        let parent = std::mem::replace(&mut environment.enclosing, None);
        std::mem::swap(environment, &mut parent.unwrap());

        result
    }
}

impl Evaluate<()> for Statement {
    fn evaluate(&self, env: &mut Box<Environment>
) -> Result<(),RuntimeError> {
        match self {
            Statement::Expression(e) => e.evaluate(env),
            Statement::Print(p) => p.evaluate(env),
            Statement::Var(v) => v.evaluate(env),
            Statement::Block(b) => b.evaluate(env),
        }
    }
}
