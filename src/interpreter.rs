use std::fmt;
use std::fmt::Formatter;
use std::ops::Neg;
use crate::ast::expressions::*;
use crate::error::RuntimeError;
use crate::token::{Token, TokenType::*, TokenType};

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

pub struct Interpreter {

}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {

        }
    }

    pub fn interpret(&self, expression: &Expression) -> Result<Value,RuntimeError> {
        expression.evaluate()
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

trait Evaluate {
    fn evaluate(&self) -> Result<Value,RuntimeError>;
}

impl Evaluate for LiteralExpression {
    fn evaluate(&self) -> Result<Value,RuntimeError> {
        Ok(match &self.value {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil
        })
    }
}

impl Evaluate for GroupingExpression {
    fn evaluate(&self) -> Result<Value,RuntimeError> {
        self.expression.evaluate()
    }
}

impl Evaluate for UnaryExpression {
    fn evaluate(&self) -> Result<Value,RuntimeError> {
        let right = self.right.evaluate()?;
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

impl Evaluate for BinaryExpression {
    fn evaluate(&self) -> Result<Value,RuntimeError> {
        let left = self.left.evaluate()?;
        let right = self.right.evaluate()?;

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

impl Evaluate for Expression {
    fn evaluate(&self) -> Result<Value,RuntimeError> {
        match self {
            Expression::Literal(l) => l.evaluate(),
            Expression::Unary(u) => u.evaluate(),
            Expression::Binary(b) => b.evaluate(),
            Expression::Grouping(g) => g.evaluate(),
            _ => panic!("Unexpected expression") // unreachable
        }
    }
}
