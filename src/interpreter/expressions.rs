use std::ops::Neg;
use std::rc::Rc;
use crate::ast::expressions::*;
use crate::error::RuntimeError;
use crate::interpreter::function::{Function, Method};
use crate::token::{Token, TokenType::*};
use super::{Value, Environment, Evaluate, InterpreterError};

fn get_double(operator: &Token, value: &Value) -> Result<f64, InterpreterError> {
    match value {
        Value::Number(n) => Ok(*n),
        _ => Err(RuntimeError::new(
            operator.clone(),
            "Operand must be a number".to_string()
        ).into())
    }
}

impl Evaluate<Value> for LiteralExpression {
    fn evaluate(&self, _: &mut Environment) -> Result<Value, InterpreterError> {
        Ok(match &self.value {
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Nil => Value::Nil
        })
    }
}

impl Evaluate<Value> for AssignmentExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
        let value = self.value.evaluate(env)?;
        env.assign(&self.name, value.clone())?;
        Ok(value)
    }
}

impl Evaluate<Value> for GroupingExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
        self.expression.evaluate(env)
    }
}

impl Evaluate<Value> for UnaryExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
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
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
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
                    ).into())
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
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
        env.get_var(&self.name)
            .map_err(|e| e.into())
            .map(|v| v.clone())
    }
}

impl Evaluate<Value> for LogicalExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        let left = self.left.evaluate(environment)?;
        match self.operator.token_type {
            AND => {
                if left.is_truthy() {
                    self.right.evaluate(environment)
                } else {
                    Ok(Value::Boolean(false))
                }
            },
            OR => {
                if left.is_truthy() {
                    Ok(Value::Boolean(true))
                } else {
                    self.right.evaluate(environment)
                }
            },
            _ => panic!("Unexpected operator") // unreachable
        }
    }
}

impl Evaluate<Value> for CallExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        let callee = self.callee.evaluate(environment)?;
        let mut arguments = Vec::new();
        for argument in &self.arguments {
            arguments.push(argument.evaluate(environment)?);
        }

        match callee {
            Value::Function(fun) => {
                if arguments.len() != fun.arity() {
                    return Err(RuntimeError::new(
                        self.paren.clone(),
                        format!("Expected {} arguments but got {}", fun.arity(), arguments.len())
                    ).into());
                }
                fun.call(&arguments).map_err(|e| e.into())
            },
            Value::Class(class) => class.call(&arguments).map_err(|e| e.into()),
            _ => Err(RuntimeError::new(
                self.paren.clone(),
                "Can only call functions and classes".to_string()
            ).into())
        }
    }
}

impl Evaluate<Value> for GetExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        let object = self.object.evaluate(environment)?;
        match object {
            Value::Instance(instance) => {
                instance.get(&self.name).map_err(|e| e.into())
            },
            _ => Err(RuntimeError::new(
                self.name.clone(),
                "Only instances have properties".to_string()
            ).into())
        }
    }
}

impl Evaluate<Value> for SetExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        let object = self.object.evaluate(environment)?;
        match object {
            Value::Instance(instance) => {
                let value = self.value.evaluate(environment)?;
                instance.set(&self.name, value.clone());
                Ok(value)
            },
            _ => Err(RuntimeError::new(
                self.name.clone(),
                "Only instances have fields".to_string()
            ).into())
        }
    }
}

impl Evaluate<Value> for SuperExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        let instance = match environment.lookup("this") {
            Some(Value::Instance(instance)) => instance.clone(),
            _ => panic!("Super outside of method definition")
        };
        let class = match environment.get_var(&self.keyword)? {
            Value::Class(class) => class.clone(),
            _ => panic!("Super must refer to a class definition")
        };

        let superclass = class.0.super_class.clone().unwrap();
        let (method, class) = superclass.find_method(&self.method.lexeme).unwrap();
        let method = Method::new(instance.clone(), class.clone(), method.clone());
        Ok(Value::Function(Rc::new(Box::new(method))))
    }
}

impl Evaluate<Value> for ThisExpression {
    fn evaluate(&self, environment: &mut Environment) -> Result<Value, InterpreterError> {
        environment.get_var(&self.keyword)
            .map(|v| v.clone())
            .map_err(|e| e.into())
    }
}

impl Evaluate<Value> for Expression {
    fn evaluate(&self, env: &mut Environment) -> Result<Value, InterpreterError> {
        match self {
            Expression::Assign(a) => a.evaluate(env),
            Expression::Binary(b) => b.evaluate(env),
            Expression::Call(c) => c.evaluate(env),
            Expression::Get(g) => g.evaluate(env),
            Expression::Grouping(g) => g.evaluate(env),
            Expression::Literal(l) => l.evaluate(env),
            Expression::Set(s) => s.evaluate(env),
            Expression::Super(s) => s.evaluate(env),
            Expression::This(t) => t.evaluate(env),
            Expression::Unary(u) => u.evaluate(env),
            Expression::Variable(v) => v.evaluate(env),
            Expression::Logical(l) => l.evaluate(env),
        }
    }
}
