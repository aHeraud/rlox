use std::fmt::{Debug, Display};
use std::rc::Rc;
use crate::ast::statements::*;
use crate::error::RuntimeError;
use crate::token::TokenType::*;
use super::{Value, Environment, Evaluate, InterpreterError};

impl Evaluate<()> for ExpressionStatement {
    fn evaluate(&self, env: &mut Box<Environment>
    ) -> Result<(),InterpreterError> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Evaluate<()> for PrintStatement {
    fn evaluate(&self, env: &mut Box<Environment>) -> Result<(),InterpreterError> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}

impl Evaluate<()> for VarStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
        let value = match &self.initializer {
            Some(e) => e.evaluate(environment)?,
            None => Value::Nil
        };
        environment.define(&self.name.lexeme, value);
        Ok(())
    }
}

impl Evaluate<()> for BlockStatement {
    fn evaluate<'a>(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
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

impl Evaluate<()> for IfStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
        if self.condition.evaluate(environment)?.is_truthy() {
            self.then_branch.evaluate(environment)?;
        } else if let Some(else_branch) = &self.else_branch {
            else_branch.evaluate(environment)?;
        }
        Ok(())
    }
}

impl Evaluate<()> for WhileStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
        while self.condition.evaluate(environment)?.is_truthy() {
            self.body.evaluate(environment)?;
        }
        Ok(())
    }
}

impl Evaluate<()> for FunctionStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
        environment.define(&self.name.lexeme, Value::Function(Rc::new(Box::new(self.clone()))));
        Ok(())
    }
}

impl Evaluate<()> for ReturnStatement {
    fn evaluate(&self, environment: &mut Box<Environment>) -> Result<(),InterpreterError> {
        let value = match &self.value {
            Some(e) => e.evaluate(environment)?,
            None => Value::Nil
        };
        Err(InterpreterError::Return(self.keyword.clone(), value))
    }
}

impl Evaluate<()> for Statement {
    fn evaluate(&self, env: &mut Box<Environment>) -> Result<(),InterpreterError> {
        match self {
            Statement::Expression(e) => e.evaluate(env),
            Statement::Print(p) => p.evaluate(env),
            Statement::Var(v) => v.evaluate(env),
            Statement::Block(b) => b.evaluate(env),
            Statement::If(i) => i.evaluate(env),
            Statement::While(w) => w.evaluate(env),
            Statement::Function(f) => f.evaluate(env),
            Statement::Return(r) => r.evaluate(env),
        }
    }
}
