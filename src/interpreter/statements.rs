use std::rc::Rc;
use crate::ast::statements::*;
use super::{Value, Environment, Evaluate, InterpreterError, Closure};

impl Evaluate<()> for ExpressionStatement {
    fn evaluate(&self, env: &mut Environment) -> Result<(), InterpreterError> {
        self.expression.evaluate(env)?;
        Ok(())
    }
}

impl Evaluate<()> for PrintStatement {
    fn evaluate(&self, env: &mut Environment) -> Result<(), InterpreterError> {
        let value = self.expression.evaluate(env)?;
        println!("{}", value);
        Ok(())
    }
}

impl Evaluate<()> for VarStatement {
    fn evaluate(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        let value = match &self.initializer {
            Some(e) => e.evaluate(environment)?,
            None => Value::Nil
        };
        environment.define(&self.name.lexeme, value);
        Ok(())
    }
}

impl Evaluate<()> for BlockStatement {
    fn evaluate<'a>(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        let mut environment = Environment::new().with_enclosing(environment.clone());

        let result = (|| {
            for statement in &self.statements {
                statement.evaluate(&mut environment)?;
            }
            Ok(())
        })();

        result
    }
}

impl Evaluate<()> for IfStatement {
    fn evaluate(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        if self.condition.evaluate(environment)?.is_truthy() {
            self.then_branch.evaluate(environment)?;
        } else if let Some(else_branch) = &self.else_branch {
            else_branch.evaluate(environment)?;
        }
        Ok(())
    }
}

impl Evaluate<()> for WhileStatement {
    fn evaluate(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        while self.condition.evaluate(environment)?.is_truthy() {
            self.body.evaluate(environment)?;
        }
        Ok(())
    }
}

impl Evaluate<()> for FunctionStatement {
    fn evaluate(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        let closure = Box::new(Closure {
            function: self.clone(),
            environment: environment.clone(),
        });
        environment.define(&self.name.lexeme, Value::Function(Rc::new(closure)));
        Ok(())
    }
}

impl Evaluate<()> for ReturnStatement {
    fn evaluate(&self, environment: &mut Environment) -> Result<(), InterpreterError> {
        let value = match &self.value {
            Some(e) => e.evaluate(environment)?,
            None => Value::Nil
        };
        Err(InterpreterError::Return(self.keyword.clone(), value))
    }
}

impl Evaluate<()> for Statement {
    fn evaluate(&self, env: &mut Environment) -> Result<(), InterpreterError> {
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
