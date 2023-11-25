use crate::ast::expressions::Expression;

#[derive(Clone,Debug,PartialEq)]
pub enum Statement {
    Expression(ExpressionStatement),
    Print(PrintStatement)
}

#[derive(Clone,Debug,PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression
}

#[derive(Clone,Debug,PartialEq)]
pub struct PrintStatement {
    pub expression: Expression
}

impl Statement {
    pub fn expression(expression: Expression) -> Statement {
        Statement::Expression(ExpressionStatement { expression })
    }

    pub fn print(expression: Expression) -> Statement {
        Statement::Print(PrintStatement { expression })
    }
}
