use crate::ast::expressions::Expression;
use crate::token::Token;

#[derive(Clone,Debug,PartialEq)]
pub enum Statement {
    Class(ClassStatement),
    Expression(ExpressionStatement),
    Print(PrintStatement),
    Var(VarStatement),
    Block(BlockStatement),
    If(IfStatement),
    While(WhileStatement),
    Function(FunctionStatement),
    Return(ReturnStatement),
}

#[derive(Clone,Debug,PartialEq)]
pub struct ClassStatement {
    pub name: Token,
    pub methods: Vec<FunctionStatement>,
}

#[derive(Clone,Debug,PartialEq)]
pub struct ExpressionStatement {
    pub expression: Expression
}

#[derive(Clone,Debug,PartialEq)]
pub struct PrintStatement {
    pub expression: Expression
}

#[derive(Clone,Debug,PartialEq)]
pub struct VarStatement {
    pub name: Token,
    pub initializer: Option<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>
}

#[derive(Clone,Debug,PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub then_branch: Box<Statement>,
    pub else_branch: Option<Box<Statement>>
}

#[derive(Clone,Debug,PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Box<Statement>,
}

#[derive(Clone,Debug,PartialEq)]
pub struct FunctionStatement {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Statement>
}

#[derive(Clone,Debug,PartialEq)]
pub struct ReturnStatement {
    pub keyword: Token,
    pub value: Option<Expression>
}

impl Statement {
    pub fn class(name: Token, methods: Vec<FunctionStatement>) -> Statement {
        Statement::Class(ClassStatement { name, methods })
    }

    pub fn expression(expression: Expression) -> Statement {
        Statement::Expression(ExpressionStatement { expression })
    }

    pub fn print(expression: Expression) -> Statement {
        Statement::Print(PrintStatement { expression })
    }

    pub fn var(name: Token, initializer: Option<Expression>) -> Statement {
        Statement::Var(VarStatement { name, initializer })
    }

    pub fn block(statements: Vec<Statement>) -> Statement {
        Statement::Block(BlockStatement { statements })
    }

    pub fn if_statement(
        condition: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>
    ) -> Statement {
        Statement::If(IfStatement { condition, then_branch, else_branch })
    }

    pub fn while_statement(condition: Expression, body: Box<Statement>) -> Statement {
        Statement::While(WhileStatement { condition, body })
    }

    pub fn function(name: Token, params: Vec<Token>, body: Vec<Statement>) -> Statement {
        Statement::Function(FunctionStatement { name, params, body })
    }

    pub fn return_statement(keyword: Token, value: Option<Expression>) -> Statement {
        Statement::Return(ReturnStatement { keyword, value })
    }
}
