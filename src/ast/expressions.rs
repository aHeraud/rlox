use std::fmt;

use crate::token::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    String(String),
    Number(f64),
    Boolean(bool),
    Nil
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Literal::String(ref s) => write!(f, "{}", s),
            Literal::Number(ref n) => write!(f, "{}", n),
            Literal::Boolean(ref b) => write!(f, "{}", b),
            Literal::Nil => write!(f, "nil")
        }
    }
}

#[derive(Clone,Debug,PartialEq)]
pub enum Expression {
    Assign(AssignmentExpression),
    Binary(BinaryExpression),
    Call(CallExpression),
    Get(GetExpression),
    Grouping(GroupingExpression),
    Literal(LiteralExpression),
    Logical(LogicalExpression),
    Set(SetExpression),
    Super(SuperExpression),
    This(ThisExpression),
    Unary(UnaryExpression),
    Variable(VariableExpression),
}

#[derive(Clone,Debug,PartialEq)]
pub struct AssignmentExpression {
    pub(crate) name: Token,
    pub(crate) value: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct BinaryExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct CallExpression {
    pub callee: Box<Expression>,
    pub paren: Token,
    pub arguments: Vec<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct GetExpression {
    pub object: Box<Expression>,
    pub name: Token
}

#[derive(Clone,Debug,PartialEq)]
pub struct GroupingExpression {
    pub expression: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct LiteralExpression {
    pub value: Literal
}

#[derive(Clone,Debug,PartialEq)]
pub struct LogicalExpression {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct SetExpression {
    pub object: Box<Expression>,
    pub name: Token,
    pub value: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct SuperExpression {
    pub keyword: Token,
    pub method: Token
}

#[derive(Clone,Debug,PartialEq)]
pub struct ThisExpression {
    pub keyword: Token
}

#[derive(Clone,Debug,PartialEq)]
pub struct UnaryExpression {
    pub operator: Token,
    pub right: Box<Expression>
}

#[derive(Clone,Debug,PartialEq)]
pub struct VariableExpression {
    pub name: Token
}


impl Expression {
    pub fn assign(name: Token, value: Expression) -> Expression {
        Expression::Assign(AssignmentExpression {
            name,
            value: Box::new(value)
        })
    }

    pub fn binary(left: Expression, operator: Token, right: Expression) -> Expression {
        Expression::Binary(BinaryExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right)
        })
    }

    pub fn call(callee: Expression, paren: Token, arguments: Vec<Expression>) -> Expression {
        Expression::Call(CallExpression {
            callee: Box::new(callee),
            paren,
            arguments
        })
    }

    pub fn get(object: Expression, name: Token) -> Expression {
        Expression::Get(GetExpression {
            object: Box::new(object),
            name
        })
    }

    pub fn grouping(expression: Expression) -> Expression {
        Expression::Grouping(GroupingExpression {
            expression: Box::new(expression)
        })
    }

    pub fn literal(value: Literal) -> Expression {
        Expression::Literal(LiteralExpression {
            value
        })
    }

    pub fn logical(left: Expression, operator: Token, right: Expression) -> Expression {
        Expression::Logical(LogicalExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right)
        })
    }

    pub fn set(object: Expression, name: Token, value: Expression) -> Expression {
        Expression::Set(SetExpression {
            object: Box::new(object),
            name,
            value: Box::new(value)
        })
    }

    pub fn super_(keyword: Token, method: Token) -> Expression {
        Expression::Super(SuperExpression {
            keyword,
            method
        })
    }

    pub fn this(keyword: Token) -> Expression {
        Expression::This(ThisExpression {
            keyword
        })
    }

    pub fn unary(operator: Token, right: Expression) -> Expression {
        Expression::Unary(UnaryExpression {
            operator,
            right: Box::new(right)
        })
    }

    pub fn variable(name: Token) -> Expression {
        Expression::Variable(VariableExpression {
            name
        })
    }
}
