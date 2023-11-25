use crate::token::{Token, TokenType, TokenType::*};
use crate::ast::expressions::*;
use crate::ast::expressions::helpers::*;
use crate::error::LoxError;

type ParseResult<T> = Result<T, LoxError>;

pub struct Parser {
    // Guaranteed to have an EOF token at the end
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(mut tokens: Vec<Token>) -> Parser {
        if tokens.len() == 0 || tokens[tokens.len() - 1].token_type != EOF {
            // Add an EOF token if there are no tokens, or if the last token is not EOF
            tokens.push(Token::new(EOF, "".to_string(), 0));
        }

        Parser { tokens, current: 0 }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap()
    }

    fn previous(&self) -> &Token {
        self.tokens.get(self.current - 1).unwrap()
    }

    fn advance(&mut self) -> &Token {
        if self.current < self.tokens.len() {
            self.current += 1;
            &self.tokens[self.current - 1]
        } else {
            // Return the EOF token
            // TODO: warn that we are at the end of the file
            self.tokens.last().unwrap()
        }
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.peek().token_type == token_type
    }

    fn match_token(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> ParseResult<&Token> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(self.error(message))
        }
    }

    fn error(&self, message: &str) -> LoxError {
        let token = self.peek();
        if token.token_type == EOF {
            LoxError::new(token.line, " at end".to_string(), message.to_string())
        } else {
            LoxError::new(token.line, format!(" at '{}'", token.lexeme), message.to_string())
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().token_type == SEMICOLON {
                return;
            }

            match self.peek().token_type {
                CLASS | FUN | VAR | FOR | IF | WHILE | PRINT | RETURN => return,
                _ => { self.advance(); },
            }
        }
    }

    pub fn parse(&mut self) -> ParseResult<Expression> {
        self.expression()
    }

    fn expression(&mut self) -> ParseResult<Expression> {
        self.equality()
    }

    fn equality(&mut self) -> ParseResult<Expression> {
        let mut expr = self.comparison()?;

        while self.match_token(&[BANG_EQUAL, EQUAL_EQUAL]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> ParseResult<Expression> {
        let mut expr = self.term()?;

        while self.match_token(&[GREATER, GREATER_EQUAL, LESS, LESS_EQUAL]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn term(&mut self) -> ParseResult<Expression> {
        let mut expr = self.factor()?;

        while self.match_token(&[MINUS, PLUS]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> ParseResult<Expression> {
        let mut expr = self.unary()?;

        while self.match_token(&[SLASH, STAR]) {
            let operator = self.previous().clone();
            let right = self.unary()?; // TODO: better error message
            expr = binary(expr, operator, right);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> ParseResult<Expression> {
        if self.match_token(&[BANG, MINUS]) {
            let operator = self.previous().clone();
            let right = self.unary()?; // TODO: better error message
            Ok(unary(operator, right))
        } else {
            self.primary()
        }
    }

    fn primary(&mut self) -> ParseResult<Expression> {
        let token = self.advance();

        match token.token_type {
            FALSE => Ok(literal(Literal::Boolean(false))),
            TRUE => Ok(literal(Literal::Boolean(true))),
            NIL => Ok(literal(Literal::Nil)),
            NUMBER => {
                let value = token.lexeme.parse::<f64>().unwrap();
                Ok(literal(Literal::Number(value)))
            },
            STRING => Ok(literal(Literal::String(token.lexeme[1..token.lexeme.len() - 1].to_string()))),
            LEFT_PAREN => {
                let line = token.line;
                let lexeme = token.lexeme.clone();
                if let Ok(expr) = self.expression() {
                    self.consume(RIGHT_PAREN, "Expect ')' after expression")?;
                    Ok(grouping(expr))
                } else {
                    Err(LoxError::new(line, lexeme, "Expected expression after '('".to_string()))
                }
            }
            _ => Err(LoxError::new(token.line, token.lexeme.clone(), "Expected expression".to_string()))
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{Token, TokenType::*};
    use crate::ast::expressions::helpers::*;

    #[test]
    fn parse_unary() {
        let tokens = vec![
            Token::new(MINUS, "-".to_string(), 1),
            Token::new(NUMBER, "1".to_string(), 1),
            Token::new(BANG, "!".to_string(), 1),
            Token::new(TRUE, "true".to_string(), 1),
        ];
        let mut parser = super::Parser::new(tokens);
        let expected = vec![
            unary(
                Token::new(MINUS, "-".to_string(), 1),
                literal(super::Literal::Number(1.0))
            ),
            unary(
                Token::new(BANG, "!".to_string(), 1),
                literal(super::Literal::Boolean(true))
            ),
        ];
        let mut results = Vec::new();
        while let Ok(expr) = parser.unary() {
            results.push(expr);
        }
        assert_eq!(results, expected);
    }

    #[test]
    fn parse_primary() {
        let tokens = vec![
            Token::new(FALSE, "false".to_string(), 1),
            Token::new(TRUE, "true".to_string(), 1),
            Token::new(NIL, "nil".to_string(), 1),
            Token::new(NUMBER, "1".to_string(), 1),
            Token::new(STRING, "\"string\"".to_string(), 1),
        ];

        let mut parser = super::Parser::new(tokens);
        let expected = vec![
            literal(super::Literal::Boolean(false)),
            literal(super::Literal::Boolean(true)),
            literal(super::Literal::Nil),
            literal(super::Literal::Number(1.0)),
            literal(super::Literal::String("string".to_string())),
        ];
        let mut results = Vec::new();
        while let Ok(expr) = parser.primary() {
            results.push(expr);
        }
        assert_eq!(results, expected);
    }
}
