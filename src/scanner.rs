use crate::error::{LoxError, ScanErrors};
use super::token::{Token, TokenType};

pub struct Scanner {
    source: String,
    tokens: Vec<Token>,
    errors: Vec<LoxError>,

    start: usize,
    current: usize,
    line: usize
}

impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,
            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1
        }
    }

    pub fn scan_tokens(mut self) -> Result<Vec<Token>, ScanErrors> {
        while !self.is_at_end() {
            self.scan_token();
        }

        self.add_token(TokenType::EOF);

        if self.errors.len() > 0 {
            Err(ScanErrors { errors: self.errors })
        } else {
            Ok(self.tokens)
        }
    }

    fn scan_token(&mut self) {
        if let Some(c) = self.advance() {
            match c {
                ' ' | '\r' | '\t' => {},
                '\n' => self.line += 1,
                '(' => self.add_token(TokenType::LEFT_PAREN),
                ')' => self.add_token(TokenType::RIGHT_PAREN),
                '{' => self.add_token(TokenType::LEFT_BRACE),
                '}' => self.add_token(TokenType::RIGHT_BRACE),
                ',' => self.add_token(TokenType::COMMA),
                '.' => self.add_token(TokenType::DOT),
                '-' => self.add_token(TokenType::MINUS),
                '+' => self.add_token(TokenType::PLUS),
                ';' => self.add_token(TokenType::SEMICOLON),
                '*' => self.add_token(TokenType::STAR),
                '/' => {
                    if let Some('/') = self.peek() {
                        // Line comment, discard all characters until we reach the end of
                        // the line
                        while let Some(n) = self.peek() {
                            if n == '\n' {
                                break;
                            } else {
                                self.advance();
                            }
                        }
                    } else {
                        self.add_token(TokenType::SLASH);
                    }
                },
                '!' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add_token(TokenType::BANG_EQUAL);
                    } else {
                        self.add_token(TokenType::BANG);
                    }
                },
                '=' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add_token(TokenType::EQUAL_EQUAL);
                    } else {
                        self.add_token(TokenType::EQUAL);
                    }
                },
                '>' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add_token(TokenType::GREATER_EQUAL);
                    } else {
                        self.add_token(TokenType::GREATER);
                    }
                },
                '<' => {
                    if let Some('=') = self.peek() {
                        self.advance();
                        self.add_token(TokenType::LESS_EQUAL);
                    } else {
                        self.add_token(TokenType::LESS);
                    }
                },
                '"' => self.string_literal(),
                '0'..='9' => self.number_literal(),
                'a'..='z' | 'A'..='Z' | '_' => self.identifier_or_reserved_word(),
                _ => self.errors.push(LoxError::new(self.line, String::from(""), format!("Unexpected character: {}", c)))
            }
            self.start = self.current;
        }
    }

    fn string_literal(&mut self) {
        let mut closed = false;
        while let Some(n) = self.peek() {
            self.advance();
            if n == '"' {
                closed = true;
                break;
            }
        }

        if !closed {
            panic!("Unterminated string literal");
        }

        self.add_token(TokenType::STRING);
    }

    fn number_literal(&mut self) {
        while let Some(n) = self.peek() {
            if n.is_digit(10) {
                self.advance();
            } else {
                break;
            }
        }

        if let Some('.') = self.peek() {
            if let Some('0'..='9') = self.peek_next() {
                self.advance();
                while let Some('0'..='9') = self.peek() {
                    self.advance();
                }
            }
        }

        self.add_token(TokenType::NUMBER);
    }

    fn identifier_or_reserved_word(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = self.source[self.start..self.current].to_string();
        match lexeme.as_str() {
            "and" => self.add_token(TokenType::AND),
            "class" => self.add_token(TokenType::CLASS),
            "else" => self.add_token(TokenType::ELSE),
            "false" => self.add_token(TokenType::FALSE),
            "fun" => self.add_token(TokenType::FUN),
            "for" => self.add_token(TokenType::FOR),
            "if" => self.add_token(TokenType::IF),
            "nil" => self.add_token(TokenType::NIL),
            "or" => self.add_token(TokenType::OR),
            "print" => self.add_token(TokenType::PRINT),
            "return" => self.add_token(TokenType::RETURN),
            "super" => self.add_token(TokenType::SUPER),
            "this" => self.add_token(TokenType::THIS),
            "true" => self.add_token(TokenType::TRUE),
            "var" => self.add_token(TokenType::VAR),
            "while" => self.add_token(TokenType::WHILE),
            _ => self.add_token(TokenType::IDENTIFIER),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }

    fn peek(&self) -> Option<char> {
       self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self.source[self.start..self.current].to_string();
        let token = Token::new(token_type, lexeme, self.line);
        self.tokens.push(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::token::TokenType::*;
    use super::*;

    #[test]
    fn test_scan_reserved_words() {
        let source = String::from("and class else false fun for if nil or print return super this true var while");
        let scanner = Scanner::new(source);
        let tokens: Vec<TokenType> = scanner.scan_tokens().unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(
            tokens,
            vec![AND, CLASS, ELSE, FALSE, FUN, FOR, IF, NIL, OR, PRINT, RETURN, SUPER, THIS, TRUE, VAR, WHILE, EOF]
        )
    }

    #[test]
    fn test_scan_identifier() {
        let source = String::from("foo");
        let scanner = Scanner::new(source);
        let tokens: Vec<TokenType> = scanner.scan_tokens().unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(tokens, vec![IDENTIFIER, EOF]);
    }

    #[test]
    fn test_scan_string_literal() {
        let source = String::from("\"yoshi, dinosaur\"");
        let tokens: Vec<TokenType> = Scanner::new(source).scan_tokens().unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(tokens, vec![STRING, EOF]);
    }

    #[test]
    fn test_scan_integer() {
        let source = String::from("123");
        let tokens: Vec<TokenType> = Scanner::new(source).scan_tokens().unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(tokens, vec![NUMBER, EOF]);
    }

    #[test]
    fn test_scan_float() {
        let source = String::from("123.456");
        let tokens: Vec<TokenType> = Scanner::new(source).scan_tokens().unwrap()
            .into_iter()
            .map(|t| t.token_type)
            .collect();
        assert_eq!(tokens, vec![NUMBER, EOF]);
    }
}
