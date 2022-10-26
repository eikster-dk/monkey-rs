use std::{iter::Peekable, str::Chars};

use crate::tokens::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input: input.chars().peekable(),
        }
    }

    fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.read_char() {
            Some('=') => self.assign_or_equal(),
            Some(';') => Token::Semicolon,
            Some('(') => Token::LParenthesis,
            Some(')') => Token::RParenthesis,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some('!') => self.bang_or_not_equal(),
            Some('<') => Token::LT,
            Some('>') => Token::GT,
            Some('-') => Token::Minus,
            Some('/') => Token::Slash,
            Some('*') => Token::Asterisk,
            Some(x) if x.is_alphabetic() || x == '_' => self.read_identifier(x),
            Some(x) if x.is_numeric() => self.read_number(x),
            None => Token::EOF,
            _ => Token::Illegal,
        };

        tok
    }

    fn read_char(&mut self) -> Option<char> {
        self.input.next()
    }

    fn read_identifier(&mut self, ch: char) -> Token {
        let mut s = String::new();
        s.push(ch);

        loop {
            match self.input.peek() {
                Some(x) if x.is_alphabetic() || x == &'_' => {
                    let x = self.read_char();
                    s.push(x.unwrap())
                }
                _ => break,
            }
        }

        let token = match s.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            "if" => Token::If,
            "else" => Token::Else,
            "true" => Token::Boolean(true),
            "false" => Token::Boolean(false),
            "return" => Token::Return,
            _ => Token::Identifier(s),
        };

        token
    }

    fn read_number(&mut self, ch: char) -> Token {
        let mut s = String::new();
        s.push(ch);

        loop {
            match self.input.peek() {
                Some(x) if x.is_numeric() => {
                    let ch = self.read_char().unwrap();
                    s.push(ch);
                }
                _ => break,
            }
        }

        let n: isize = s.parse().unwrap();

        Token::Int(n)
    }

    fn assign_or_equal(&mut self) -> Token {
        match self.input.peek() {
            Some(&'=') => {
                self.input.next();
                Token::Equal
            }
            _ => Token::Assign,
        }
    }

    fn bang_or_not_equal(&mut self) -> Token {
        match self.input.peek() {
            Some(&'=') => {
                self.input.next();
                Token::NotEqual
            }
            _ => Token::Bang,
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.input.peek() {
                Some(x) if x.is_whitespace() => {
                    self.input.next();
                }
                _ => break,
            }
        }
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();

        match tok {
            Token::EOF => None,
            _ => Some(tok),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Token;

    #[test]
    fn simple_parse_test() {
        let input = "=+(){},;^";

        let mut l = Lexer::new(input);

        let expected_results = [
            Token::Assign,
            Token::Plus,
            Token::LParenthesis,
            Token::RParenthesis,
            Token::LBrace,
            Token::RBrace,
            Token::Comma,
            Token::Semicolon,
            Token::Illegal,
            Token::EOF,
        ];

        for expected in expected_results {
            let actual = l.next_token();

            assert_eq!(actual, expected);
        }
    }

    #[test]
    fn next_token_simple() {
        let input = "let x = 5;";

        let tokens = vec![
            Token::Let,
            Token::Identifier("x".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for token in tokens {
            let resulted_token = l.next_token();
            assert_eq!(token, resulted_token);
        }
    }

    #[test]
    fn test_next_token_advanced() {
        let input = r#"
let five = 5;
let ten = 10;

let add = fn(x,y) {
 x + y;
}

let result = add(five, ten);

!-/*5;
5 < 10 > 5;
"#;

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::Semicolon,
            Token::Let,
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::LParenthesis,
            Token::Identifier("x".to_string()),
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::RParenthesis,
            Token::LBrace,
            Token::Identifier("x".to_string()),
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::RBrace,
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::LParenthesis,
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::RParenthesis,
            Token::Semicolon,
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int(5),
            Token::Semicolon,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::GT,
            Token::Int(5),
            Token::Semicolon,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for expected_token in expected_tokens {
            let token = l.next_token();

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn can_parse_return_lt_gt_and_boolean() {
        let input = r#"
        if (5 < 10) {
           return true;
        } else {
          return false;
        }

        10 > 5;
        "#;

        let expected_tokens = vec![
            Token::If,
            Token::LParenthesis,
            Token::Int(5),
            Token::LT,
            Token::Int(10),
            Token::RParenthesis,
            Token::LBrace,
            Token::Return,
            Token::Boolean(true),
            Token::Semicolon,
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            Token::Return,
            Token::Boolean(false),
            Token::Semicolon,
            Token::RBrace,
            Token::Int(10),
            Token::GT,
            Token::Int(5),
            Token::Semicolon,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for expected_token in expected_tokens {
            let token = l.next_token();

            assert_eq!(expected_token, token);
        }
    }

    #[test]
    fn can_parse_equal_and_not_equal() {
        let input = r#"
          5 != 10;
          10 == 10;
        "#;

        let expected_tokens = vec![
            Token::Int(5),
            Token::NotEqual,
            Token::Int(10),
            Token::Semicolon,
            Token::Int(10),
            Token::Equal,
            Token::Int(10),
            Token::Semicolon,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for expected_token in expected_tokens {
            let token = l.next_token();

            assert_eq!(expected_token, token);
        }
    }
}
