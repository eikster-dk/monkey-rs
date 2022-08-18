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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.read_char() {
            Some('=') => Token::Assign,
            Some(';') => Token::SemiColon,
            Some('(') => Token::LParenthesis,
            Some(')') => Token::RParenthesis,
            Some(',') => Token::Comma,
            Some('+') => Token::Plus,
            Some('{') => Token::LBrace,
            Some('}') => Token::RBrace,
            Some(x) if x.is_alphabetic() || x == '_' => {
                return self.read_identifier(x);
            }
            Some(x) if x.is_numeric() => {
                return self.read_number(x);
            }
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
            Token::SemiColon,
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
            Token::SemiColon,
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
"#;

        let expected_tokens = vec![
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
            Token::Let,
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int(10),
            Token::SemiColon,
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
            Token::SemiColon,
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
            Token::SemiColon,
            Token::EOF,
        ];

        let mut l = Lexer::new(input);

        for expected_token in expected_tokens {
            let token = l.next_token();

            assert_eq!(expected_token, token);
        }
    }
}
