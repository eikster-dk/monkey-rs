use std::usize;

use crate::tokens::Token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    current_char: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Lexer {
            input,
            position: 0,
            read_position: 0,
            current_char: 0,
        };

        l.read_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.current_char {
            b'=' => Token::Assign,
            b';' => Token::SemiColon,
            b'(' => Token::LParenthesis,
            b')' => Token::RParenthesis,
            b',' => Token::Comma,
            b'+' => Token::Plus,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                return self.read_identifier();
            }
            b'0'..=b'9' => {
                return self.read_number();
            }
            0 => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_char();
        tok
    }

    // posible improvement: move away from ASCII and improve the readChar to iterate over UTF8
    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = 0
        } else {
            self.current_char = self.input.as_bytes()[self.read_position]
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.position;
        while is_letter(self.current_char) {
            self.read_char();
        }

        let x = &self.input[pos..self.position];

        let token = match x {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Identifier(x.to_string()),
        };

        token
    }

    fn read_number(&mut self) -> Token {
        let pos = self.position;

        loop {
            match self.current_char {
                b'0'..=b'9' => {
                    self.read_char();
                }
                _ => {
                    break;
                }
            }
        }

        let slice = &self.input[pos..self.position];

        let n: isize = slice.trim().parse().unwrap();

        Token::Int(n)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char {
                b' ' | b'\t' | b'\n' | b'\r' => self.read_char(),
                _ => break,
            }
        }
    }
}

fn is_letter(ch: u8) -> bool {
    b'a' <= ch && ch <= b'z' || b'A' <= ch && ch <= b'Z' || ch == b'_'
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokens::Token;

    #[test]
    fn parse_number() {
        let input = "12q";
        let mut l = Lexer::new(input);

        let t = l.read_number();

        assert_eq!(Token::Int(12), t);
        assert_eq!(b'q', l.current_char);
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
