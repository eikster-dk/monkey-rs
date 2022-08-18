use std::usize;

use crate::tokens::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    current_char: char, // refactor to use Option<char>
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Lexer {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            current_char: '\0',
        };

        l.read_char();

        return l;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.current_char {
            '=' => Token::Assign,
            ';' => Token::SemiColon,
            '(' => Token::LParenthesis,
            ')' => Token::RParenthesis,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            x if x.is_alphabetic() || x == '_' => {
                return self.read_identifier();
            }
            x if x.is_numeric() => {
                return self.read_number();
            }
            '\0' => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_char();
        tok
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_identifier(&mut self) -> Token {
        let pos = self.position;

        loop {
            match self.current_char {
                x if x.is_alphabetic() || x == '_' => self.read_char(),
                _ => break,
            }
        }

        let slice = &self.input[pos..self.position];
        let word = String::from_iter(slice);

        let token = match word.as_str() {
            "fn" => Token::Function,
            "let" => Token::Let,
            _ => Token::Identifier(word),
        };

        token
    }

    fn read_number(&mut self) -> Token {
        let pos = self.position;

        loop {
            match self.current_char {
                x if x.is_numeric() => {
                    self.read_char();
                }
                _ => break,
                
            }
        }

        let slice = &self.input[pos..self.position];

        let n: isize = String::from_iter(slice).parse().unwrap();

        Token::Int(n)
    }

    fn skip_whitespace(&mut self) {
        loop {
            match self.current_char {
                x if x.is_whitespace() => self.read_char(),
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
    fn parse_number() {
        let input = "12q";
        let mut l = Lexer::new(input);

        let t = l.read_number();

        assert_eq!(Token::Int(12), t);
        assert_eq!('q', l.current_char);
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
