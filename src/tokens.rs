#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    EOF,
    Identifier(String),
    Int(isize),
    Assign,
    Plus,
    Comma,
    SemiColon,
    LParenthesis,
    RParenthesis,
    LBrace,
    RBrace,
    Function,
    Let,
}
