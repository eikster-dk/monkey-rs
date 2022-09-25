use core::fmt;
use serde::{Serialize, Deserialize};

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Statement {
    Let(String, Expression),
    Return(Expression),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Statement::Let(name, exp) => format!("let {} = {};", name, exp),
            Statement::Return(exp) => format!("return {};", exp),
            Statement::Expression(exp) => format!("{}", exp),
        };

        write!(f, "{}", s)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(isize),
    Bool(bool),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(Box<Expression>, InfixOperator, Box<Expression>),
    If(Box<Expression>, BlockStatement, BlockStatement),
    Function(Vec<String>, BlockStatement),
    Call(Box<Expression>, Vec<Expression>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Expression::Identifier(ident) => format!("{}", ident),
            Expression::Integer(int) => format!("{}", int),
            Expression::Bool(b) => format!("{}", b),
            Expression::Prefix(op, exp) => format!("({}{})", op, exp),
            Expression::Infix(left, op, right) => format!("({} {} {})", left, op, right),
            Expression::If(condition, consequnce, alternative) => {
                let consequnce_str: Vec<String> =
                    consequnce.iter().map(|x| x.to_string()).collect();
                let mut s = format!("if {} then {}", condition, consequnce_str.join(""));
                if alternative.len() > 0 {
                    let alternative_str: Vec<String> =
                        alternative.iter().map(|x| x.to_string()).collect();
                    s = format!("{} else {}", s, alternative_str.join(""));
                }

                s
            }
            Expression::Function(parameters, block) => {
                let parameters = parameters.join(" ");

                let block_stmt: Vec<String> = block.iter().map(|x| x.to_string()).collect();

                let s = format!("fn ({}) {{ {} }}", parameters, block_stmt.join(""));

                s
            }
            Expression::Call(identifier, arguments) => {
                let args: Vec<String> = arguments.iter().map(|x| x.to_string()).collect();

                format!("{}({})", identifier.to_string(), args.join(", "))
            }
        };

        write!(f, "{}", s)
    }
}

pub type BlockStatement = Vec<Statement>;

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum PrefixOperator {
    Minus,
    Bang,
}

impl fmt::Display for PrefixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            PrefixOperator::Minus => format!("-"),
            PrefixOperator::Bang => format! {"!"},
        };

        write!(f, "{}", s)
    }
}

#[derive(Serialize, Deserialize, Debug, PartialEq)]
pub enum InfixOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    LT,
    GT,
    Equals,
    NotEquals,
}

impl fmt::Display for InfixOperator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            InfixOperator::Plus => format!("+"),
            InfixOperator::Minus => format!("-"),
            InfixOperator::Multiply => format!("*"),
            InfixOperator::Divide => format!("/"),
            InfixOperator::LT => format!("<"),
            InfixOperator::GT => format!(">"),
            InfixOperator::Equals => format!("=="),
            InfixOperator::NotEquals => format!("!="),
        };

        write!(f, "{}", s)
    }
}


#[derive(Serialize, Deserialize, Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Program {
        Program { statements }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s: Vec<String> = self
            .statements
            .iter()
            .map(|stmt| stmt.to_string())
            .collect();

        write!(f, "{}", s.join("\n"))
    }
}
