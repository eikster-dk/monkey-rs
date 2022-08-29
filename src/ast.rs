use core::fmt;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String),
    Integer(isize),
    Bool(bool),
    Prefix(PrefixOperator, Box<Expression>),
    Infix(Box<Expression>, InfixOperator, Box<Expression>),
    If(Box<Expression>, BlockStatement, BlockStatement),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Expression::Identifier(ident) => format!("{}", ident),
            Expression::Integer(int) => format!("{}", int),
            Expression::Bool(b) => format!("{}", b),
            Expression::Prefix(op, exp) => format!("({}{})", op, exp),
            Expression::Infix(left, op, right) => format!("({} {} {})", left, op, right),
            Expression::If(condition, consequnce, alternative) => format!("if {}", condition),
        };

        write!(f, "{}", s)
    }
}

pub type BlockStatement = Vec<Statement>;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
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
