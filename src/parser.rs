use std::iter::Peekable;

use crate::{
    ast::{Expression, InfixOperator, PrefixOperator, Program, Statement},
    lexer::Lexer,
    tokens::Token,
};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(l: Lexer<'a>) -> Parser {
        Parser {
            lexer: l.peekable(),
            errors: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Program {
        let mut stmts = vec![];

        while let Some(token) = self.lexer.next() {
            match token {
                Token::Let => {
                    let stmt = self.parse_let_statement();
                    match stmt {
                        Ok(s) => stmts.push(s),
                        Err(e) => self.errors.push(e),
                    }
                }
                Token::Return => {
                    let stmt = self.parse_return_statement();
                    match stmt {
                        Ok(s) => stmts.push(s),
                        Err(e) => self.errors.push(e),
                    }
                }
                token => {
                    let stmt = self.parse_expression_statement(token);
                    match stmt {
                        Ok(s) => stmts.push(s),
                        Err(e) => self.errors.push(e),
                    }
                }
            };
        }

        Program::new(stmts)
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name = match self.lexer.next() {
            Some(Token::Identifier(name)) => name.to_string(),
            _ => return Err("Expected to find an identifier token".to_string()),
        };

        if let Some(Token::Assign) = self.lexer.next() {
            self.lexer.next();
        } else {
            return Err("Expected to find an assign token".to_string());
        }

        //TODO: fix expression part
        //TODO: go to next token until we reach semicolon

        // improve this:
        while Some(&Token::Semicolon) != self.lexer.peek() {
            self.lexer.next();
        }
        self.lexer.next();

        Ok(Statement::Let(name, Expression::Identifier("".to_string())))
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        //TODO: fix expression part
        //TODO: go to next token until we reach semicolon

        // improve this:
        while Some(&Token::Semicolon) != self.lexer.peek() {
            self.lexer.next();
        }
        self.lexer.next();

        Ok(Statement::Return(Expression::Identifier("".to_string())))
    }

    fn parse_expression_statement(&mut self, token: Token) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest, token);
        if let Some(&Token::Semicolon) = self.lexer.peek() {
            self.lexer.next();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence, token: Token) -> Expression {
        let left = match token {
            Token::Identifier(name) => Expression::Identifier(name),
            Token::Int(number) => Expression::Integer(number),
            Token::Bang | Token::Minus => self.parse_prefix_operator(token),
            _ => panic!("TODO: Implement more operators??: {:?}", token),
        };

        while let Some(t) = self.lexer.peek() {
            if t != &Token::Semicolon && precedence < Parser::token_precedence(&t) {
                let t = self.lexer.next().unwrap();
                match t {
                    Token::Plus
                    | Token::Minus
                    | Token::Asterisk
                    | Token::Slash
                    | Token::GT
                    | Token::LT
                    | Token::Equal
                    | Token::NotEqual => return self.parse_infix_expression(t, left),
                    _ => return left,
                }
            } else {
                return left;
            }
        }
        left
    }

    fn parse_prefix_operator(&mut self, token: Token) -> Expression {
        let prefix = match token {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            _ => panic!("TODO: This should really return an option type"),
        };

        if let Some(tt) = self.lexer.next() {
            let right = self.parse_expression(Precedence::Prefix, tt);
            Expression::Prefix(prefix, Box::new(right))
        } else {
            panic!("TODO: This should really return an option type")
        }
    }

    fn parse_infix_expression(&mut self, token: Token, left: Expression) -> Expression {
        let precedence = Parser::token_precedence(&token);

        let operator = match token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::GT => InfixOperator::GT,
            Token::LT => InfixOperator::LT,
            Token::Equal => InfixOperator::Equals,
            Token::NotEqual => InfixOperator::NotEquals,
            _ => panic!("todo: handle incorrect operator token"),
        };

        let t = self.lexer.next().unwrap(); // TODO: don't use unwrap
        let right = self.parse_expression(precedence, t);

        Expression::Infix(operator, Box::new(left), Box::new(right))
    }

    fn token_precedence(token: &Token) -> Precedence {
        match token {
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::LT | Token::GT => Precedence::LessGreater,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::Equal | Token::NotEqual => Precedence::Equals,
            _ => panic!("TODO: handle if a token somehow get this far"),
        }
    }
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest = 0,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // addition
    Product,     // Multiplier
    Prefix,      // -x or !x
    Call,        // a call to my fn()
}

#[cfg(test)]
mod tests {
    use crate::ast::{InfixOperator, PrefixOperator};

    use super::*;

    #[test]
    fn parsing_let_statement() {
        let input = r#"
        let x = 5;
        let y = 10;
        let foo = 838383;
"#;
        let expected_statements = vec![
            Statement::Let("x".to_string(), Expression::Identifier("".to_string())),
            Statement::Let("y".to_string(), Expression::Identifier("".to_string())),
            Statement::Let("foo".to_string(), Expression::Identifier("".to_string())),
        ];

        validate_and_parse_program(input, expected_statements);
    }

    #[test]
    fn parsing_return_statement() {
        let input = r#"
            return 5;
            return 10;
            return add(15);
        "#;

        let expected_statements = vec![
            Statement::Return(Expression::Identifier("".to_string())),
            Statement::Return(Expression::Identifier("".to_string())),
            Statement::Return(Expression::Identifier("".to_string())),
        ];

        validate_and_parse_program(input, expected_statements);
    }

    #[test]
    fn parsing_identifier_expression() {
        let input = r#"
          foobar;
        "#;

        let expected_statements = vec![Statement::Expression(Expression::Identifier(
            "foobar".to_string(),
        ))];

        validate_and_parse_program(input, expected_statements);
    }

    #[test]
    fn parsing_integer_expression() {
        let input = r#"
            5;
        "#;

        let expected_statements = vec![Statement::Expression(Expression::Integer(5))];

        validate_and_parse_program(input, expected_statements);
    }

    #[test]
    fn parsing_prefix_expression() {
        let input = r#"
            !5;
            -15;
        "#;

        let expected_statements = vec![
            Statement::Expression(Expression::Prefix(
                PrefixOperator::Bang,
                Box::new(Expression::Integer(5)),
            )),
            Statement::Expression(Expression::Prefix(
                PrefixOperator::Minus,
                Box::new(Expression::Integer(15)),
            )),
        ];

        validate_and_parse_program(input, expected_statements);
    }

    #[test]
    fn parsing_infix_expression() {
        let input = r#"
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 == 5;
            5 != 5;
        "#;

        let expected_statements = vec![
            new_infix_expression(
                InfixOperator::Plus,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::Minus,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::Multiply,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::Divide,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::GT,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::LT,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::Equals,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
            new_infix_expression(
                InfixOperator::NotEquals,
                Expression::Integer(5),
                Expression::Integer(5),
            ),
        ];

        validate_and_parse_program(input, expected_statements);
    }

    fn new_infix_expression(
        operator: InfixOperator,
        left: Expression,
        right: Expression,
    ) -> Statement {
        Statement::Expression(Expression::Infix(operator, Box::new(left), Box::new(right)))
    }

    fn validate_and_parse_program(input: &str, expected_statements: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();
        if parser.errors.len() > 0 {
            panic!("error occured in parser: {:?}", parser.errors);
        }

        if expected_statements.len() != program.statements.len() {
            panic!("the program statements and expected statements are not the same. expected: {}, got: {}\n\n {:?}",
                expected_statements.len(),
                program.statements.len(),
                program.statements);
        }

        let pairs = expected_statements.iter().zip(program.statements.iter());

        for (expected, actual) in pairs {
            assert_eq!(expected, actual);
        }
    }
}
