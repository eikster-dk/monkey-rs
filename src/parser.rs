use std::iter::Peekable;

use crate::{
    ast::{BlockStatement, Expression, InfixOperator, PrefixOperator, Program, Statement},
    lexer::Lexer,
    tokens::Token,
};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    pub errors: Vec<String>,
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
            let result = self.parse_statement(token);
            match result {
                Ok(s) => stmts.push(s),
                Err(e) => self.errors.push(e),
            }
        }
        Program::new(stmts)
    }

    fn parse_statement(&mut self, token: Token) -> Result<Statement, String> {
        match token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            token => self.parse_expression_statement(token),
        }
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name = match self.lexer.next() {
            Some(Token::Identifier(name)) => name.to_string(),
            _ => return Err("Expected to find an identifier token".to_string()),
        };

        if let Some(Token::Assign) = self.lexer.next() {
            let t = self.lexer.next().unwrap();
            let expression = self.parse_expression(Precedence::Lowest, t).unwrap(); // todo: fix

            self.lexer.next();

            Ok(Statement::Let(name, expression))
        } else {
            return Err("Expected to find an assign token".to_string());
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let t = self.lexer.next().unwrap();
        let expression = self.parse_expression(Precedence::Lowest, t).unwrap(); // todo: fix

        self.lexer.next();

        Ok(Statement::Return(expression))
    }

    fn parse_expression_statement(&mut self, token: Token) -> Result<Statement, String> {
        let expression = self.parse_expression(Precedence::Lowest, token).unwrap(); // todo: fix
        if let Some(&Token::Semicolon) = self.lexer.peek() {
            self.lexer.next();
        }

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, precedence: Precedence, token: Token) -> Option<Expression> {
        let opt_left = match token {
            Token::Identifier(name) => Some(Expression::Identifier(name)),
            Token::Int(number) => Some(Expression::Integer(number)),
            Token::Bang | Token::Minus => self.parse_prefix_operator(token),
            Token::Boolean(b) => Some(Expression::Bool(b)),
            Token::LParenthesis => self.parse_group_expression(),
            Token::If => self.parse_if_expression(),
            Token::Function => self.parse_function(),
            _ => {
                self.errors.push(format!("No prefix parser for {:?} found", token));
                None
            },
        };

        if let Some(mut left) = opt_left {
            while let Some(next_token) = self.lexer.peek() {
                if next_token != &Token::Semicolon
                    && precedence < Parser::token_precedence(&next_token)
                {
                    let t = self.lexer.next()?;
                    match t {
                        Token::Plus
                        | Token::Minus
                        | Token::Asterisk
                        | Token::Slash
                        | Token::GT
                        | Token::LT
                        | Token::Equal
                        | Token::NotEqual => left = self.parse_infix_expression(t, left)?,
                        Token::LParenthesis => left = self.parse_call_expression(left)?,
                        _ => (),
                    }
                } else {
                    return Some(left);
                }
            }

            return Some(left);
        }

        None
    }

    fn parse_prefix_operator(&mut self, token: Token) -> Option<Expression> {
        let prefix = match token {
            Token::Bang => PrefixOperator::Bang,
            Token::Minus => PrefixOperator::Minus,
            _ => panic!("TODO: This should really return an option type"),
        };

        if let Some(tt) = self.lexer.next() {
            let right = self.parse_expression(Precedence::Prefix, tt)?;
            Some(Expression::Prefix(prefix, Box::new(right)))
        } else {
            self.errors.push("expected".to_string());
            None
        }
    }

    fn parse_infix_expression(&mut self, token: Token, left: Expression) -> Option<Expression> {
        let operator = match token {
            Token::Plus => InfixOperator::Plus,
            Token::Minus => InfixOperator::Minus,
            Token::Asterisk => InfixOperator::Multiply,
            Token::Slash => InfixOperator::Divide,
            Token::GT => InfixOperator::GT,
            Token::LT => InfixOperator::LT,
            Token::Equal => InfixOperator::Equals,
            Token::NotEqual => InfixOperator::NotEquals,
            _ => {
                self.errors.push(format!("incorrect operator token"));
                return None;
            }
        };

        let precedence = Parser::token_precedence(&token);
        let t = self.lexer.next()?;

        let right = self.parse_expression(precedence, t)?;

        Some(Expression::Infix(Box::new(left), operator, Box::new(right)))
    }

    fn parse_group_expression(&mut self) -> Option<Expression> {
        let t = self.lexer.next()?; 
        let exp = self.parse_expression(Precedence::Lowest, t);

        if let Some(Token::RParenthesis) = self.lexer.peek() {
            self.lexer.next();
        } else {
        }

        exp
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if !self.expect_token(Token::LParenthesis) {
            self.errors.push("expected x but got y".to_string());
            return None;
        }

        let t = self.lexer.next()?;
        let condition = self.parse_expression(Precedence::Lowest, t)?;

        if !self.expect_token(Token::RParenthesis) {
            self.errors.push("expected x but got y".to_string());
            return None;
        }
        if !self.expect_token(Token::LBrace) {
            self.errors.push("expected x but got y".to_string());
            return None;
        };

        let consequence = self.parse_block_statement();

        let mut alternative = vec![];
        if Some(&Token::Else) == self.lexer.peek() {
            self.lexer.next();

            if !self.expect_token(Token::LBrace) {
                self.errors.push("expected left brace".to_string()); // <-- improve error msg
                return None;
            }

            alternative = self.parse_block_statement();
        }

        Some(Expression::If(Box::new(condition), consequence, alternative))
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut stmts: Vec<Statement> = vec![];

        while Some(&Token::RBrace) != self.lexer.peek() {
            let token = self.lexer.next().unwrap(); // todo: remove unwrap
            let stmt = self.parse_statement(token);
            match stmt {
                Ok(s) => stmts.push(s),
                Err(e) => self.errors.push(e),
            }
        }

        self.lexer.next();
        stmts
    }

    fn parse_function(&mut self) -> Option<Expression> {
        if !self.expect_token(Token::LParenthesis) {
            self.errors.push("expected left parenthesis".to_string());
            return None;
        }

        let params = self.parse_function_parameters();
        if let Some(params) = params {
            if !self.expect_token(Token::LBrace) {
                self.errors.push("expected left parenthesis".to_string());
                return None;
            }

            let block = self.parse_block_statement();

            return Some(Expression::Function(params, block));
        }

        None
    }

    fn parse_function_parameters(&mut self) -> Option<Vec<String>> {
        let mut parameters = vec![];

        if self.expect_token(Token::RParenthesis) {
            return Some(parameters);
        }

        let t = self.lexer.next();
        match t {
            Some(Token::Identifier(x)) => parameters.push(x),
            _ => (),
        }

        while Some(&Token::Comma) == self.lexer.peek() {
            self.lexer.next();
            let t = self.lexer.next();
            match t {
                Some(Token::Identifier(x)) => parameters.push(x),
                _ => (),
            }
        }

        if !self.expect_token(Token::RParenthesis) {
            self.errors
                .push("expected to find right parenthesis".to_string());
            return None;
        }

        Some(parameters)
    }

    fn expect_token(&mut self, t: Token) -> bool {
        if Some(&t) == self.lexer.peek() {
            self.lexer.next();
            return true;
        }

        false
    }

    fn token_precedence(token: &Token) -> Precedence {
        match token {
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::LT | Token::GT => Precedence::LessGreater,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LParenthesis => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn parse_call_expression(&mut self, left: Expression) -> Option<Expression> {
        let mut args: Vec<Expression> = vec![];
        if self.expect_token(Token::RParenthesis) {
            self.lexer.next();
            return Some(Expression::Call(Box::new(left), args));
        }

        while !self.expect_token(Token::RParenthesis) {
            self.expect_token(Token::Comma);

            let t = self.lexer.next()?;

            let arg = self.parse_expression(Precedence::Lowest, t)?;
            args.push(arg);
        }

        Some(Expression::Call(Box::new(left), args))
    }
}

#[derive(Debug, PartialEq, PartialOrd)]
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
    use super::*;

    macro_rules! snapshot {
        ($name:tt, $input:tt) => {
            #[test]
            fn $name() {
                let lexer = Lexer::new($input);
                let mut parser = Parser::new(lexer);

                let program = parser.parse_program();
                if parser.errors.len() > 0 {
                    panic!("error occured in parser: {:#?}", parser.errors);
                }

                insta::assert_debug_snapshot!(&program);
            }
        };
    }

    snapshot!(
        parsing_let_statemt,
        r#"
        let x = 5;
        let y = 10;
        let foo = 838383;
        let x = 1 * 2 * 3 * 4 * 5;
    "#
    );

    snapshot!(
        parsing_return_statement,
        r#"
            return 5;
            return 10;
            return add(15);
    "#
    );

    snapshot!(
        parsing_identifier_expression,
        r#"
          foobar;
    "#
    );

    snapshot!(parsing_integer_expression, "5;");

    snapshot!(
        parsing_prefix_expression,
        r#"
        !5;
        -15;
        !true;
        !false;
    "#
    );

    snapshot!(
        parsing_infix_expression,
        r#"
        5 + 5;
        5 - 5;
        5 * 5;
        5 / 5;
        5 > 5;
        5 < 5;
        5 == 5;
        5 != 5;
        true == true;
        true != false;
        false == false;
    "#
    );

    snapshot!(
        test_parsing_boolean_expression,
        r#"
        true;
        false;
    "#
    );

    snapshot!(
        test_if_expression,
        r#"
        if (x < y) { x }
    "#
    );

    snapshot!(
        test_if_else_expression,
        r#"
        if (x < y) { x } else { y }
    "#
    );

    snapshot!(
        test_function_parsing,
        r#"
        let add = fn(x,y) {
          x + y
        }
    "#
    );

    snapshot!(
        test_function_paramenter_parsing,
        r#"
        fn() {};
        fn(x) {};
        fn(x, y, z) {};
    "#
    );

    snapshot!(
        test_call_expressions,
        r#"
        add(1, 2 * 3, 4 + 5);
    "#
    );

    #[test]
    fn test_operator_precedence_parsing() {
        let pair = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) +4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("1 * 2 * 3 * 4 * 5", "((((1 * 2) * 3) * 4) * 5)"),
            ("a + add(b * c) +d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (input, result) in pair {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();

            println!("{:?}", program.statements);
            assert_eq!(program.to_string(), result.to_string());
        }
    }
}
