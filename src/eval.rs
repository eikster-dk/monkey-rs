use std::fmt;

use crate::ast::{BlockStatement, Expression, InfixOperator, PrefixOperator, Program, Statement};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(isize),
    Bool(bool),
    Return(Box<Object>),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Object::Integer(i) => i.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Return(value) => value.to_string(),
            Object::Null => "null".to_string(),
        };
        write!(f, "{}", str)
    }
}

pub fn evaluate_program(program: Program) -> Object {
    evaluate_statements(&program.statements)
}

fn evaluate_statements(statements: &Vec<Statement>) -> Object {
    let mut result = Object::Null;
    for stmt in statements {
        result = evaluate_statement(stmt);

        match result {
            Object::Return(result) => return *result,
            _ => (),
        }
    }

    result
}

fn evaluate_block_statement(statements: &Vec<Statement>) -> Object {
    let mut result = Object::Null;
    for stmt in statements {
        result = evaluate_statement(stmt);

        match result {
            Object::Return(_) => return result,
            _ => (),
        }
    }

    result
}

fn evaluate_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::Let(_, _) => Object::Null,
        Statement::Return(expr) => Object::Return(Box::new(evaluate_expression(expr))),
        Statement::Expression(expr) => evaluate_expression(expr),
    }
}

fn evaluate_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Identifier(_) => Object::Null,
        Expression::Integer(i) => Object::Integer(i.clone()),
        Expression::Bool(b) => Object::Bool(b.clone()),
        Expression::Prefix(prefix, expr) => {
            evaluate_prefix_expression(prefix, evaluate_expression(&expr))
        }
        Expression::Infix(left, operator, right) => evaluate_infix_operator(
            evaluate_expression(left),
            operator,
            evaluate_expression(right),
        ),
        Expression::If(condition, consequence, alternative) => {
            evaluate_conditionals(evaluate_expression(condition), consequence, alternative)
        }
        Expression::Function(_, _) => Object::Null,
        Expression::Call(_, _) => Object::Null,
    }
}

fn evaluate_prefix_expression(operator: &PrefixOperator, right: Object) -> Object {
    match operator {
        PrefixOperator::Minus => match right {
            Object::Integer(i) => Object::Integer(i * -1),
            _ => Object::Null,
        },
        PrefixOperator::Bang => match right {
            Object::Bool(b) => Object::Bool(!b),
            Object::Null => Object::Bool(true),
            _ => Object::Bool(false),
        },
    }
}

fn evaluate_infix_operator(left: Object, operator: &InfixOperator, right: Object) -> Object {
    match (left, right) {
        (Object::Integer(left), Object::Integer(right)) => match operator {
            InfixOperator::Plus => Object::Integer(left + right),
            InfixOperator::Minus => Object::Integer(left - right),
            InfixOperator::Multiply => Object::Integer(left * right),
            InfixOperator::Divide => Object::Integer(left / right),
            InfixOperator::LT => Object::Bool(left < right),
            InfixOperator::GT => Object::Bool(left > right),
            InfixOperator::Equals => Object::Bool(left == right),
            InfixOperator::NotEquals => Object::Bool(left != right),
        },
        (Object::Bool(left), Object::Bool(right)) => match operator {
            InfixOperator::Equals => Object::Bool(left == right),
            InfixOperator::NotEquals => Object::Bool(left != right),
            _ => Object::Null,
        },
        (_, _) => Object::Null,
    }
}

fn evaluate_conditionals(
    condition: Object,
    consequence: &BlockStatement,
    alternative: &BlockStatement,
) -> Object {
    println!("hello");
    if is_truthy(condition) {
        return evaluate_block_statement(consequence);
    }

    if !alternative.is_empty() {
        return evaluate_block_statement(alternative);
    }

    Object::Null
}

#[inline]
fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Bool(b) => b,
        Object::Null => false,
        _ => true,
    }
}

#[cfg(test)]
mod tests {
    use crate::{lexer::Lexer, parser::Parser};

    use super::*;

    macro_rules! snapshot {
        ($name:tt, $input:tt) => {
            #[test]
            fn $name() {
                let result: Vec<Object> = include_str!($input)
                    .split("\n")
                    .into_iter()
                    .filter(|stmt| stmt != &"")
                    .map(|stmt| {
                        let lexer = Lexer::new(stmt);
                        let mut parser = Parser::new(lexer);

                        let program = parser.parse_program().unwrap();
                        evaluate_program(program)
                    })
                    .collect();

                let mut settings = insta::Settings::clone_current();
                settings.set_snapshot_path("./testdata/output/eval");
                settings.bind(|| {
                    insta::assert_debug_snapshot!(&result);
                });
            }
        };
    }

    snapshot!(
        evaluting_integer_expressions,
        "./testdata/eval-integers.monkey"
    );
    snapshot!(
        evaluting_boolean_expressions,
        "./testdata/eval-booleans.monkey"
    );
    snapshot!(
        evaluting_bang_operators,
        "./testdata/eval-bang-operators.monkey"
    );
    snapshot!(
        evaluting_conditionals,
        "./testdata/eval-conditionals.monkey"
    );
    snapshot!(evaluting_returns, "./testdata/eval-return.monkey");
}
