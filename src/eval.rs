use std::{fmt, vec};

use crate::ast::{Expression, InfixOperator, PrefixOperator, Program, Statement, BlockStatement};

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(isize),
    Bool(bool),
    Null,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Object::Integer(i) => i.to_string(),
            Object::Bool(b) => b.to_string(),
            Object::Null => "null".to_string(),
        };
        write!(f, "{}", str)
    }
}

pub fn evaluate_program(program: Program) -> Vec<Object> {
    evaluate_statements(&program.statements)
}

fn evaluate_statements(statements: &Vec<Statement>) -> Vec<Object> {
    statements.iter().map(|stmt| evaluate_statement(stmt)).collect()
}
 
fn evaluate_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::Let(_, _) => Object::Null,
        Statement::Return(_) => Object::Null,
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
        Expression::If(condition, consequence, alternative) => evaluate_conditionals(evaluate_expression(condition), consequence, alternative),
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
        (Object::Bool(left), Object::Bool(right)) => {
            match operator {
                InfixOperator::Equals => Object::Bool(left == right),
                InfixOperator::NotEquals => Object::Bool(left != right),
                _ => Object::Null,
            }
        }
        (_, _) => Object::Null,
    }
}

fn evaluate_conditionals(condition: Object, consequence: &BlockStatement, alternative: &BlockStatement) -> Object {
    // todo: this conditional evaluator is not complete and still requires work
    // but this is the initial implementation
    if isTruthy(condition) {
        return evaluate_statements(consequence).last().unwrap().clone();
    }

    if !alternative.is_empty() {
        return evaluate_statements(alternative).last().unwrap().clone();
    }

    Object::Null
}

#[inline]
fn isTruthy(obj:Object) -> bool {
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
                let str = include_str!($input);
                let lexer = Lexer::new(str);
                let mut parser = Parser::new(lexer);

                let program = parser.parse_program().unwrap();
                let result = evaluate_program(program);

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
}
