use std::todo;

use crate::{
    ast::{BinaryOperator, Expression, FunctionCall, Ident, ParseError, UnaryOperator},
    lexer::{Lexer, Token},
};

// EXP
// => EXP * EXP
// => EXP / EXP
// => EXP + EXP
// => EXP - EXP
// => EXP % EXP
// => - EXP
// => ( EXP )
// => number
// => bool

// ============
// after disambiguity and remove left recursion
// ============

// EXP
// => EXP_NO_ADDICTIVE ADDICTIVE_OP? EXP_NO_ADDICTIVE?

// ADDICTIVE_OP
//  => + -

// EXP_NO_ADDICTIVE
// => EXP_SINGLE MULTIPLICATIVE_OP? EXP_SINGLE?

// MULTIPLICATIVE_OP
// => * / %

// EXP_SINGLE
// => ( EXP )
// => - EXP
// => number
// => bool

// EXP
pub fn parse_expression<'a>(lexer: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    // additive_expression
    parse_binary_op(
        lexer,
        |token| match token {
            Token::Operation('+') => Some(BinaryOperator::Add),
            Token::Operation('-') => Some(BinaryOperator::Sub),
            _ => None,
        },
        // multiplicative_expression
        |lexer| {
            parse_binary_op(
                lexer,
                |token| match token {
                    Token::Operation('*') => Some(BinaryOperator::Mul),
                    Token::Operation('/') => Some(BinaryOperator::Div),
                    Token::Operation('%') => Some(BinaryOperator::Mod),
                    _ => None,
                },
                |lexer| parse_single_expression(lexer),
            )
        },
    )
}

// EXP_SINGLE
pub fn parse_single_expression<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    let r = match input.next().0 {
        Token::Number { .. } => Expression::Number {},
        Token::Bool(v) => Expression::Bool(v),
        Token::Operation('-') => {
            let inner = parse_expression(input)?;
            let inner = Box::new(inner);
            Expression::UnaryOperator {
                op: UnaryOperator::Neg,
                expr: inner,
            }
        }
        Token::Operation('!') => {
            let inner = parse_expression(input)?;
            let inner = Box::new(inner);
            Expression::UnaryOperator {
                op: UnaryOperator::Not,
                expr: inner,
            }
        }
        Token::Paren('(') => {
            let inner = parse_expression(input)?;
            input.expect(Token::Paren(')'))?;
            inner
        }
        _ => return Err(ParseError::Any("failed in parse single expression")),
    };
    Ok(r)
}

fn parse_binary_op<'a>(
    lexer: &mut Lexer<'a>,
    separator: impl Fn(Token<'a>) -> Option<BinaryOperator>,
    mut parser: impl FnMut(&mut Lexer<'a>) -> Result<Expression, ParseError<'a>>,
) -> Result<Expression, ParseError<'a>> {
    let mut left = parser(lexer)?;
    while let Some(op) = separator(lexer.peek().0) {
        let _ = lexer.next();
        let expression = Expression::BinaryOperator {
            op,
            left: Box::new(left),
            right: Box::new(parser(lexer)?),
        };
        left = expression;
    }
    Ok(left)
}

// pub fn parse_function_call<'a>(input: &mut Lexer<'a>) -> Result<FunctionCall, ParseError<'a>> {
//     let r = match input.next().0 {
//         Token::Word(name) => {
//             input.expect(Token::Separator('('))?;
//             let mut arguments = Vec::new();
//             // if skipped means empty argument
//             if !input.if_skip(Token::Paren(')')) {
//                 loop {
//                     let arg = parse_expression(input)?;
//                     arguments.push(arg);
//                     match input.next() {
//                         (Token::Paren(')'), _) => break,
//                         (Token::Separator(','), _) => (),
//                         other => {
//                             return Err(ParseError::Unexpected(other, "argument list separator"))
//                         }
//                     }
//                 }
//             }
//             FunctionCall {
//                 name: name.to_owned(),
//                 arguments,
//             }
//         }
//         _ => return Err(ParseError::Any("function call")),
//     };
//     Ok(r)
// }
