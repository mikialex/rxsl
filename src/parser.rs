use std::todo;

use crate::{
    ast::{BinaryOperator, Expression, FunctionCall, Ident, ParseError},
    lexer::{Lexer, Token},
};

pub fn parse_expression<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    match input.next() {
        _ => todo!(),
    }
}

pub fn parse_binary_expression<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    //
    todo!()
}

pub fn parse_bool<'a>(input: &mut Lexer<'a>) -> Result<bool, ParseError<'a>> {
    let r = match input.next().0 {
        Token::Word("true") => true,
        Token::Word("false") => true,
        _ => return Err(ParseError::Any("bool")),
    };
    Ok(r)
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

// fn parse_binary_op<'a>(
//     lexer: &mut Lexer<'a>,
//     classifier: impl Fn(Token<'a>) -> Option<BinaryOperator>,
//     mut parser: impl FnMut(&mut Lexer<'a>) -> Result<Expression, ParseError<'a>>,
// ) -> Result<Expression, ParseError<'a>> {
//     let mut left = parser(lexer)?;
//     while let Some(op) = classifier(lexer.peek().0) {
//         let _ = lexer.next();
//         let expression = crate::Expression::Binary {
//             op,
//             left,
//             right: parser(lexer, self.reborrow())?,
//         };
//         left = self.expressions.append(expression);
//     }
//     Ok(left)
// }

// EXP
// => EXP * EXP
// => EXP / EXP
// => EXP + EXP
// => EXP - EXP
// => - EXP
// => ( EXP )
// => number
// => bool

// ============

// EXP
// => EXP_WITHOUT_ARITHMETIC_OP ARITHMETIC_OP? EXP_WITHOUT_ARITHMETIC_OP

// ARITHMETIC_OP
//  => * / + -

// EXP_WITHOUT_ARITHMETIC_OP
// => ( EXP )
// => - EXP
// => number
// => bool

// EXP_WITHOUT_ARITHMETIC_OP
pub fn parse_single_expression<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    let r = match input.next().0 {
        Token::Number { .. } => Expression::Number {},
        Token::Operation('-') => parse_expression(input)?,
        _ => return Err(ParseError::Any("failed in parse single expression")),
    };
    Ok(r)
}
