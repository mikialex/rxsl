use crate::{
    ast::{
        BinaryOperator, Block, Expression, FunctionCall, Ident, If, IfElse, ParseError, Statement,
        UnaryOperator,
    },
    lexer::{Lexer, Token},
};

pub fn parse_block<'a>(lexer: &mut Lexer<'a>) -> Result<Block, ParseError<'a>> {
    let mut block = Block {
        statements: Vec::new(),
    };
    lexer.expect(Token::Paren('{'))?;
    while lexer.peek().0 != Token::Paren('}') {
        block.statements.push(parse_statement(lexer)?);
    }
    lexer.expect(Token::Paren('}'))?;
    Ok(block)
}

pub fn parse_statement<'a>(lexer: &mut Lexer<'a>) -> Result<Statement, ParseError<'a>> {
    let r = match lexer.next().0 {
        Token::Word("return") => {
            let value = if lexer.peek().0 == Token::Separator(';') {
                None
            } else {
                Some(parse_expression(lexer)?)
            };
            lexer.expect(Token::Separator(';'))?;
            Statement::Return { value }
        }
        Token::Word("if") => {
            let condition = parse_expression(lexer)?;
            let accept = parse_block(lexer)?;
            let mut elses = Vec::new();

            while lexer.peek().0 == Token::Word("elseif") {
                lexer.expect(Token::Word("elseif"))?;
                elses.push(IfElse {
                    condition: parse_expression(lexer)?,
                    accept: parse_block(lexer)?,
                });
            }

            let reject = if lexer.if_skip(Token::Word("else")) {
                Some(parse_block(lexer)?)
            } else {
                None
            };
            lexer.if_skip(Token::Separator(';'));
            Statement::If(If {
                condition,
                accept,
                elses,
                reject,
            })
        }
        _ => return Err(ParseError::Any("cant parse statement")),
    };
    Ok(r)
}

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
                |lexer| parse_exp_with_postfix(lexer),
            )
        },
    )
}

// EXP_WITH_POSTFIX
pub fn parse_exp_with_postfix<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    let mut result = parse_single_expression(input)?;
    loop {
        result = match input.peek().0 {
            Token::Paren('[') => {
                let _ = input.next();
                let index = parse_single_expression(input)?;
                input.expect(Token::Paren(']'))?;
                Expression::ArrayAccess {
                    array: Box::new(result),
                    index: Box::new(index),
                }
            }
            Token::Separator('.') => {
                let _ = input.next();
                match input.next().0 {
                    Token::Word(ident) => Expression::ItemAccess {
                        from: Box::new(result),
                        to: Ident {
                            name: ident.to_owned(),
                        },
                    },
                    _ => return Err(ParseError::Any("only ident can dot with")),
                }
            }
            _ => break,
        };
    }

    Ok(result)
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
        Token::Word(name) => {
            if let Token::Paren('(') = input.peek().0 {
                Expression::FunctionCall(parse_function_parameters(input, name)?)
            } else {
                Expression::Ident(Ident {
                    name: name.to_owned(),
                })
            }
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

pub fn parse_function_parameters<'a>(
    input: &mut Lexer<'a>,
    name: &'a str,
) -> Result<FunctionCall, ParseError<'a>> {
    input.expect(Token::Paren('('))?;
    let mut arguments = Vec::new();
    // if skipped means empty argument
    if !input.if_skip(Token::Paren(')')) {
        loop {
            let arg = parse_expression(input)?;
            arguments.push(arg);
            match input.next() {
                (Token::Paren(')'), _) => break,
                (Token::Separator(','), _) => (),
                other => return Err(ParseError::Unexpected(other, "argument list separator")),
            }
        }
    }
    Ok(FunctionCall {
        name: name.to_owned(),
        arguments,
    })
}
