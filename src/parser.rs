use crate::{
    ast::{
        BinaryOperator, Block, Expression, FunctionCall, FunctionDefine, Ident, If, IfElse,
        ParseError, Statement, SyntaxElement, TypeExpression, UnaryOperator, While,
    },
    ir::*,
    lexer::{Keyword, Lexer, Token},
};

use Keyword::*;

impl SyntaxElement for FunctionDefine {
    fn parse<'a>(input: &mut Lexer<'a>) -> Result<Self, ParseError<'a>> {
        input.expect(Token::Keyword(Function))?;

        let name = parse_ident(input)?;
        input.expect(Token::Paren('('))?;
        let mut arguments = Vec::new();
        if !input.skip(Token::Paren(')')) {
            loop {
                let name = parse_ident(input)?;
                let arg = TypeExpression::parse(input)?;
                arguments.push((name, arg));
                match input.next().token {
                    Token::Paren(')') => break,
                    Token::Separator(',') => (),
                    other => return Err(ParseError::Unexpected(other, "argument list separator")),
                }
            }
        };
        let return_type = if input.skip(Token::Arrow) {
            Some(TypeExpression::parse(input)?)
        } else {
            None
        };

        let body = Block::parse(input)?;
        Ok(FunctionDefine {
            name,
            arguments,
            return_type,
            body,
        })
    }
}

fn parse_ident<'a>(lexer: &mut Lexer<'a>) -> Result<Ident, ParseError<'a>> {
    let r = match lexer.next().token {
        Token::Word(name) => Ident {
            name: name.to_owned(),
        },
        _ => return Err(ParseError::Any("cant parse ident")),
    };
    Ok(r)
}

impl SyntaxElement for TypeExpression {
    fn parse<'a>(lexer: &mut Lexer<'a>) -> Result<Self, ParseError<'a>> {
        let r = match lexer.next().token {
            Token::Word(name) => TypeExpression::Named(Ident {
                name: name.to_owned(),
            }),
            _ => return Err(ParseError::Any("cant parse type_expression")),
        };
        Ok(r)
    }
}

impl SyntaxElement for Block {
    fn parse<'a>(lexer: &mut Lexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut block = Block {
            statements: Vec::new(),
        };
        lexer.expect(Token::Paren('{'))?;
        while lexer.peek().token != Token::Paren('}') {
            block.statements.push(Statement::parse(lexer)?);
        }
        lexer.expect(Token::Paren('}'))?;
        Ok(block)
    }
}

impl SyntaxElement for Statement {
    fn parse<'a>(lexer: &mut Lexer<'a>) -> Result<Self, ParseError<'a>> {
        let r = match lexer.peek().token {
            Token::Keyword(keyword) => match keyword {
                Declare(_) => parse_expression_like_statement(lexer)?,
                Return => {
                    let _ = lexer.next();
                    let value = if lexer.peek().token == Token::Separator(';') {
                        None
                    } else {
                        Some(Expression::parse(lexer)?)
                    };
                    lexer.expect(Token::Separator(';'))?;
                    Statement::Return { value }
                }
                Break => {
                    let _ = lexer.next();
                    lexer.expect(Token::Separator(';'))?;
                    Statement::Break
                }
                Continue => {
                    let _ = lexer.next();
                    lexer.expect(Token::Separator(';'))?;
                    Statement::Continue
                }
                If => {
                    let _ = lexer.next();
                    let condition = Expression::parse(lexer)?;
                    let accept = Block::parse(lexer)?;
                    let mut elses = Vec::new();

                    while lexer.peek().token == Token::Keyword(ElseIf) {
                        lexer.expect(Token::Keyword(ElseIf))?;
                        elses.push(IfElse {
                            condition: Expression::parse(lexer)?,
                            accept: Block::parse(lexer)?,
                        });
                    }

                    let reject = if lexer.skip(Token::Keyword(Else)) {
                        Some(Block::parse(lexer)?)
                    } else {
                        None
                    };

                    lexer.skip(Token::Separator(';'));
                    Statement::If(If {
                        condition,
                        accept,
                        elses,
                        reject,
                    })
                }
                For => {
                    let _ = lexer.next();
                    let init = parse_expression_like_statement(lexer)?;
                    let test = parse_expression_like_statement(lexer)?;
                    let update = Expression::parse(lexer)?;
                    let body = Block::parse(lexer)?;
                    Statement::For(crate::ast::For {
                        init: Box::new(init),
                        test: Box::new(test),
                        update,
                        body,
                    })
                }
                While => {
                    let _ = lexer.next();
                    Statement::While(While {
                        condition: Expression::parse(lexer)?,
                        body: Block::parse(lexer)?,
                    })
                }
                _ => return Err(ParseError::Any("cant parse statement")),
            },
            Token::Paren('{') => Statement::Block(Block::parse(lexer)?),
            _ => parse_expression_like_statement(lexer)?,
        };
        Ok(r)
    }
}

pub fn parse_expression_like_statement<'a>(
    lexer: &mut Lexer<'a>,
) -> Result<Statement, ParseError<'a>> {
    let r = match lexer.peek().token {
        Token::Keyword(keyword) => match keyword {
            Declare(ty) => {
                let _ = lexer.next();
                let exp = Expression::parse(lexer)?;
                if let Expression::Assign { left, right } = exp {
                    let r = Statement::Declare {
                        ty,
                        name: left,
                        init: *right,
                    };
                    lexer.expect(Token::Separator(';'))?;
                    r
                } else {
                    return Err(ParseError::Any("cant parse simple declare statement"));
                }
            }
            _ => return Err(ParseError::Any("cant parse simple statement")),
        },
        Token::Separator(';') => {
            let _ = lexer.next();
            Statement::Empty
        }
        _ => {
            let exp = Expression::parse(lexer)?;
            lexer.expect(Token::Separator(';'))?;
            Statement::Expression(exp)
        }
    };
    Ok(r)
}

// EXP

impl SyntaxElement for Expression {
    fn parse<'a>(lexer: &mut Lexer<'a>) -> Result<Self, ParseError<'a>> {
        let mut l = lexer.clone();
        if let Token::Word(_) = l.next().token {
            if let Token::Operation('=') = l.next().token {
                return parse_assignment_expression(lexer);
            }
        }

        parse_exp_with_binary_operators(lexer)
    }
}

fn parse_assignment_expression<'a>(lexer: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    parse_binary_like_right(
        lexer,
        &|tk| tk == Token::Operation('='),
        &|lexer| {
            let r = match lexer.next().token {
                Token::Word(ident) => Ok(Ident {
                    name: ident.to_owned(),
                }),
                _ => Err(ParseError::Any("assignment left should only be ident")),
            };

            lexer.clone().expect(Token::Operation('='))?;
            r
        },
        &|lexer| parse_exp_with_binary_operators(lexer),
        &|left, _, right| Expression::Assign {
            left,
            right: Box::new(right),
        },
    )
}

pub fn parse_exp_with_binary_operators<'a>(
    lexer: &mut Lexer<'a>,
) -> Result<Expression, ParseError<'a>> {
    parse_binary_op_left(
        lexer,
        |token| match token {
            Token::LogicalOperation('|') => Some(BinaryOperator::LogicalOr),
            _ => None,
        },
        // logical_and_expression
        |lexer| {
            parse_binary_op_left(
                lexer,
                |token| match token {
                    Token::LogicalOperation('&') => Some(BinaryOperator::LogicalAnd),
                    _ => None,
                },
                // inclusive_or_expression
                |lexer| {
                    parse_binary_op_left(
                        lexer,
                        |token| match token {
                            Token::Operation('|') => Some(BinaryOperator::Or),
                            _ => None,
                        },
                        // exclusive_or_expression
                        |lexer| {
                            parse_binary_op_left(
                                lexer,
                                |token| match token {
                                    Token::Operation('^') => Some(BinaryOperator::Xor),
                                    _ => None,
                                },
                                // and_expression
                                |lexer| {
                                    parse_binary_op_left(
                                        lexer,
                                        |token| match token {
                                            Token::Operation('&') => Some(BinaryOperator::And),
                                            _ => None,
                                        },
                                        |lexer| {
                                            parse_exp_with_binary_operators_no_logic_no_bit(lexer)
                                        },
                                    )
                                },
                            )
                        },
                    )
                },
            )
        },
    )
}

pub fn parse_exp_with_binary_operators_no_logic_no_bit<'a>(
    lexer: &mut Lexer<'a>,
) -> Result<Expression, ParseError<'a>> {
    // equality_expression
    parse_binary_op_left(
        lexer,
        |token| match token {
            Token::LogicalOperation('=') => Some(BinaryOperator::Equal),
            Token::LogicalOperation('!') => Some(BinaryOperator::NotEqual),
            _ => None,
        },
        // relational_expression
        |lexer| {
            parse_binary_op_left(
                lexer,
                |token| match token {
                    Token::Paren('<') => Some(BinaryOperator::Less),
                    Token::Paren('>') => Some(BinaryOperator::Greater),
                    Token::LogicalOperation('<') => Some(BinaryOperator::LessEqual),
                    Token::LogicalOperation('>') => Some(BinaryOperator::GreaterEqual),
                    _ => None,
                },
                |lexer| {
                    // additive_expression
                    parse_binary_op_left(
                        lexer,
                        |token| match token {
                            Token::Operation('+') => Some(BinaryOperator::Add),
                            Token::Operation('-') => Some(BinaryOperator::Sub),
                            _ => None,
                        },
                        // multiplicative_expression
                        |lexer| {
                            parse_binary_op_left(
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
                },
            )
        },
    )
}

// EXP_WITH_POSTFIX
pub fn parse_exp_with_postfix<'a>(input: &mut Lexer<'a>) -> Result<Expression, ParseError<'a>> {
    let mut result = parse_single_expression(input)?;
    loop {
        result = match input.peek().token {
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
                match input.next().token {
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
    let r = match input.next().token {
        Token::Number { .. } => Expression::PrimitiveConst(PrimitiveConstValue::Numeric(
            NumericTypeConstValue::Float(1.), // todo
        )),
        Token::Bool(v) => Expression::PrimitiveConst(PrimitiveConstValue::Bool(v)),
        Token::Operation('-') => {
            let inner = Expression::parse(input)?;
            let inner = Box::new(inner);
            Expression::UnaryOperator {
                op: UnaryOperator::Neg,
                expr: inner,
            }
        }
        Token::Operation('!') => {
            let inner = Expression::parse(input)?;
            let inner = Box::new(inner);
            Expression::UnaryOperator {
                op: UnaryOperator::Not,
                expr: inner,
            }
        }
        Token::Paren('(') => {
            let inner = Expression::parse(input)?;
            input.expect(Token::Paren(')'))?;
            inner
        }
        Token::Word(name) => {
            if let Token::Paren('(') = input.peek().token {
                Expression::FunctionCall(parse_function_parameters(input, name)?)
            } else {
                Expression::Ident(Ident {
                    name: name.to_owned(),
                })
            }
        }
        _ => panic!(),
        // _ => return Err(ParseError::Any("failed in parse single expression")),
    };
    Ok(r)
}

fn parse_binary_op_left<'a>(
    lexer: &mut Lexer<'a>,
    separator: impl Fn(Token<'a>) -> Option<BinaryOperator>,
    parser: impl Fn(&mut Lexer<'a>) -> Result<Expression, ParseError<'a>>,
) -> Result<Expression, ParseError<'a>> {
    parse_binary_like_left(
        lexer,
        |tk| separator(tk).is_some(),
        &parser,
        &parser,
        |left, tk, right| Expression::BinaryOperator {
            op: separator(tk).unwrap(), // this unwrap is safe
            left: Box::new(left),
            right: Box::new(right),
        },
    )
}

fn parse_binary_like_left<'a, L, R>(
    lexer: &mut Lexer<'a>,
    separator: impl Fn(Token<'a>) -> bool,
    left_parser: &impl Fn(&mut Lexer<'a>) -> Result<L, ParseError<'a>>,
    right_parser: &impl Fn(&mut Lexer<'a>) -> Result<R, ParseError<'a>>,
    assemble: impl Fn(L, Token<'a>, R) -> L,
) -> Result<L, ParseError<'a>> {
    let mut result = left_parser(lexer)?;
    while separator(lexer.peek().token) {
        let token = lexer.next().token;
        let right = right_parser(lexer)?;
        result = assemble(result, token, right);
    }
    Ok(result)
}

fn parse_binary_like_right<'a, L, R>(
    lexer: &mut Lexer<'a>,
    separator: &impl Fn(Token<'a>) -> bool,
    left_parser: &impl Fn(&mut Lexer<'a>) -> Result<L, ParseError<'a>>,
    right_parser: &impl Fn(&mut Lexer<'a>) -> Result<R, ParseError<'a>>,
    assemble: &impl Fn(L, Token<'a>, R) -> R,
) -> Result<R, ParseError<'a>> {
    let mut backup = lexer.clone();
    let left = left_parser(lexer);
    if let Ok(left) = left {
        while separator(lexer.peek().token) {
            let token = lexer.next().token;
            let right =
                parse_binary_like_right(lexer, separator, left_parser, right_parser, assemble)?;
            return Ok(assemble(left, token, right));
        }
        right_parser(lexer)
    } else {
        let r = right_parser(&mut backup);
        *lexer = backup;
        r
    }
}

pub fn parse_function_parameters<'a>(
    input: &mut Lexer<'a>,
    name: &'a str,
) -> Result<FunctionCall, ParseError<'a>> {
    input.expect(Token::Paren('('))?;
    let mut arguments = Vec::new();
    // if skipped means empty argument
    if !input.skip(Token::Paren(')')) {
        loop {
            let arg = Expression::parse(input)?;
            arguments.push(arg);
            match input.next().token {
                Token::Paren(')') => break,
                Token::Separator(',') => (),
                other => return Err(ParseError::Unexpected(other, "argument list separator")),
            }
        }
    }
    Ok(FunctionCall {
        name: name.to_owned(),
        arguments,
    })
}
