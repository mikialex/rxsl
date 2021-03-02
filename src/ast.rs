use crate::lexer::TokenSpan;

pub enum ASTNode {
    Expression,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    Any(&'static str),
    Unexpected(TokenSpan<'a>, &'a str),
}

#[derive(Debug)]
pub enum Expression {
    UnaryOperator {
        op: UnaryOperator,
        expr: Box<Self>,
    },
    BinaryOperator {
        left: Box<Self>,
        op: BinaryOperator,
        right: Box<Self>,
    },
    FunctionCall(FunctionCall),
    Number {},
    Bool(bool),
    Ident(Ident),
}

#[derive(Debug)]
pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug)]
pub struct Ident {
    pub name: String,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Neg,
    Not,
}

#[derive(Debug)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum Keyword {
    If,
    Else,
    ElseIf,
    For,
}
