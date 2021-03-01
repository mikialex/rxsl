use crate::lexer::TokenSpan;

pub enum ASTNode {
    Expression,
}

#[derive(Debug)]
pub enum ParseError<'a> {
    Any(&'static str),
    Unexpected(TokenSpan<'a>, &'a str),
}

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

pub struct FunctionCall {
    pub name: String,
    pub arguments: Vec<Expression>,
}

pub struct Ident {
    pub name: String,
}

pub enum UnaryOperator {
    Neg,
    Not,
}

pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

pub enum Keyword {
    If,
    Else,
    ElseIf,
    For,
}
