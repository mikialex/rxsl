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
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug)]
pub struct If {
    pub condition: Expression,
    pub accept: Block,
    pub elses: Vec<IfElse>,
    pub reject: Option<Block>,
}

#[derive(Debug)]
pub struct IfElse {
    pub condition: Expression,
    pub accept: Block,
}

#[derive(Debug)]
pub struct While {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Debug)]
pub enum Statement {
    Block(Block),
    Empty,
    Expression(Expression),
    Return { value: Option<Expression> },
    If(If),
    While(While),
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
    ArrayAccess {
        array: Box<Self>,
        index: Box<Self>,
    },
    ItemAccess {
        from: Box<Self>,
        to: Ident,
    },
    Assign {
        left: Ident,
        right: Box<Self>,
    },
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
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
}
