pub enum ASTNode {
    Expression,
}

pub enum Expression {
    UnaryOperator {
        op: UnaryOperator,
        expr: Box<Self>,
    },
    BinaryOperator {
        op: BinaryOperator,
        left: Box<Self>,
        right: Box<Self>,
    },
    FunctionCall {
        fn_name: String,
        parameters: Vec<Expression>,
    },
}

pub enum UnaryOperator {
    Neg,
}

pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
}

pub enum Keyword {
    If,
    Else,
    ElseIf,
    For,
}
