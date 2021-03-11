pub struct TripleCodeInstruction {
    op: Operation,
    arg1: TripleCodeArgument,
    arg2: TripleCodeArgument,
}

// impl std::fmt::Display for TripleCodeInstruction {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{}", self.0)
//     }
// }

pub enum TripleCodeArgument {
    Reference(usize),
    Symbol(String),
}

pub struct InstructionList {
    instructions: Vec<usize>,
    instruction_poll: Vec<TripleCodeInstruction>,
}

pub enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Copy,
}

pub enum ScalarValueType {
    Float,
    Int,
    UnsignedInt,
}

pub struct ScalarType {
    ty: ScalarValueType,
    size: usize,
}

pub enum PrimitiveType {
    Scalar(ScalarType),
    Bool,
}
