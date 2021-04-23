use crate::{
    ast::*,
    control_flow_graph::BasicBlockHandle,
    utils::storage::{Handle, Storage, VecStorage},
    ConstHandle, ConstPool,
};

pub struct IR {
    pub const_pool: ConstPool,
    pub functions: Storage<InstructionList, VecStorage>,
    pub entry: Handle<InstructionList, VecStorage>,
}

pub enum IRInstruction {
    Binary {
        op: BinaryOperator,
        left: Address,
        right: Address,
    },
    Unary {
        op: UnaryOperator,
        target: Address,
    },
    Copy {
        source: Address,
        target: Address,
    },
    Jump(JumpInstruction),
}

pub enum JumpInstruction {
    Goto(JumpAddress),
    IfTrueGoto {
        prediction: Address,
        target: JumpAddress,
    },
}

pub fn unknown_goto() -> JumpInstruction {
    JumpInstruction::Goto(JumpAddress::Unknown)
}
pub fn unknown_goto_if_true(prediction: Address) -> JumpInstruction {
    JumpInstruction::IfTrueGoto {
        prediction,
        target: JumpAddress::Unknown,
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum JumpAddress {
    Unknown,
    BasicBlock(BasicBlockHandle),
    Termination,
}

impl std::fmt::Display for JumpAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpAddress::Unknown => write!(f, "UNKNOWN"),
            JumpAddress::BasicBlock(bb) => write!(f, "{}", bb),
            JumpAddress::Termination => write!(f, "TERMINATION"),
        }
    }
}

impl IRInstruction {
    pub fn unary(op: UnaryOperator, target: Address) -> Self {
        Self::Unary { op, target }
    }
    pub fn binary(op: BinaryOperator, left: Address, right: Address) -> Self {
        Self::Binary { op, left, right }
    }
}

#[derive(Clone)]
pub enum Address {
    Address(usize),
    Symbol(String),
    Const(ConstHandle), // todo
}

pub struct InstructionList {
    instructions: Vec<usize>,
    instruction_pool: Vec<IRInstruction>,
}

impl InstructionList {
    fn format_arg(&self, arg: &Address) -> String {
        match arg {
            Address::Address(index) => format!("t{}", index),
            Address::Symbol(name) => format!("symbol({})", name),
            Address::Const(_) => format!("const"),
        }
    }

    pub fn next_inst_line(&self) -> usize {
        self.instructions.len()
    }

    pub fn mut_instruction_by_line(&mut self, line: usize) -> &mut IRInstruction {
        let index = self.instructions[line];
        &mut self.instruction_pool[index]
    }
}

impl std::fmt::Display for InstructionList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.instructions
            .iter()
            .enumerate()
            .for_each(|(line, &index)| {
                write!(f, "{} | ", line).unwrap();
                let ins = &self.instruction_pool[index];
                let _ = match ins {
                    IRInstruction::Binary { op, left, right } => write!(
                        f,
                        "t{} = {} {} {}",
                        index,
                        self.format_arg(left),
                        op,
                        self.format_arg(right)
                    ),
                    IRInstruction::Unary { op, target } => {
                        write!(f, "t{} = {} {}", index, op, self.format_arg(target))
                    }
                    IRInstruction::Copy { source, target } => write!(
                        f,
                        "{} = copy {}",
                        self.format_arg(target),
                        self.format_arg(source)
                    ),
                    IRInstruction::Jump(jump) => match jump {
                        JumpInstruction::Goto(target) => write!(f, "goto line {}", target),
                        JumpInstruction::IfTrueGoto { prediction, target } => {
                            write!(f, "if {} goto line {}", self.format_arg(prediction), target)
                        }
                    },
                };

                write!(f, "\n",).unwrap();
            });
        Ok(())
    }
}

impl InstructionList {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            instruction_pool: Vec::new(),
        }
    }

    pub fn push(&mut self, ins: IRInstruction) -> usize {
        let index = self.instruction_pool.len();
        self.instruction_pool.push(ins);
        self.instructions.push(index);
        index
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum NumericType {
    Float,
    Int,
    UnsignedInt,
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Numeric(NumericType),
    Bool,
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum PrimitiveConstValue {
    Bool(bool),
    Numeric(NumericTypeConstValue),
}

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum NumericTypeConstValue {
    Float(f32),
    Int(i32),
    UnsignedInt(u32),
}
