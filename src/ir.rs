use std::collections::{HashMap, HashSet};

use crate::{ast::*, symbol_table::SymbolTable, visitor::*};

pub enum IRInstruction {
    Binary {
        op: BinaryOperator,
        left: IRInstructionAddress,
        right: IRInstructionAddress,
    },
    Unary {
        op: UnaryOperator,
        target: IRInstructionAddress,
    },
    Copy {
        source: IRInstructionAddress,
    },
    Goto(InstructionLabel),
    IfTrueGoto {
        prediction: IRInstructionAddress,
        target: InstructionLabel,
    },
}

impl IRInstruction {
    pub fn unary(op: UnaryOperator, target: IRInstructionAddress) -> Self {
        Self::Unary { op, target }
    }
    pub fn binary(
        op: BinaryOperator,
        left: IRInstructionAddress,
        right: IRInstructionAddress,
    ) -> Self {
        Self::Binary { op, left, right }
    }
    pub fn if_true_goto(prediction: IRInstructionAddress, target: InstructionLabel) -> Self {
        Self::IfTrueGoto { prediction, target }
    }
}

pub enum IRInstructionAddress {
    Address(usize),
    Symbol(String),
    Const(), // todo
}

pub struct InstructionList {
    instructions: Vec<usize>,
    instruction_pool: Vec<IRInstruction>,
}

impl InstructionList {
    fn format_arg(&self, arg: &IRInstructionAddress) -> String {
        match arg {
            IRInstructionAddress::Address(index) => format!("t{}", index),
            IRInstructionAddress::Symbol(symbol) => format!("{}", symbol),
            IRInstructionAddress::Const() => format!("const"),
        }
    }

    fn instruction_count(&self) -> usize {
        self.instructions.len()
    }
}

impl std::fmt::Display for InstructionList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.instructions.iter().for_each(|&index| {
            let ins = &self.instruction_pool[index];
            let _ = match ins {
                IRInstruction::Binary { op, left, right } => write!(
                    f,
                    "t{} = {} {} {}\n",
                    index,
                    self.format_arg(left),
                    op,
                    self.format_arg(right)
                ),
                IRInstruction::Unary { op, target } => {
                    write!(f, "t{} = {} {}\n", index, op, self.format_arg(target))
                }
                IRInstruction::Copy { source } => {
                    write!(f, "t{} = {}\n", index, self.format_arg(source))
                }
                IRInstruction::Goto(target) => write!(f, "goto line {}\n", target),
                IRInstruction::IfTrueGoto { prediction, target } => write!(
                    f,
                    "if {} goto line {}\n",
                    self.format_arg(prediction),
                    target
                ),
            };
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

    pub fn push(&mut self, ins: IRInstruction) -> IRInstructionAddress {
        self.instruction_pool.push(ins);
        let index = self.instruction_pool.len();
        self.instructions.push(index);
        IRInstructionAddress::Address(index)
    }
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

pub struct IRGenerationError {}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct InstructionLabel {
    inner: usize,
}

impl std::fmt::Display for InstructionLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<LABEL:{}>", self.inner)
    }
}

struct FunctionCtx {
    return_label: InstructionLabel,
}

struct LoopCtx {}

pub struct IRGenerator {
    instructions: InstructionList,
    label_index: usize,
    unresolved_labels: HashSet<InstructionLabel>,
    fn_ctx: Option<FunctionCtx>,
    loop_ctx: Vec<LoopCtx>,
    label_map: HashMap<InstructionLabel, usize>,
    symbol_table: SymbolTable,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: InstructionList::new(),
            label_index: 0,
            unresolved_labels: HashSet::new(),
            label_map: HashMap::new(),
            fn_ctx: None,
            loop_ctx: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn new_label(&mut self) -> InstructionLabel {
        let ins = InstructionLabel {
            inner: self.label_index,
        };
        self.label_index += 1;
        self.unresolved_labels.insert(ins);
        ins
    }
}

impl Visitor<Expression, IRInstructionAddress, IRGenerationError> for IRGenerator {
    fn visit(&mut self, exp: &Expression) -> Result<IRInstructionAddress, IRGenerationError> {
        let r = match exp {
            Expression::UnaryOperator { op, expr } => {
                let ins = IRInstruction::unary(*op, expr.visit_by(self)?);
                self.instructions.push(ins)
            }
            Expression::BinaryOperator { left, op, right } => {
                let arg1 = left.visit_by(self)?;
                let arg2 = right.visit_by(self)?;
                let ins = IRInstruction::binary(*op, arg1, arg2);
                self.instructions.push(ins)
            }
            Expression::FunctionCall(_) => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::ItemAccess { from, to } => todo!(),
            Expression::Assign { left, right } => todo!(),
            Expression::Number {} => IRInstructionAddress::Const(),
            Expression::Bool(v) => IRInstructionAddress::Const(),
            Expression::Ident(name) => {
                IRInstructionAddress::Symbol(name.name.clone())
                // todo!()
                // self.symbol_table.search(name.name.as_str());
            }
        };
        Ok(r)
    }
}

struct InstructionBlock {
    label_id: usize,
    body: Vec<IRInstruction>,
    // termination: Option<Instruction>,
}

impl Visitor<Statement, (), IRGenerationError> for IRGenerator {
    fn visit(&mut self, stmt: &Statement) -> Result<(), IRGenerationError> {
        match stmt {
            Statement::Block(_) => {}
            Statement::Declare { ty, name, init } => {
                // self.symbol_table.declare(name, info);
            }
            Statement::Empty => {}
            Statement::Expression(_) => {}
            Statement::Return { .. } => todo!(),
            Statement::If(des) => {
                let prediction: IRInstructionAddress = des.condition.visit_by(self)?;
            }
            Statement::While(des) => {
                // let prediction_start =
                // let prediction: IRInstructionAddress = des.condition.visit_by(self)?;
                // let goto = IRInstruction::if_true_goto(prediction, target)
            }
            Statement::For(_) => todo!(),
        }
        Ok(())
    }
}
