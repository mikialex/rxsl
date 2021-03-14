use std::collections::HashMap;

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
    Goto(JumpAddress),
    IfTrueGoto {
        prediction: IRInstructionAddress,
        target: JumpAddress,
    },
}

pub enum JumpAddress {
    Unknown,
    Line(usize),
}

impl std::fmt::Display for JumpAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpAddress::Unknown => write!(f, "UNKNOWN"),
            JumpAddress::Line(line) => write!(f, "{}", line),
        }
    }
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
    pub fn if_true_goto(prediction: IRInstructionAddress, target: JumpAddress) -> Self {
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

pub struct InstructionLabelManager {
    latest_label_index: usize,
    /// map the unresolved label to instructions that referenced
    unresolved_labels: HashMap<InstructionLabel, Vec<usize>>,
}

impl InstructionLabelManager {
    pub fn new() -> Self {
        Self {
            latest_label_index: 0,
            unresolved_labels: HashMap::new(),
        }
    }

    pub fn new_label(&mut self, ins_line: usize) -> InstructionLabel {
        let ins = InstructionLabel {
            inner: self.latest_label_index,
        };
        self.latest_label_index += 1;
        self.unresolved_labels.insert(ins, vec![ins_line]);
        ins
    }

    pub fn back_patch(&mut self, label: InstructionLabel, ins: usize) {
        todo!()
    }

    pub fn merge(
        &mut self,
        one: InstructionLabel,
        the_other: InstructionLabel,
    ) -> InstructionLabel {
        todo!()
    }
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
    labels: InstructionLabelManager,
    fn_ctx: Option<FunctionCtx>,
    loop_ctx: Vec<LoopCtx>,
    symbol_table: SymbolTable,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: InstructionList::new(),
            labels: InstructionLabelManager::new(),
            fn_ctx: None,
            loop_ctx: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }
    pub fn new_label_next_inst(&mut self) -> InstructionLabel {
        self.labels.new_label(self.instructions.instructions.len())
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

struct BlockInstJump {
    ins_begin: usize,
    next: InstructionLabel,
}

// impl Visitor<Block, BlockInstJump, IRGenerationError> for IRGenerator {
//     fn visit(&mut self, b: &Block) -> Result<BlockInstJump, IRGenerationError> {
//         todo!()
//     }
// }

struct BooleanInstJump {
    ins_begin: usize,
    true_tag: InstructionLabel,
    false_tag: InstructionLabel,
}

impl IRGenerator {
    fn code_gen(
        &mut self,
        stmt: &Statement,
        next: InstructionLabel,
    ) -> Result<BlockInstJump, IRGenerationError> {
        let re = match stmt {
            Statement::Block(b) => b.visit_by(self)?,
            Statement::Declare { ty, name, init } => {
                // self.symbol_table.declare(name, info);
            }
            Statement::Empty => {}
            Statement::Expression(_) => {}
            Statement::Return { .. } => todo!(),
            Statement::If(des) => {
                let prediction: IRInstructionAddress = des.condition.visit_by(self)?;
                todo!()
            }
            Statement::While(des) => {
                let prediction: BooleanInstJump = des.condition.visit_by(self)?;
                let loop_body: BlockInstJump = des.body.visit_by(self)?;
                self.labels.back_patch(loop_body.next, prediction.ins_begin);
                self.labels
                    .back_patch(prediction.true_tag, loop_body.ins_begin);
                self.instructions
                    .push(IRInstruction::Goto(JumpAddress::Line(prediction.ins_begin)));
                BlockInstJump {
                    ins_begin: prediction.ins_begin,
                    next: prediction.false_tag,
                }
            }
            Statement::For(_) => todo!(),
        };
        Ok(re)
    }
}
