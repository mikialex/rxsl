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
pub struct JumpUnresolved {
    inner: usize,
}

pub struct InstructionJumpResolver {
    latest_item_index: usize,
    /// map the unresolved item to instructions that referenced
    unresolved_items: HashMap<JumpUnresolved, Vec<usize>>,
}

impl InstructionJumpResolver {
    pub fn new() -> Self {
        Self {
            latest_item_index: 0,
            unresolved_items: HashMap::new(),
        }
    }

    pub fn new_by_line(&mut self, ins_line: usize) -> JumpUnresolved {
        let ins = JumpUnresolved {
            inner: self.latest_item_index,
        };
        self.latest_item_index += 1;
        self.unresolved_items.insert(ins, vec![ins_line]);
        ins
    }

    pub fn back_patch(&mut self, item: JumpUnresolved, ins: usize) {
        todo!()
    }

    pub fn merge(&mut self, one: JumpUnresolved, the_other: JumpUnresolved) -> JumpUnresolved {
        todo!()
    }
}

struct FunctionCtx {
    return_item: JumpUnresolved,
}

struct LoopCtx {}

pub struct IRGenerator {
    instructions: InstructionList,
    jump_resolver: InstructionJumpResolver,
    fn_ctx: Option<FunctionCtx>,
    loop_ctx: Vec<LoopCtx>,
    symbol_table: SymbolTable,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: InstructionList::new(),
            jump_resolver: InstructionJumpResolver::new(),
            fn_ctx: None,
            loop_ctx: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }
    pub fn new_item_next_inst(&mut self) -> JumpUnresolved {
        self.jump_resolver.new_by_line(self.next_inst_line())
    }
    pub fn next_inst_line(&self) -> usize {
        self.instructions.instructions.len()
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
    next: Option<JumpUnresolved>,
}

impl Visitor<Block, BlockInstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, b: &Block) -> Result<BlockInstJump, IRGenerationError> {
        let ins_begin = self.next_inst_line();
        let mut last_next: Option<JumpUnresolved>;
        for s in b.statements {
            let jump: BlockInstJump = s.visit_by(self)?;
            if let Some(last_next) = last_next {
                self.jump_resolver.back_patch(last_next, jump.ins_begin)
            }
            last_next = jump.next;
        }
        Ok(BlockInstJump {
            ins_begin,
            next: last_next,
        })
    }
}

struct BooleanInstJump {
    ins_begin: usize,
    true_tag: JumpUnresolved,
    false_tag: JumpUnresolved,
}

impl Visitor<Statement, BlockInstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, stmt: &Statement) -> Result<BlockInstJump, IRGenerationError> {
        let re = match stmt {
            Statement::Block(b) => b.visit_by(self)?,
            Statement::Declare { ty, name, init } => {
                let ins_begin = self.next_inst_line();
                self.symbol_table.declare(name.name.as_str(), todo!());
                BlockInstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Empty => {
                let ins_begin = self.next_inst_line();
                BlockInstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Expression(_) => {
                let ins_begin = self.next_inst_line();
                BlockInstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Return { .. } => todo!(),
            Statement::If(des) => {
                let prediction: IRInstructionAddress = des.condition.visit_by(self)?;
                todo!()
            }
            Statement::While(des) => {
                let prediction: BooleanInstJump = des.condition.visit_by(self)?;
                let loop_body: BlockInstJump = des.body.visit_by(self)?;
                if let Some(loop_body_next) = loop_body.next {
                    self.jump_resolver
                        .back_patch(loop_body_next, prediction.ins_begin);
                }
                self.jump_resolver
                    .back_patch(prediction.true_tag, loop_body.ins_begin);
                self.instructions
                    .push(IRInstruction::Goto(JumpAddress::Line(prediction.ins_begin)));
                BlockInstJump {
                    ins_begin: prediction.ins_begin,
                    next: prediction.false_tag.into(),
                }
            }
            Statement::For(_) => todo!(),
        };
        Ok(re)
    }
}
