use std::collections::HashMap;

use crate::{ast::*, symbol_table::SymbolTable, visitor::*};

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
    },
    Goto(JumpAddress),
    IfTrueGoto {
        prediction: Address,
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
    pub fn unary(op: UnaryOperator, target: Address) -> Self {
        Self::Unary { op, target }
    }
    pub fn binary(op: BinaryOperator, left: Address, right: Address) -> Self {
        Self::Binary { op, left, right }
    }
    pub fn if_true_goto(prediction: Address, target: JumpAddress) -> Self {
        Self::IfTrueGoto { prediction, target }
    }
}

pub enum Address {
    Address(usize),
    Symbol(String),
    Const(), // todo
}

pub struct InstructionList {
    instructions: Vec<usize>,
    instruction_pool: Vec<IRInstruction>,
}

impl InstructionList {
    fn format_arg(&self, arg: &Address) -> String {
        match arg {
            Address::Address(index) => format!("t{}", index),
            Address::Symbol(symbol) => format!("{}", symbol),
            Address::Const() => format!("const"),
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

    pub fn push(&mut self, ins: IRInstruction) -> Address {
        self.instruction_pool.push(ins);
        let index = self.instruction_pool.len();
        self.instructions.push(index);
        Address::Address(index)
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
    pub fn new_item_next_next_inst(&mut self) -> JumpUnresolved {
        self.jump_resolver.new_by_line(self.next_inst_line() + 1)
    }
    pub fn next_inst_line(&self) -> usize {
        self.instructions.instructions.len()
    }
}

impl Visitor<Expression, ExpInstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, exp: &Expression) -> Result<ExpInstJump, IRGenerationError> {
        let ins_begin = self.next_inst_line();
        let r: ExpInstJump = match exp {
            Expression::UnaryOperator { op, expr } => {
                let exp: ExpInstJump = expr.visit_by(self)?;

                let ins = IRInstruction::unary(*op, Address::Address(exp.ins_begin));
                self.instructions.push(ins);
                match op {
                    UnaryOperator::Not => {
                        let bool_exp = exp.expect_boolean()?;
                        ExpInstJump::Bool(BooleanInstJump {
                            ins_begin,
                            true_tag: bool_exp.false_tag,
                            false_tag: bool_exp.true_tag,
                        })
                    }
                    _ => ExpInstJump::Common(InstJump {
                        ins_begin,
                        next: None,
                    }),
                }
            }
            Expression::BinaryOperator { left, op, right } => {
                let arg1: ExpInstJump = left.visit_by(self)?;
                let arg2: ExpInstJump = right.visit_by(self)?;
                let ins = IRInstruction::binary(
                    *op,
                    Address::Address(arg1.ins_begin),
                    Address::Address(arg2.ins_begin),
                );
                let compute = self.instructions.push(ins);

                let re = match op {
                    BinaryOperator::Less
                    | BinaryOperator::LessEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual
                    | BinaryOperator::Equal
                    | BinaryOperator::NotEqual => {
                        let j = ExpInstJump::Bool(BooleanInstJump {
                            ins_begin,
                            true_tag: self.new_item_next_inst(),
                            false_tag: self.new_item_next_next_inst(),
                        });
                        self.instructions
                            .push(IRInstruction::if_true_goto(compute, JumpAddress::Unknown));
                        self.instructions
                            .push(IRInstruction::Goto(JumpAddress::Unknown));
                        j
                    }
                    _ => ExpInstJump::Common(InstJump {
                        ins_begin,
                        next: None,
                    }),
                };
                re
            }
            Expression::FunctionCall(_) => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::ItemAccess { from, to } => todo!(),
            Expression::Assign { left, right } => todo!(),
            Expression::Number {} => Address::Const(),
            Expression::Bool(v) => Address::Const(),
            Expression::Ident(name) => {
                Address::Symbol(name.name.clone())
                // todo!()
                // self.symbol_table.search(name.name.as_str());
            }
        };
        Ok(r)
    }
}

struct InstJump {
    ins_begin: usize,
    next: Option<JumpUnresolved>,
}

impl Visitor<Block, InstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, b: &Block) -> Result<InstJump, IRGenerationError> {
        let ins_begin = self.next_inst_line();
        let mut last_next: Option<JumpUnresolved>;
        for s in b.statements {
            let jump: InstJump = s.visit_by(self)?;
            if let Some(last_next) = last_next {
                self.jump_resolver.back_patch(last_next, jump.ins_begin)
            }
            last_next = jump.next;
        }
        Ok(InstJump {
            ins_begin,
            next: last_next,
        })
    }
}

pub enum ExpInstJump {
    Common(InstJump),
    Bool(BooleanInstJump),
}

impl ExpInstJump {
    pub fn expect_boolean(&self) -> Result<BooleanInstJump, IRGenerationError> {
        todo!()
    }
}

struct BooleanInstJump {
    ins_begin: usize,
    true_tag: JumpUnresolved,
    false_tag: JumpUnresolved,
}

impl Visitor<Statement, InstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, stmt: &Statement) -> Result<InstJump, IRGenerationError> {
        let re = match stmt {
            Statement::Block(b) => b.visit_by(self)?,
            Statement::Declare { ty, name, init } => {
                let ins_begin = self.next_inst_line();
                self.symbol_table.declare(name.name.as_str(), todo!());
                InstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Empty => {
                let ins_begin = self.next_inst_line();
                InstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Expression(_) => {
                let ins_begin = self.next_inst_line();
                InstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Return { .. } => todo!(),
            Statement::If(des) => {
                let prediction: Address = des.condition.visit_by(self)?;
                todo!()
            }
            Statement::While(des) => {
                let prediction: BooleanInstJump = des.condition.visit_by(self)?;
                let loop_body: InstJump = des.body.visit_by(self)?;
                if let Some(loop_body_next) = loop_body.next {
                    self.jump_resolver
                        .back_patch(loop_body_next, prediction.ins_begin);
                }
                self.jump_resolver
                    .back_patch(prediction.true_tag, loop_body.ins_begin);
                self.instructions
                    .push(IRInstruction::Goto(JumpAddress::Line(prediction.ins_begin)));
                InstJump {
                    ins_begin: prediction.ins_begin,
                    next: prediction.false_tag.into(),
                }
            }
            Statement::For(_) => todo!(),
        };
        Ok(re)
    }
}
