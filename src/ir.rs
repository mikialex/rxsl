use std::{collections::HashMap, todo};

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
}

#[derive(Clone, Copy)]
pub enum Address {
    Address(usize),
    Symbol(),
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
            Address::Symbol() => format!("symbol"),
            Address::Const() => format!("const"),
        }
    }

    fn mut_instruction_by_line(&mut self, line: usize) -> &mut IRInstruction {
        let index = self.instructions[line];
        &mut self.instruction_pool[index]
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
                IRInstruction::Jump(jump) => match jump {
                    JumpInstruction::Goto(target) => write!(f, "goto line {}\n", target),
                    JumpInstruction::IfTrueGoto { prediction, target } => write!(
                        f,
                        "if {} goto line {}\n",
                        self.format_arg(prediction),
                        target
                    ),
                },
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
    pub ty: ScalarValueType,
    pub size: usize,
}

pub enum PrimitiveType {
    Scalar(ScalarType),
    Bool,
}

pub enum IRGenerationError {
    TypeError,
}

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

    pub fn new_empty(&mut self) -> JumpUnresolved {
        let ins = JumpUnresolved {
            inner: self.latest_item_index,
        };
        self.latest_item_index += 1;
        self.unresolved_items.insert(ins, vec![]);
        ins
    }

    pub fn new_by_line(&mut self, ins_line: usize) -> JumpUnresolved {
        let ins = JumpUnresolved {
            inner: self.latest_item_index,
        };
        self.latest_item_index += 1;
        self.unresolved_items.insert(ins, vec![ins_line]);
        ins
    }

    pub fn back_patch(
        &mut self,
        jump: Option<JumpUnresolved>,
        line: usize,
        instructions: &mut InstructionList,
    ) {
        if let Some(jump) = jump {
            let jump_instructions = self
                .unresolved_items
                .remove(&jump)
                .expect("can not back patch a resolved/or not exist unresolved jump");
            jump_instructions.iter().for_each(|&ins| {
                let ins = instructions.mut_instruction_by_line(ins);
                match ins {
                    IRInstruction::Jump(ins) => match ins {
                        JumpInstruction::Goto(address) => {
                            assert_eq!(*address, JumpAddress::Unknown);
                            *address = JumpAddress::Line(line)
                        }
                        JumpInstruction::IfTrueGoto { target, .. } => {
                            assert_eq!(*target, JumpAddress::Unknown);
                            *target = JumpAddress::Line(line)
                        }
                    },
                    _ => unreachable!("try back patch a none jump instruction"),
                }
            })
        }
    }

    pub fn merge(
        &mut self,
        one: Option<JumpUnresolved>,
        the_other: Option<JumpUnresolved>,
    ) -> Option<JumpUnresolved> {
        one.zip(the_other).map(|(one, the_other)| {
            let mut one = self
                .unresolved_items
                .remove(&one)
                .expect("jump not exist in resolver");

            let the_other = self
                .unresolved_items
                .remove(&the_other)
                .expect("jump not exist in resolver");

            one.extend(the_other);

            let ins = JumpUnresolved {
                inner: self.latest_item_index,
            };
            self.latest_item_index += 1;
            self.unresolved_items.insert(ins, one);
            ins
        })
    }
}

struct FunctionCtx {
    return_point: Option<JumpUnresolved>,
}

struct LoopCtx {
    start: usize,
    next: Option<JumpUnresolved>,
}

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
    pub fn push_loop_ctx(&mut self) {
        self.loop_ctx.push(LoopCtx {
            start: self.next_inst_line(),
            next: self.jump_resolver.new_empty().into(),
        })
    }

    pub fn pop_loop_ctx(
        &mut self,
        previous_next: Option<JumpUnresolved>,
    ) -> Option<JumpUnresolved> {
        let ctx = self.loop_ctx.pop().expect("no loop context stored");
        self.merge(ctx.next, previous_next)
    }

    pub fn loop_continue(&mut self, previous_next: Option<JumpUnresolved>) -> InstJump {
        let start = self.loop_ctx.last().expect("no loop context stored").start;
        let ins_begin = self.next_inst_line();
        self.back_patch(previous_next, start);
        self.push_inst(IRInstruction::Jump(JumpInstruction::Goto(
            JumpAddress::Line(start),
        )));
        InstJump {
            ins_begin,
            next: None,
        }
    }
    pub fn loop_break(&mut self, previous_next: Option<JumpUnresolved>) -> InstJump {
        let loop_next = self.loop_ctx.last().expect("no loop context stored").next;
        let merged = self.merge(loop_next, previous_next);
        let ins_begin = self.next_inst_line();
        let unknown = self.push_unknown_jump(unknown_goto());
        let next = self.merge(merged, unknown.into());

        InstJump { ins_begin, next }
    }

    pub fn push_inst(&mut self, ins: IRInstruction) -> Address {
        self.instructions.push(ins)
    }

    #[must_use]
    pub fn push_unknown_jump(&mut self, jump: JumpInstruction) -> JumpUnresolved {
        let r = self.jump_resolver.new_by_line(self.next_inst_line());
        self.push_inst(IRInstruction::Jump(jump));
        r
    }

    #[must_use]
    pub fn next_inst_line(&self) -> usize {
        self.instructions.instructions.len()
    }

    pub fn back_patch(&mut self, jump: Option<JumpUnresolved>, line: usize) {
        self.jump_resolver
            .back_patch(jump, line, &mut self.instructions)
    }

    #[must_use]
    pub fn merge(
        &mut self,
        one: Option<JumpUnresolved>,
        the_other: Option<JumpUnresolved>,
    ) -> Option<JumpUnresolved> {
        self.jump_resolver.merge(one, the_other)
    }
}

impl Visitor<Expression, (Address, ExpInstJump), IRGenerationError> for IRGenerator {
    fn visit(&mut self, exp: &Expression) -> Result<(Address, ExpInstJump), IRGenerationError> {
        let ins_begin = self.next_inst_line();
        let r: (Address, ExpInstJump) = match exp {
            Expression::UnaryOperator { op, expr } => {
                let (address, exp): (Address, ExpInstJump) = expr.visit_by(self)?;
                self.push_inst(IRInstruction::unary(*op, address));

                let jmp = match op {
                    UnaryOperator::Not => {
                        let bool_exp = exp.expect_boolean()?;

                        ExpInstJump::Bool(BooleanInstExpJump {
                            ins_begin,
                            true_tag: bool_exp.false_tag,
                            false_tag: bool_exp.true_tag,
                        })
                    }
                    _ => ExpInstJump::Common(InstJump {
                        ins_begin,
                        next: None,
                    }),
                };
                (address, jmp)
            }
            Expression::BinaryOperator { left, op, right } => {
                let (address1, arg1): (Address, ExpInstJump) = left.visit_by(self)?;
                let (address2, arg2): (Address, ExpInstJump) = right.visit_by(self)?;
                let compute = self.push_inst(IRInstruction::binary(*op, address1, address2));

                let re = match op {
                    BinaryOperator::Less
                    | BinaryOperator::LessEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual
                    | BinaryOperator::Equal
                    | BinaryOperator::NotEqual => ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin,
                        true_tag: self.push_unknown_jump(unknown_goto_if_true(compute)).into(),
                        false_tag: self.push_unknown_jump(unknown_goto()).into(),
                    }),
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        let arg1 = arg1.expect_boolean()?;
                        let arg2 = arg2.expect_boolean()?;
                        match op {
                            BinaryOperator::LogicalOr => {
                                self.back_patch(arg1.false_tag, arg2.ins_begin);
                                ExpInstJump::Bool(BooleanInstExpJump {
                                    ins_begin,
                                    true_tag: self.merge(arg1.true_tag, arg2.true_tag),
                                    false_tag: arg2.false_tag,
                                })
                            }
                            BinaryOperator::LogicalAnd => {
                                self.back_patch(arg1.true_tag, arg2.ins_begin);
                                ExpInstJump::Bool(BooleanInstExpJump {
                                    ins_begin,
                                    true_tag: arg2.true_tag,
                                    false_tag: self.merge(arg1.false_tag, arg2.false_tag),
                                })
                            }
                            _ => unreachable!(),
                        }
                    }
                    _ => ExpInstJump::Common(InstJump {
                        ins_begin,
                        next: None,
                    }),
                };
                (compute, re)
            }
            Expression::FunctionCall(_) => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::ItemAccess { from, to } => todo!(),
            Expression::Assign { left, right } => todo!(),
            Expression::Number {} => todo!(),
            Expression::Bool(v) => {
                let jmp = if *v {
                    ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin,
                        true_tag: self.push_unknown_jump(unknown_goto()).into(),
                        false_tag: None,
                    })
                } else {
                    ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin,
                        true_tag: None,
                        false_tag: self.push_unknown_jump(unknown_goto()).into(),
                    })
                };
                (Address::Const(), jmp)
            }
            Expression::Ident(name) => {
                (Address::Symbol(), todo!())
                // todo!()
                // self.symbol_table.search(name.name.as_str());
            }
        };
        Ok(r)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InstJump {
    ins_begin: usize,
    next: Option<JumpUnresolved>,
}

impl Visitor<Block, InstJump, IRGenerationError> for IRGenerator {
    fn visit(&mut self, b: &Block) -> Result<InstJump, IRGenerationError> {
        let ins_begin = self.next_inst_line();
        let mut last_next: Option<JumpUnresolved> = None;
        for s in &b.statements {
            let jump = self.code_gen_statement(s, last_next)?;
            self.back_patch(last_next, jump.ins_begin);
            last_next = jump.next;
        }
        Ok(InstJump {
            ins_begin,
            next: last_next,
        })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ExpInstJump {
    Common(InstJump),
    Bool(BooleanInstExpJump),
}

impl ExpInstJump {
    pub fn expect_boolean(&self) -> Result<BooleanInstExpJump, IRGenerationError> {
        match *self {
            Self::Bool(b) => Ok(b),
            _ => Err(IRGenerationError::TypeError),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BooleanInstExpJump {
    ins_begin: usize,
    true_tag: Option<JumpUnresolved>,
    false_tag: Option<JumpUnresolved>,
}

impl IRGenerator {
    fn code_gen_statement(
        &mut self,
        stmt: &Statement,
        previous_next: Option<JumpUnresolved>,
    ) -> Result<InstJump, IRGenerationError> {
        let re = match stmt {
            Statement::Block(b) => b.visit_by(self)?,
            Statement::Declare { ty, name, init } => {
                let ins_begin = self.next_inst_line();
                self.symbol_table.declare(name.name.as_str(), todo!());
                if let Some(init) = init {
                    let (source, exp) = init.visit_by(self)?;
                    match exp {
                        ExpInstJump::Common(j) => self.back_patch(j.next, self.next_inst_line()),
                        ExpInstJump::Bool(b) => {
                            self.back_patch(b.true_tag, self.next_inst_line());
                            self.back_patch(b.false_tag, self.next_inst_line())
                        }
                    }
                    self.push_inst(IRInstruction::Copy { source });
                }
                InstJump {
                    ins_begin,
                    next: None,
                }
            }
            Statement::Empty => {
                let ins_begin = self.next_inst_line();
                InstJump {
                    ins_begin,
                    next: previous_next,
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
            Statement::Continue => self.loop_continue(previous_next),
            Statement::Break => self.loop_break(previous_next),
            Statement::If(des) => {
                let (_, prediction): (Address, ExpInstJump) = des.condition.visit_by(self)?;
                let prediction = prediction.expect_boolean()?;
                let first_accept_block: InstJump = des.accept.visit_by(self)?;
                self.back_patch(prediction.true_tag, first_accept_block.ins_begin);

                let mut next = first_accept_block.next;

                let else_block = if let Some(reject) = &des.reject {
                    let else_block: InstJump = reject.visit_by(self)?;
                    next = self.merge(else_block.next, next);
                    self.back_patch(prediction.true_tag, else_block.ins_begin);
                    else_block.into()
                } else {
                    next = self.merge(prediction.false_tag, next);
                    None
                };

                let mut last_prediction = prediction;

                for else_if in &des.elses {
                    let (_, else_if_prediction) = else_if.condition.visit_by(self)?;
                    let else_if_prediction = else_if_prediction.expect_boolean()?;
                    let accept_block: InstJump = des.accept.visit_by(self)?;

                    self.back_patch(else_if_prediction.true_tag, accept_block.ins_begin);
                    next = self.merge(accept_block.next, next);

                    if let Some(else_block) = else_block {
                        self.back_patch(else_if_prediction.false_tag, else_block.ins_begin)
                    } else {
                        next = self.merge(last_prediction.false_tag, next);
                    }

                    last_prediction = else_if_prediction;
                }

                InstJump {
                    ins_begin: prediction.ins_begin,
                    next,
                }
            }
            Statement::While(des) => {
                let (_, prediction): (Address, ExpInstJump) = des.condition.visit_by(self)?;
                let prediction = prediction.expect_boolean()?;

                self.push_loop_ctx();
                let mut loop_body: InstJump = des.body.visit_by(self)?;
                loop_body.next = self.pop_loop_ctx(loop_body.next);

                self.back_patch(loop_body.next, prediction.ins_begin);
                self.back_patch(prediction.true_tag, loop_body.ins_begin);
                self.instructions
                    .push(IRInstruction::Jump(JumpInstruction::Goto(
                        JumpAddress::Line(prediction.ins_begin),
                    )));
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
