use std::collections::HashMap;

use crate::{ast::*, symbol_table::*};

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
    Line(usize),
    Termination,
}

impl std::fmt::Display for JumpAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JumpAddress::Unknown => write!(f, "UNKNOWN"),
            JumpAddress::Line(line) => write!(f, "{}", line),
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
            Address::Symbol(name) => format!("symbol({})", name),
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
        write!(f, "============ \n").unwrap();
        write!(f, "instructions \n").unwrap();
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
pub enum ScalarValueType {
    Float,
    Int,
    UnsignedInt,
}

#[derive(Clone, Copy, PartialEq)]
pub struct ScalarType {
    pub ty: ScalarValueType,
    pub size: usize,
}

#[derive(Clone, Copy, PartialEq)]
pub enum PrimitiveType {
    Scalar(ScalarType),
    Bool,
}

pub trait ConstPrimitive {}

pub struct PrimitiveConstValue {
    pub value: Box<dyn ConstPrimitive>,
    pub ty: PrimitiveType,
}

pub struct ConstPool {
    literals: Vec<PrimitiveConstValue>,
}
#[derive(Clone, Copy)]
pub struct ConstRef {
    index: usize,
}

impl ConstPool {
    pub fn new() -> Self {
        Self {
            literals: Vec::new(),
        }
    }
    pub fn insert_literal(&mut self) -> ConstRef {
        // merge possible same value const
        todo!()
    }
}

#[derive(Debug)]
pub enum IRGenerationError {
    TypeError,
    AssignToConst,
    SymbolError(SymbolError),
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
        line: JumpAddress,
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
                            *address = line
                        }
                        JumpInstruction::IfTrueGoto { target, .. } => {
                            assert_eq!(*target, JumpAddress::Unknown);
                            *target = line
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
        if let Some(one) = one {
            if let Some(the_other) = the_other {
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
                ins.into()
            } else {
                one.into()
            }
        } else {
            the_other
        }
    }
}

struct LoopCtx {
    start: Option<JumpUnresolved>,
    next: Option<JumpUnresolved>,
}

pub struct IRGenerator {
    instructions: InstructionList,
    jump_resolver: InstructionJumpResolver,
    loop_ctx: Vec<LoopCtx>,
    symbol_table: SymbolTable,
}

impl IRGenerator {
    pub fn new() -> Self {
        Self {
            instructions: InstructionList::new(),
            jump_resolver: InstructionJumpResolver::new(),
            loop_ctx: Vec::new(),
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn generate(ast: &Block) -> Result<InstructionList, IRGenerationError> {
        let mut generator = Self::new();
        let jmp: InstJump = generator.gen_block(ast)?;
        generator.back_patch_termination(jmp.next);
        Ok(generator.instructions)
    }

    pub fn push_loop_ctx(&mut self) {
        self.loop_ctx.push(LoopCtx {
            start: None,
            next: self.jump_resolver.new_empty().into(),
        })
    }

    pub fn pop_loop_ctx(
        &mut self,
        loop_start: Option<usize>,
        previous_next: Option<JumpUnresolved>,
    ) -> Option<JumpUnresolved> {
        let ctx = self.loop_ctx.pop().expect("no loop context stored");
        let mut r = self.merge(ctx.next, previous_next);
        self.back_patch_or_merge(ctx.start, loop_start, &mut r);
        r
    }

    pub fn loop_continue(&mut self, previous_next: Option<JumpUnresolved>) -> InstJump {
        let start = self.loop_ctx.last().expect("no loop context stored").start;
        let ins_begin = self.push_unknown_jump(unknown_goto());
        InstJump {
            ins_begin: ins_begin.0.into(),
            next: self.merge(previous_next, start),
        }
    }
    pub fn loop_break(&mut self, previous_next: Option<JumpUnresolved>) -> InstJump {
        let loop_next = self.loop_ctx.last().expect("no loop context stored").next;
        let merged = self.merge(loop_next, previous_next);
        let unknown = self.push_unknown_jump(unknown_goto());
        let next = self.merge(merged, unknown.1.into());

        InstJump {
            ins_begin: unknown.0.into(),
            next,
        }
    }

    pub fn push_inst(&mut self, ins: IRInstruction) -> usize {
        self.instructions.push(ins)
    }

    #[must_use]
    pub fn push_unknown_jump(&mut self, jump: JumpInstruction) -> (usize, JumpUnresolved) {
        let line = self.push_inst(IRInstruction::Jump(jump));
        let r = self.jump_resolver.new_by_line(line);
        (line, r)
    }

    // #[must_use]
    // pub fn next_inst_line(&self) -> usize {
    //     self.instructions.instructions.len()
    // }

    pub fn back_patch(&mut self, jump: Option<JumpUnresolved>, line: usize) {
        self.jump_resolver
            .back_patch(jump, JumpAddress::Line(line), &mut self.instructions)
    }
    pub fn back_patch_termination(&mut self, jump: Option<JumpUnresolved>) {
        self.jump_resolver
            .back_patch(jump, JumpAddress::Termination, &mut self.instructions)
    }

    pub fn back_patch_or_merge(
        &mut self,
        jump: Option<JumpUnresolved>,
        line: Option<usize>,
        next: &mut Option<JumpUnresolved>,
    ) {
        if let Some(line) = line {
            self.jump_resolver
                .back_patch(jump, JumpAddress::Line(line), &mut self.instructions)
        } else {
            *next = self.merge(jump, *next);
        }
    }

    pub fn back_patch_or_merge_bool_exp(
        &mut self,
        jump: Option<JumpUnresolved>,
        exp: &mut BooleanInstExpJump,
    ) {
        if let Some(line) = exp.ins_begin {
            self.jump_resolver
                .back_patch(jump, JumpAddress::Line(line), &mut self.instructions);
        } else {
            let single = exp.expect_single_jump();
            *single.1 = self.merge(jump, *single.1);
        }
    }

    pub fn back_patch_or_merge_bool_exp_with_given_line(
        &mut self,
        jump: Option<JumpUnresolved>,
        line: Option<usize>,
        exp: &mut BooleanInstExpJump,
    ) {
        if let Some(line) = line {
            self.jump_resolver
                .back_patch(jump, JumpAddress::Line(line), &mut self.instructions);
        } else {
            let single = exp.expect_single_jump();
            *single.1 = self.merge(jump, *single.1);
        }
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

impl IRGenerator {
    fn binary_operator_type_checking(
        &self,
        left_ty: PrimitiveType,
        op: BinaryOperator,
        right_ty: PrimitiveType,
    ) -> Result<PrimitiveType, IRGenerationError> {
        fn expect_both_scalar(
            l: PrimitiveType,
            r: PrimitiveType,
            bool_re: bool,
        ) -> Result<PrimitiveType, IRGenerationError> {
            if l != r {
                return Err(IRGenerationError::TypeError);
            }
            match l {
                PrimitiveType::Scalar(_) => {}
                _ => return Err(IRGenerationError::TypeError),
            };
            match r {
                PrimitiveType::Scalar(_) => {}
                _ => return Err(IRGenerationError::TypeError),
            };
            if bool_re {
                Ok(PrimitiveType::Bool)
            } else {
                Ok(l)
            }
        }
        fn expect_both_boolean(
            l: PrimitiveType,
            r: PrimitiveType,
        ) -> Result<PrimitiveType, IRGenerationError> {
            if l != PrimitiveType::Bool || r != PrimitiveType::Bool {
                return Err(IRGenerationError::TypeError);
            }
            return Ok(PrimitiveType::Bool);
        }
        use BinaryOperator::*;
        match op {
            Add | Sub | Mul | Div | Mod => expect_both_scalar(left_ty, right_ty, false),
            Less | LessEqual | Greater | GreaterEqual | Equal | NotEqual => {
                expect_both_scalar(left_ty, right_ty, true)
            }
            And | Or | Xor => todo!(),
            LogicalAnd | LogicalOr => expect_both_boolean(left_ty, right_ty),
        }
    }
    fn unary_operator_type_checking(
        &self,
        ty: PrimitiveType,
        op: UnaryOperator,
    ) -> Result<PrimitiveType, IRGenerationError> {
        match op {
            UnaryOperator::Neg => match ty {
                PrimitiveType::Scalar(_) => Ok(ty),
                PrimitiveType::Bool => Err(IRGenerationError::TypeError),
            },
            UnaryOperator::Not => match ty {
                PrimitiveType::Scalar(_) => Err(IRGenerationError::TypeError),
                PrimitiveType::Bool => Ok(ty),
            },
        }
    }

    fn gen_exp(
        &mut self,
        exp: &Expression,
    ) -> Result<(Address, ExpInstJump, PrimitiveType), IRGenerationError> {
        let r = match exp {
            Expression::UnaryOperator { op, expr } => {
                let (address, exp, ty) = self.gen_exp(expr)?;
                let ins_begin = self
                    .push_inst(IRInstruction::unary(*op, address.clone()))
                    .into();

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
                let ty = self.unary_operator_type_checking(ty, *op)?;
                (address, jmp, ty)
            }
            Expression::BinaryOperator { left, op, right } => {
                let (address1, arg1, ty1) = self.gen_exp(left)?;
                let (address2, arg2, ty2) = self.gen_exp(right)?;
                let compute = self.push_inst(IRInstruction::binary(*op, address1, address2));

                let ins_begin = arg1
                    .get_ins_begin()
                    .or(arg2.get_ins_begin())
                    .or(compute.into());

                let re = match op {
                    BinaryOperator::Less
                    | BinaryOperator::LessEqual
                    | BinaryOperator::Greater
                    | BinaryOperator::GreaterEqual
                    | BinaryOperator::Equal
                    | BinaryOperator::NotEqual => ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin: compute.into(),
                        true_tag: self
                            .push_unknown_jump(unknown_goto_if_true(Address::Address(compute)))
                            .1
                            .into(),
                        false_tag: self.push_unknown_jump(unknown_goto()).1.into(),
                    }),
                    BinaryOperator::LogicalAnd | BinaryOperator::LogicalOr => {
                        let arg1 = arg1.expect_boolean()?;
                        let mut arg2 = arg2.expect_boolean()?;
                        match op {
                            BinaryOperator::LogicalOr => {
                                self.back_patch_or_merge_bool_exp(arg1.false_tag, &mut arg2);
                                ExpInstJump::Bool(BooleanInstExpJump {
                                    ins_begin: compute.into(),
                                    true_tag: self.merge(arg1.true_tag, arg2.true_tag),
                                    false_tag: arg2.false_tag,
                                })
                            }
                            BinaryOperator::LogicalAnd => {
                                self.back_patch_or_merge_bool_exp(arg1.true_tag, &mut arg2);
                                ExpInstJump::Bool(BooleanInstExpJump {
                                    ins_begin: compute.into(),
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
                let ty = self.binary_operator_type_checking(ty1, *op, ty2)?;
                (Address::Address(compute), re, ty)
            }
            Expression::FunctionCall(_) => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::ItemAccess { from, to } => todo!(),
            Expression::Assign { left, right } => {
                let (address, jmp, exp_ty) = self.gen_exp(&right)?;
                let SymbolInfo { is_const, ty } = *self
                    .symbol_table
                    .search(left.name.as_str())
                    .map_err(|e| IRGenerationError::SymbolError(e))?;
                if is_const {
                    return Err(IRGenerationError::AssignToConst);
                }
                if exp_ty != ty {
                    return Err(IRGenerationError::TypeError);
                }
                let line = self.push_inst(IRInstruction::Copy {
                    source: address,
                    target: Address::Symbol(left.name.clone()),
                });
                match jmp {
                    ExpInstJump::Common(jmp) => self.back_patch(jmp.next, line),
                    ExpInstJump::Bool(jmp) => {
                        self.back_patch(jmp.true_tag, line);
                        self.back_patch(jmp.false_tag, line)
                    }
                }
                (
                    Address::Symbol(left.name.clone()),
                    ExpInstJump::Common(InstJump {
                        ins_begin: jmp.get_ins_begin().or(line.into()),
                        next: None,
                    }),
                    ty,
                )
            }
            Expression::Number {} => (
                Address::Const(),
                ExpInstJump::Common(InstJump {
                    ins_begin: None,
                    next: None,
                }),
                PrimitiveType::Scalar(ScalarType {
                    ty: ScalarValueType::Float,
                    size: 1,
                }),
            ),
            Expression::Bool(v) => {
                let jmp = if *v {
                    let true_tag = self.push_unknown_jump(unknown_goto());
                    ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin: true_tag.0.into(),
                        true_tag: true_tag.1.into(),
                        false_tag: None,
                    })
                } else {
                    let false_tag = self.push_unknown_jump(unknown_goto());
                    ExpInstJump::Bool(BooleanInstExpJump {
                        ins_begin: false_tag.0.into(),
                        true_tag: None,
                        false_tag: false_tag.1.into(),
                    })
                };
                (Address::Const(), jmp, PrimitiveType::Bool)
            }
            Expression::Ident(name) => {
                let ty = self
                    .symbol_table
                    .search(name.name.as_str())
                    .map_err(|e| IRGenerationError::SymbolError(e))?
                    .ty;
                let jmp = match ty {
                    PrimitiveType::Bool => {
                        let jmp = self.push_unknown_jump(unknown_goto_if_true(Address::Symbol(
                            name.name.clone(),
                        )));
                        ExpInstJump::Bool(BooleanInstExpJump {
                            ins_begin: jmp.0.into(),
                            true_tag: jmp.1.into(),
                            false_tag: self.push_unknown_jump(unknown_goto()).1.into(),
                        })
                    }
                    _ => ExpInstJump::Common(InstJump {
                        ins_begin: None,
                        next: None,
                    }),
                };
                (Address::Symbol(name.name.clone()), jmp, ty)
            }
        };
        Ok(r)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct InstJump {
    ins_begin: Option<usize>,
    next: Option<JumpUnresolved>,
}

impl IRGenerator {
    fn gen_block(&mut self, b: &Block) -> Result<InstJump, IRGenerationError> {
        let mut ins_begin = None;
        let mut last_next: Option<JumpUnresolved> = None;
        self.symbol_table.push_scope();
        for s in &b.statements {
            let mut jump = self.gen_statement(s, last_next)?;
            if ins_begin.is_none() {
                ins_begin = jump.ins_begin;
            }
            self.back_patch_or_merge(last_next, jump.ins_begin, &mut jump.next);
            last_next = jump.next;
        }
        self.symbol_table.pop_scope();
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
    pub fn get_ins_begin(&self) -> Option<usize> {
        match self {
            ExpInstJump::Common(jmp) => jmp.ins_begin,
            ExpInstJump::Bool(jmp) => jmp.ins_begin,
        }
    }

    pub fn expect_boolean(&self) -> Result<BooleanInstExpJump, IRGenerationError> {
        match *self {
            Self::Bool(b) => Ok(b),
            _ => Err(IRGenerationError::TypeError),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BooleanInstExpJump {
    ins_begin: Option<usize>,
    true_tag: Option<JumpUnresolved>,
    false_tag: Option<JumpUnresolved>,
}

impl BooleanInstExpJump {
    pub fn merge_branch(self, g: &mut IRGenerator) -> InstJump {
        InstJump {
            ins_begin: self.ins_begin,
            next: g.merge(self.true_tag, self.false_tag),
        }
    }
    pub fn expect_single_jump(&mut self) -> (bool, &mut Option<JumpUnresolved>) {
        if self.true_tag.is_some() && self.false_tag.is_some() {
            panic!()
        }
        if self.true_tag.is_none() && self.false_tag.is_none() {
            panic!()
        }
        if self.true_tag.is_some() {
            (true, &mut self.true_tag)
        } else {
            (false, &mut self.false_tag)
        }
    }
}

impl IRGenerator {
    fn gen_statement(
        &mut self,
        stmt: &Statement,
        previous_next: Option<JumpUnresolved>,
    ) -> Result<InstJump, IRGenerationError> {
        let re = match stmt {
            Statement::Block(b) => self.gen_block(b)?,
            Statement::Declare { ty, name, init } => {
                let (source, exp, vty) = self.gen_exp(init)?;
                self.symbol_table
                    .declare(
                        name.name.as_str(),
                        SymbolInfo {
                            is_const: *ty == DeclarationType::Const,
                            ty: vty,
                        },
                    )
                    .map_err(|e| IRGenerationError::SymbolError(e))?;

                let ins_begin = self.push_inst(IRInstruction::Copy {
                    source,
                    target: Address::Symbol(name.name.clone()),
                });
                match exp {
                    ExpInstJump::Common(j) => self.back_patch(j.next, ins_begin),
                    ExpInstJump::Bool(b) => {
                        self.back_patch(b.true_tag, ins_begin);
                        self.back_patch(b.false_tag, ins_begin)
                    }
                }
                let ins_begin = exp.get_ins_begin().or(ins_begin.into());

                InstJump {
                    ins_begin: ins_begin,
                    next: None,
                }
            }
            Statement::Empty => InstJump {
                ins_begin: None,
                next: previous_next,
            },
            Statement::Expression(exp) => {
                let (_, exp_jmp, _) = self.gen_exp(exp)?;
                match exp_jmp {
                    ExpInstJump::Common(jmp) => jmp,
                    ExpInstJump::Bool(b_jmp) => b_jmp.merge_branch(self),
                }
            }
            Statement::Return { .. } => todo!(),
            Statement::Continue => self.loop_continue(previous_next),
            Statement::Break => self.loop_break(previous_next),
            Statement::If(des) => {
                let (_, prediction, _) = self.gen_exp(&des.condition)?;
                let prediction = prediction.expect_boolean()?;
                let mut first_accept_block: InstJump = self.gen_block(&des.accept)?;

                self.back_patch_or_merge(
                    prediction.true_tag,
                    first_accept_block.ins_begin,
                    &mut first_accept_block.next,
                );

                let mut next = self.push_unknown_jump(unknown_goto()).1.into();
                next = self.merge(first_accept_block.next, next);

                let else_block = if let Some(reject) = &des.reject {
                    let mut else_block: InstJump = self.gen_block(reject)?;
                    next = self.merge(else_block.next, next);

                    // todo not consider else if block yet
                    self.back_patch_or_merge(
                        prediction.false_tag,
                        else_block.ins_begin,
                        &mut else_block.next,
                    );
                    else_block.into()
                } else {
                    next = self.merge(prediction.false_tag, next);
                    None
                };

                let mut last_prediction = prediction;

                // let mut is_first = true;
                for else_if in &des.elses {
                    let (_, else_if_prediction, _) = self.gen_exp(&else_if.condition)?;
                    let else_if_prediction = else_if_prediction.expect_boolean()?;

                    // if is_first {
                    //     self.back_patch_or_merge(
                    //         prediction.false_tag,
                    //         else_if_prediction.ins_begin,
                    //         &mut else_if_prediction.false_tag,
                    //     );
                    //     self.back_patch_or_merge(
                    //         prediction.false_tag,
                    //         else_if_prediction.ins_begin,
                    //         &mut else_if_prediction.true_tag,
                    //     );
                    //     is_first = false;
                    // }

                    let mut accept_block: InstJump = self.gen_block(&des.accept)?;

                    self.back_patch_or_merge(
                        else_if_prediction.true_tag,
                        accept_block.ins_begin,
                        &mut accept_block.next,
                    );
                    next = self.merge(accept_block.next, next);

                    if let Some(mut else_block) = else_block {
                        self.back_patch_or_merge(
                            else_if_prediction.false_tag,
                            else_block.ins_begin,
                            &mut else_block.next,
                        )
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
                self.push_loop_ctx();
                let (_, prediction, _) = self.gen_exp(&des.condition)?;
                let mut prediction = prediction.expect_boolean()?;

                let mut loop_body: InstJump = self.gen_block(&des.body)?;
                loop_body.next = self.pop_loop_ctx(prediction.ins_begin, loop_body.next);

                self.back_patch_or_merge_bool_exp(loop_body.next, &mut prediction);
                self.back_patch_or_merge_bool_exp_with_given_line(
                    prediction.true_tag,
                    loop_body.ins_begin,
                    &mut prediction,
                );

                if let Some(prediction_begin) = prediction.ins_begin {
                    self.push_inst(IRInstruction::Jump(JumpInstruction::Goto(
                        JumpAddress::Line(prediction_begin),
                    )));
                    InstJump {
                        ins_begin: prediction.ins_begin,
                        next: self.merge(prediction.false_tag, previous_next),
                    }
                } else {
                    let (const_result, _) = prediction.expect_single_jump();
                    if const_result {
                        // goto self, infinite loop
                        let ins_begin = self.push_inst(IRInstruction::Jump(JumpInstruction::Goto(
                            JumpAddress::Line(self.instructions.instructions.len()),
                        )));
                        InstJump {
                            ins_begin: ins_begin.into(),
                            next: None,
                        }
                    } else {
                        InstJump {
                            ins_begin: None,
                            next: previous_next,
                        }
                    }
                }
            }
            Statement::For(_) => todo!(),
        };
        Ok(re)
    }
}
