use crate::ir::PrimitiveType;

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
