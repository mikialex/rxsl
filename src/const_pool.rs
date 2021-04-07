use crate::{
    utils::storage::{DeduplicateVecStorage, Handle, Storage},
    PrimitiveConstValue,
};

pub struct ConstPool {
    consts: Storage<PrimitiveConstValue, DeduplicateVecStorage>,
}

pub type ConstHandle = Handle<PrimitiveConstValue, DeduplicateVecStorage>;

impl ConstPool {
    pub fn new() -> Self {
        Self {
            consts: Storage::new(),
        }
    }
    pub fn insert_const(&mut self, v: PrimitiveConstValue) -> ConstHandle {
        self.consts.insert(v)
    }
}
