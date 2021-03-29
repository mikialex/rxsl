use crate::ir::PrimitiveType;
use std::collections::HashMap;

#[derive(Clone, Copy)]
pub struct SymbolInfo {
    pub is_const: bool,
    pub ty: PrimitiveType,
}

pub struct SymbolTable {
    scopes: Vec<ScopeSymbolTable>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    pub fn push_scope(&mut self) {
        self.scopes.push(ScopeSymbolTable::new());
    }

    pub fn pop_scope(&mut self) {
        self.scopes
            .pop()
            .expect("failed to pop scope, no outer scope exist");
    }

    #[must_use]
    pub fn search(&self, name: &str) -> Result<&SymbolInfo, SymbolError> {
        self.scopes
            .iter()
            .rev()
            .find_map(|table| table.symbols.get(name))
            .ok_or(SymbolError::NotExist(name.to_owned()))
    }

    pub fn declare(&mut self, name: &str, info: SymbolInfo) -> Result<(), SymbolError> {
        let previous = self
            .scopes
            .last_mut()
            .unwrap()
            .symbols
            .insert(name.to_owned(), info);
        // do options have map none to result method?
        if previous.is_some() {
            Err(SymbolError::NameConflict(name.to_owned()))
        } else {
            Ok(())
        }
    }
}

pub struct ScopeSymbolTable {
    symbols: HashMap<String, SymbolInfo>,
}

#[derive(Debug)]
pub enum SymbolError {
    NameConflict(String),
    NotExist(String),
}

impl ScopeSymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
        }
    }
}
