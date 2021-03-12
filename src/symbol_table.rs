use std::collections::HashMap;

use crate::ir::PrimitiveType;

pub struct SymbolInfo {
    pub ty: PrimitiveType,
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
    outer_scope: Option<Box<SymbolTable>>,
}

pub enum SymbolError {
    SameNamedSymbolHasExistInScope(String),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            outer_scope: None,
        }
    }

    pub fn search(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbols.get(name).or_else(|| {
            self.outer_scope
                .as_ref()
                .map(|outer| outer.search(name))
                .flatten()
        })
    }

    pub fn declare(&mut self, name: &str, info: SymbolInfo) -> Result<(), SymbolError> {
        let previous = self.symbols.insert(name.to_owned(), info);
        // do options have map none to result method?
        if previous.is_some() {
            Err(SymbolError::SameNamedSymbolHasExistInScope(name.to_owned()))
        } else {
            Ok(())
        }
    }

    pub fn push_scope(self) -> Self {
        let mut new_scope = Self::new();
        new_scope.outer_scope = Box::new(self).into();
        new_scope
    }

    pub fn pop_scope(self) -> Self {
        if let Some(outer) = self.outer_scope {
            *outer
        } else {
            unreachable!("failed to pop scope, no outer scope exist")
        }
    }
}
