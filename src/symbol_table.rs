use std::collections::HashMap;

use crate::ir::TypeInValidation;

pub struct SymbolInfo {
    pub is_const: bool,
    pub ty: TypeInValidation,
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
    outer_scope: Option<Box<SymbolTable>>,
}

#[derive(Debug)]
pub enum SymbolError {
    NameConflict(String),
    NotExist(String),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            outer_scope: None,
        }
    }

    #[must_use]
    pub fn search(&self, name: &str) -> Result<&SymbolInfo, SymbolError> {
        self.symbols
            .get(name)
            .or_else(|| {
                self.outer_scope
                    .as_ref()
                    .map(|outer| outer.search(name).ok())
                    .flatten()
            })
            .ok_or(SymbolError::NotExist(name.to_owned()))
    }

    pub fn declare(&mut self, name: &str, info: SymbolInfo) -> Result<(), SymbolError> {
        let previous = self.symbols.insert(name.to_owned(), info);
        // do options have map none to result method?
        if previous.is_some() {
            Err(SymbolError::NameConflict(name.to_owned()))
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
