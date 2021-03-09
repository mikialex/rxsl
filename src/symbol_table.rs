use std::collections::HashMap;

pub enum ValueType {
    F32,
    Boolean,
    Unknown,
}

pub struct SymbolInfo {
    pub ty: ValueType,
}

pub struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
    outer_scope: Option<Box<SymbolTable>>,
}

pub enum SymbolError {
    SameNamedSymbolHasExistInScope(String),
}

impl SymbolTable {
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
}
