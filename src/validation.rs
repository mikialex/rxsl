use crate::{
    ast::FunctionDefine,
    symbol_table::{SymbolError, SymbolTable},
    visitor::Visitor,
};

pub struct ASTValidator {
    symbol_table: SymbolTable,
}

pub enum ASTValidationError {
    SymbolError(SymbolError),
}

impl Visitor<FunctionDefine, ASTValidationError> for ASTValidator {
    fn visit(&mut self, item: &FunctionDefine) -> Result<(), ASTValidationError> {
        todo!()
    }
}
