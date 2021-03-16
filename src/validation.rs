use crate::{
    ast::*,
    ir::*,
    symbol_table::{SymbolError, SymbolTable},
    visitor::{SyntaxTreeVisitable, Visitor},
};

pub struct ASTValidator {
    symbol_table: SymbolTable,
}

impl ASTValidator {
    pub fn new() -> Self {
        Self {
            symbol_table: SymbolTable::new(),
        }
    }
}

pub enum ASTValidationError {
    SymbolError(SymbolError),
}

pub struct TypeError(&'static str);

pub struct TypeChecker<'a> {
    symbol_table: &'a mut SymbolTable,
}

impl<'a> Visitor<Expression, PrimitiveType, TypeError> for TypeChecker<'a> {
    fn visit(&mut self, exp: &Expression) -> Result<PrimitiveType, TypeError> {
        let ty = match exp {
            Expression::UnaryOperator { op, expr } => {
                let ty = expr.visit_by(self)?;
                match op {
                    UnaryOperator::Neg => match ty {
                        PrimitiveType::Scalar(_) => ty,
                        PrimitiveType::Bool => {
                            return Err(TypeError("cant use neg operator on boolean type"))
                        }
                    },
                    UnaryOperator::Not => match ty {
                        PrimitiveType::Scalar(_) => {
                            return Err(TypeError("cant use not operator on non boolean type"))
                        }
                        PrimitiveType::Bool => ty,
                    },
                }
            }
            Expression::BinaryOperator { left, op, right } => {
                let left_ty = left.visit_by(self)?;
                let right_ty = right.visit_by(self)?;
                if left_ty != right_ty {
                    return Err(TypeError("type not same between binary operator"));
                }
                //
                todo!()
            }
            Expression::FunctionCall(_) => todo!(),
            Expression::ArrayAccess { array, index } => todo!(),
            Expression::ItemAccess { from, to } => todo!(),
            Expression::Assign { left, right } => right.visit_by(self)?,
            Expression::Number {} => todo!(),
            Expression::Bool(_) => PrimitiveType::Bool,
            Expression::Ident(_) => todo!(),
        };
        Ok(ty)
    }
}
