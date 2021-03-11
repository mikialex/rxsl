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

pub struct TypeChecker {}

impl Visitor<Expression, PrimitiveType, TypeError> for TypeChecker {
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
                match op {
                    BinaryOperator::Add => {}
                    BinaryOperator::Sub => {}
                    BinaryOperator::Mul => {}
                    BinaryOperator::Div => {}
                    BinaryOperator::Mod => {}
                    BinaryOperator::Less => {}
                    BinaryOperator::LessEqual => {}
                    BinaryOperator::Greater => {}
                    BinaryOperator::GreaterEqual => {}
                    BinaryOperator::Equal => {}
                    BinaryOperator::NotEqual => {}
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
