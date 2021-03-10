use crate::ast::{Block, Expression, FunctionCall, FunctionDefine, Ident, Statement};

pub trait Visitor<T, E> {
    fn visit(&mut self, _item: &T) -> Result<NextVisit, E>;
}

pub enum NextVisit {
    Continue,
    SkipChildren,
}

impl<X, T, E> Visitor<X, E> for T {
    default fn visit(&mut self, _item: &X) -> Result<NextVisit, E> {
        Ok(NextVisit::Continue)
    }
}

pub trait SyntaxTreeVisitable<T, E>: Sized {
    fn visit_by(&self, visitor: &mut T) -> Result<(), E> {
        match visitor.visit(self)? {
            NextVisit::Continue => self.visit_children_by(visitor),
            NextVisit::SkipChildren => Ok(()),
        }
    }
    fn visit_children_by(&self, _visitor: &mut T) -> Result<(), E> {
        Ok(())
    }
}

impl<T, E> SyntaxTreeVisitable<T, E> for FunctionDefine {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        self.name.visit_by(visitor)?;
        // self.arguments.iter().for_each(|ar| ar);
        self.body.visit_by(visitor)
    }
}

impl<T, E> SyntaxTreeVisitable<T, E> for Ident {}
impl<T, E> SyntaxTreeVisitable<T, E> for Statement {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> SyntaxTreeVisitable<T, E> for Block {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> SyntaxTreeVisitable<T, E> for FunctionCall {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> SyntaxTreeVisitable<T, E> for Expression {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        match self {
            Expression::UnaryOperator { expr, .. } => expr.visit_by(visitor),
            Expression::BinaryOperator { left, right, .. } => {
                left.visit_by(visitor)?;
                right.visit_by(visitor)
            }
            Expression::FunctionCall(f) => f.visit_by(visitor),
            Expression::ArrayAccess { array, index } => {
                array.visit_by(visitor)?;
                index.visit_by(visitor)
            }
            Expression::ItemAccess { from, to } => {
                from.visit_by(visitor)?;
                to.visit_by(visitor)
            }
            Expression::Assign { left, right } => {
                left.visit_by(visitor)?;
                right.visit_by(visitor)
            }
            Expression::Number {} => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::Ident(_) => todo!(),
        }
    }
}
