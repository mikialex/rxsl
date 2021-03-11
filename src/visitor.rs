use crate::ast::{
    Block, Expression, FunctionCall, FunctionDefine, Ident, Statement, TypeExpression,
};

pub trait Visitor<T, R, E> {
    fn visit(&mut self, item: &T) -> Result<R, E>;
}

impl<X, T, E> Visitor<X, (), E> for T {
    default fn visit(&mut self, _item: &X) -> Result<(), E> {
        Ok(())
    }
}

pub trait SyntaxTreeVisitable<T: Visitor<Self, R, E>, R, E>: Sized {
    fn visit_by(&self, visitor: &mut T) -> Result<R, E> {
        visitor.visit(self)
    }
}

impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for FunctionDefine {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for TypeExpression {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for Ident {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for Statement {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for Block {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for FunctionCall {}
impl<T: Visitor<Self, R, E>, R, E> SyntaxTreeVisitable<T, R, E> for Expression {}

////////

pub enum NextVisit {
    Continue,
    SkipChildren,
}

pub struct ASTTreeTraverseVisitor<T> {
    visitor: T,
}
pub trait ASTTreeTraverseVisitorTrait<T, E> {
    fn traverse_visit(&mut self, _item: &T) -> Result<NextVisit, E>;
}

impl<X, T, E> ASTTreeTraverseVisitorTrait<X, E> for T {
    default fn traverse_visit(&mut self, _item: &X) -> Result<NextVisit, E> {
        Ok(NextVisit::Continue)
    }
}

pub trait ASTTreeTraverseVisitable<T, E>: Sized {
    fn traverse_visit_by(&self, visitor: &mut T) -> Result<(), E> {
        match visitor.traverse_visit(self)? {
            NextVisit::Continue => self.visit_children_by(visitor),
            NextVisit::SkipChildren => Ok(()),
        }
    }
    fn visit_children_by(&self, _visitor: &mut T) -> Result<(), E> {
        Ok(())
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for FunctionDefine {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        self.name.traverse_visit_by(visitor)?;
        // self.arguments.iter().for_each(|ar| ar);
        self.body.traverse_visit_by(visitor)
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for TypeExpression {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        match self {
            TypeExpression::Named(ident) => ident.traverse_visit_by(visitor),
        }
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for Ident {}
impl<T, E> ASTTreeTraverseVisitable<T, E> for Statement {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for Block {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for FunctionCall {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        todo!()
    }
}

impl<T, E> ASTTreeTraverseVisitable<T, E> for Expression {
    fn visit_children_by(&self, visitor: &mut T) -> Result<(), E> {
        match self {
            Expression::UnaryOperator { expr, .. } => expr.traverse_visit_by(visitor),
            Expression::BinaryOperator { left, right, .. } => {
                left.traverse_visit_by(visitor)?;
                right.traverse_visit_by(visitor)
            }
            Expression::FunctionCall(f) => f.traverse_visit_by(visitor),
            Expression::ArrayAccess { array, index } => {
                array.traverse_visit_by(visitor)?;
                index.traverse_visit_by(visitor)
            }
            Expression::ItemAccess { from, to } => {
                from.traverse_visit_by(visitor)?;
                to.traverse_visit_by(visitor)
            }
            Expression::Assign { left, right } => {
                left.traverse_visit_by(visitor)?;
                right.traverse_visit_by(visitor)
            }
            Expression::Number {} => todo!(),
            Expression::Bool(_) => todo!(),
            Expression::Ident(_) => todo!(),
        }
    }
}
