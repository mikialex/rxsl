use crate::ast::{FunctionDefine, Ident};

pub trait Visitor<T> {
    fn visit(&mut self, _item: &T) {}
}

pub trait SyntaxTreeVisitable<T> {
    fn visit_children(&self, visitor: &mut T);
}

impl<T: Visitor<Ident>> SyntaxTreeVisitable<T> for FunctionDefine {
    fn visit_children(&self, visitor: &mut T) {
        visitor.visit(&self.name)
    }
}
