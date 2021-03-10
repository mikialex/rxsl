pub struct ASTValidator {}

impl Visitor<FunctionDefine> for TestVisitor {
    fn visit(&mut self, _item: &FunctionDefine) {}
}
