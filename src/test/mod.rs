use crate::{ast::Expression, lexer::Lexer, parser::parse_expression};

pub fn parse(input: &str) -> Expression {
    parse_expression(&mut Lexer::new(input)).unwrap()
}

#[test]
fn parse_expression_test() {
    parse("1");
    parse("1+1");
    parse("1+(1)");
    parse("1+1*  3");
    parse("1+ -1*  - 3");
    parse("(1+ -1)*  (- 3 / 4)");
}
