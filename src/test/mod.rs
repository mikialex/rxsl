use crate::{ast::Expression, lexer::Lexer, parser::parse_expression};

// "cargo test -- --nocapture" can print log in unit test
pub fn parse(input: &str) -> Expression {
    let r = parse_expression(&mut Lexer::new(input)).unwrap();
    println!("{:?}", r);
    r
}

#[test]
fn parse_expression_test() {
    parse("1");
    parse("true");
    parse("!(true)");
    parse("1+1");
    parse("1+(1)");
    parse("1+1*  3");
    parse("1+ -1*  - 3");
    parse("(1+ -1)*  (- 3 / 4)");
    parse("(1+ -1)*  (- test / ddd )");
    parse("(1+ -1)*  (- test(1, 2, 1/5, mu) / ddd )");
}
