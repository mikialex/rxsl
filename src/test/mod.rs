use crate::{
    ast::{Expression, Statement},
    lexer::Lexer,
    parser::*,
};

// cargo test -- --nocapture
// can print log in unit test, but have some order issue
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
    parse("1+1+1");
    parse("1+(1)");
    parse("1+1*  3");
    parse("1+ -1*  - 3");
    parse("(1+ -1)*  (- 3 / 4)");
    parse("(1+ -1)*  (- test / ddd )");
    parse("(1+ -1)*  (- test(1, 2, 1/5, mu) / ddd )");
    parse("2 - 4 - 5");
    // parse("2-4-5"); fixme

    parse("test[1]");
    parse("test2[1]/2");
    parse("test2.ui");
    parse("test3[1][3].xyz");

    parse("a= b");
    parse("a= 2");
    parse("a= b = c= 2");
    parse("a= b = c= 2 + 1 * 4");
}

fn test_parse_statement(input: &str) -> Statement {
    let r = parse_statement(&mut Lexer::new(input)).unwrap();
    println!("{:?}", r);
    r
}

#[test]
fn parse_st_test() {
    test_parse_statement("return 1;");
    // test_parse_statement("{};"); // fix me
    // test_parse_statement("{;};"); // fix me
    test_parse_statement("a = 2; ");
    test_parse_statement("(1+ 1); ");
    test_parse_statement(
        "
    if 1+1 {
        if false {
            return test;
        }
    } elseif test2 {
        return 9;
    }  else {
        return x;
    }

    ",
    );
}
