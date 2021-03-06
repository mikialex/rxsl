use crate::*;

// cargo test -- --nocapture
// can print log in unit test, but have some order issue
pub fn parse(input: &str) -> Expression {
    let r = Expression::parse(&mut Lexer::new(input)).unwrap();
    println!("{:#?}", r);
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
    parse(" 1 < 2");
    parse(" 1 < 2 == 2");

    parse(" false && false");
    parse(" 1 & 2 == 1 || false ");

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
    let r = Statement::parse(&mut Lexer::new(input)).unwrap();
    println!("{:#?}", r);
    r
}

#[test]
fn parse_st_test() {
    test_parse_statement("return 1;");
    test_parse_statement("{}");
    test_parse_statement("{;}"); // fix me
    test_parse_statement("a = 2; ");
    test_parse_statement("(1+ 1); ");
    test_parse_statement("let a = (1+ 1); ");
    test_parse_statement("const a = b = (1+ 1); ");
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

    test_parse_statement(
        "
        for let i = 0; false; false   {
            print();
        }
    ",
    );
}

#[test]
fn gen_ir_test() {
    let input = r#"
{
    let a = 1;
    let b = false;
    let c = 2;
    if b {
        a = 2;
        c = a;
        let d = a + c;
    } else {
        a = 3;
    }

    let d = true;

    if d || b {
         a = a * c + a * a;
    }

    while a < 10 {
        a = a + 1;
    }
    
}
    "#;

    let ast = Block::parse(&mut Lexer::new(input)).unwrap();
    let ins = IRGenerator::generate(&ast).unwrap();
    println!("{}", ins)
}

const test_component: &str = r#"
pub struct Fog {
    range: vec2,
    color: vec2
}


"#;
