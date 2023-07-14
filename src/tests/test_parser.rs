use log::info;

use crate::{parser::Parser, tests::setup};

#[test]
fn test_let_statement_parsing() {
    let code = "let something = 454 + 3 * 4 - 35; ";
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_multiple_let_statement_parsing() {
    let code = r#"
        let something = 454 + 3 * 4 - 35;
        let a = 35 / 3;
        "#;
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_char_parsing() {
    let code = "'a';";
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_char_parsing_dual() {
    let code = "'\\n' > 'n';";
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_prefix_expression_parsing() {
    let code = "let val = +454;";
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_function_parsing() {
    let code = r#"
        fun return_something(a: usize) => bool {
            let something = false;
            let another_var = 3535 + 35;
            return something;
        }
        "#;

    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_if_statement_parsing() {
    let code = r#"
        if (54 > 34) {
            let something = 446;
        } else {
            let somethine = 0;
        }
        "#;
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_function_call_parsing() {
    let code = r#"
        function_call(35 + 3435, some_var);
        "#;
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_nested_function_call_parsing() {
    let code = r#"
        function_call(35 + 3435, another_func(34535, 355));
        "#;
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_expression_statement_infix() {
    let code = r#"
        5456 + 33563 + 34;
        "#;
    insta::assert_debug_snapshot!(Parser::new("", code).parse());
}

#[test]
fn test_function_and_call() {
    setup();
    let code = r#"
    let a = 445;
    let b = 45;


    fun sum(a: usize, b: usize) => usize {
        if (a != 0) {
            let c = a + b;
            return c;
        } else {
            return b;
        }
    }

    sum(a, b);
    "#;
    let ast = Parser::new("", code).parse();
    info!("AST generated for the program: {:#?}", &ast);
    insta::assert_debug_snapshot!(&ast);
}

#[test]
fn test_while_and_assignement() {
    setup();
    let code = r#"
    /**
    * Demostration of while loop in tiny lang.
    **/

    let i = 0;
    while (i < 5) {
        print(i);
        print('\n');
        i = i + 1;
    }
    "#;
    let ast = Parser::new("", code).parse();
    info!("AST generated for the program: {:#?}", &ast);
    insta::assert_debug_snapshot!(&ast);
}

#[test]
fn test_assignement_parsing() {
    setup();
    let code = r#"
    let a = 454;
    a = 0;
    print(a);
    "#;
    let ast = Parser::new("", code).parse();
    info!("AST generated for the program: {:#?}", &ast);
    insta::assert_debug_snapshot!(&ast);
}

#[test]
fn test_bitwise_shift_parsing() {
    setup();
    let code = r#"
    let a = 5 << 1;
    "#;
    let ast = Parser::new("", code).parse();
    info!("AST generated for the program: {:#?}", &ast);
    insta::assert_debug_snapshot!(&ast);
}
