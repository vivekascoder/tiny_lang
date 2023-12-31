use crate::ast::*;
use crate::lexer::lexer::Lexer;
use crate::tests::setup;

#[test]
fn test_lexing_let_syntax() {
    let source = "let some_var: usize = 345 + 35353;";
    insta::assert_debug_snapshot!(Lexer::new("", source).into_iter().collect::<Vec<Token>>());
}

#[test]
fn test_lexing_char() {
    setup();
    let source = "'\\n'";
    insta::assert_debug_snapshot!(Lexer::new("", source).into_iter().collect::<Vec<Token>>());
}

#[test]
fn test_lexing_string() {
    setup();
    let source = r#""str";"#;
    insta::assert_debug_snapshot!(Lexer::new("", source).into_iter().collect::<Vec<Token>>());
}

#[test]
fn test_function_declaration() {
    setup();
    let source = r#"
        fun calculate_something(a: usize, b: usize) => bool {
            if (a > b) {
                return true;
            } else {
                return false;
            }
        }
        "#;
    insta::assert_debug_snapshot!(Lexer::new("", source).into_iter().collect::<Vec<Token>>());
}

#[test]
fn test_function_decl_and_call() {
    let code = r#"
    let a = 445;
    let b = 45;


    fun sum(a: usize, b: usize) => usize {
        let c = a + b;
        return c;
    }

    sum(a, b);
    "#;
    insta::assert_debug_snapshot!(Lexer::new("", code).into_iter().collect::<Vec<Token>>());
}

#[test]
fn test_lex_bitwise_operations() {
    let code = r#"
    let a = 45 | 45;
    let b = 45 ^ 3;
    let c = 34 & 2;
    let d = 34 << 1;
    let e = 1 >> 34;
    "#;
    insta::assert_debug_snapshot!(Lexer::new("", code).into_iter().collect::<Vec<Token>>());
}
