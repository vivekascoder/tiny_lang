use crate::lexer::logos_lexer::TokenKind;
use logos::Logos;

#[test]
fn test_logos_lexer() {
    let mut lex = TokenKind::lexer("let =");

    loop {
        if let Some(v) = lex.next() {
            println!("> {:?}, {:?}", v, lex.span());
        } else {
            break;
        }
    }
}

#[test]
fn test_function_decl_and_call_logos() {
    let code = r#"
    let a = 445;
    let b = 45;
    let bool_var = false;
    let string_val = "this is a \"string";
    let char_val = '\n';


    fun sum(a: usize, b: usize) => usize {
        let c = a + b;
        return c;
    }

    sum(a, b);
    "#;
    insta::assert_debug_snapshot!(TokenKind::lexer(code).collect::<Vec<_>>());
}
