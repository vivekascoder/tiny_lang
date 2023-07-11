use crate::ast::*;
use crate::{interpreter::Interpreter, parser::Parser};

#[test]

fn test_is_type_expr_result_same() {
    assert_eq!(
        Interpreter::is_type_expr_result_same(None, &ExprResult::Void),
        true
    );
    assert_eq!(
        Interpreter::is_type_expr_result_same(None, &ExprResult::Bool(false)),
        false
    );

    assert_eq!(
        Interpreter::is_type_expr_result_same(Some(Type::Bool), &ExprResult::Bool(false)),
        true
    );

    assert_eq!(
        Interpreter::is_type_expr_result_same(None, &ExprResult::UnsignedInteger(465)),
        false
    );
}

#[test]
fn does_interpreter_starts() {
    let source = r#"
        let a = 454 + 3636 * 3; 
        let b = 45 / 3;

        fun something(a: usize) => usize {
            let something = 353;
            return something;
        }

        let re = something(35);
        "#;

    let mut i = Interpreter::new(source);
    println!("parsed: {:#?}", Parser::new(source).parse());
    i.eval().unwrap();
}
