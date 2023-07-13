use log::info;

use crate::ast::*;
use crate::tests::setup;
use crate::{interpreter::Interpreter, parser::Parser};

#[test]

fn test_is_type_expr_result_same() {
    setup();
    assert_eq!(
        Interpreter::is_type_expr_result_same(&None, &ExprResult::Void),
        true
    );
    assert_eq!(
        Interpreter::is_type_expr_result_same(&None, &ExprResult::Bool(false)),
        false
    );

    assert_eq!(
        Interpreter::is_type_expr_result_same(&Some(Type::Bool), &ExprResult::Bool(false)),
        true
    );

    assert_eq!(
        Interpreter::is_type_expr_result_same(&None, &ExprResult::UnsignedInteger(465)),
        false
    );
    assert_eq!(
        Interpreter::is_type_expr_result_same(
            &Some(Type::UnsignedInteger),
            &ExprResult::UnsignedInteger(465)
        ),
        true
    );
    assert_eq!(
        Interpreter::is_type_expr_result_same(
            &Some(Type::UnsignedInteger),
            &ExprResult::Return(Box::new(ExprResult::UnsignedInteger(345)))
        ),
        true
    )
}

#[test]
fn does_interpreter_starts() {
    setup();
    let source = r#"
        let a = 454 + 3636 * 3; 
        let b = 45 / 3;

        fun something(a: usize) => usize {
            let something = 353;
            return something;
        }

        let re = something(35);
        print(re);
        "#;

    let mut i = Interpreter::new("", source);
    info!("parsed: {:#?}", Parser::new("", source).parse());
    i.eval().unwrap();
}

#[test]
fn does_if_eval_works() {
    setup();
    let code = r#"
    let a = 445;
    let b = 45;


    fun sum(a: usize, b: usize) => usize {
        let c = 0;
        if (a != 0) {
            print(c);
            let c = a + b;
            return c;
        } else {
            return b;
        }
    }

    print(sum(a, b));
    "#;
    let mut i = Interpreter::new("", code);
    i.eval().unwrap();
}

#[test]
fn does_assignment_works() {
    setup();
    let code = r#"
    let a = 454;
    a = 0;
    print(a);
    "#;
    let mut i = Interpreter::new("", code);
    insta::assert_debug_snapshot!(i.eval().unwrap());
}
