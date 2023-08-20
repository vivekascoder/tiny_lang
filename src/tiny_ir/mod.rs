use std::rc::Rc;

use crate::ast::{Program, Type};

#[derive(Debug, Clone)]
enum Cond {
    Eq,
    Ne,
    Gt,
    Gte,
    Lt,
    Lte,
}

#[derive(Debug, Clone)]
enum Instr {
    // # memory access
    //%p = load i32, i32* %v
    Load {
        name: Rc<str>,
        ty_: Type,
        reg_to_load: Rc<str>,
    },
    Store(Rc<str>),        // store i32 34, i32* %v
    Alloca(Rc<str>, Type), // %v = alloca i32

    // binary op.
    Add(i64, i64),
    Sub(i64, i64),
    Mul(i64, i64),
    Div(i64, i64),
    Rem(i64, i64),

    // terminator.
    Ret(i64),

    // comparison
    Icmp(Cond, i64, i64),
}

struct Block {
    instrs: Vec<Instr>,
}

struct IrFunction {
    blocks: Vec<Block>,
    r_ty: Type,
    // params:
}

struct TinyIR {
    program: Program,
}

impl TinyIR {
    fn new(program: Program) -> Self {
        Self { program }
    }

    fn ast_to_tiny_ir(&self) -> IrFunction {
        todo!()
    }
}
