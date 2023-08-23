use std::rc::Rc;

use crate::ast::{Literal, Program, Type};

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
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone)]
enum Instr {
    //%p = load i32, i32* %v
    Load {
        name: Rc<str>,
        ty_: Type,
        reg_to_load: Rc<str>,
    },

    // store i32 34, i32* %v
    Store {
        ty_: Type,
        val: Literal,
        store_at: Rc<str>,
    },

    // %v = alloca i32
    Alloca {
        name: Rc<str>,
        ty_: Type,
    },

    BinaryInstr {
        op: BinOp,
        ty_: Type,
        lr: (Literal, Literal),
    },

    // terminator.
    Ret(i64),

    // comparison: %c = icmp sgt i32 %a, %b
    Icmp {
        name: Rc<str>,
        cond: Cond,
        ty_: Type,
        lr: (Literal, Literal),
    },
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
