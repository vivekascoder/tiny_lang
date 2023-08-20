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
    // memory access
    Load(Rc<str>),
    Store(Rc<str>),

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
