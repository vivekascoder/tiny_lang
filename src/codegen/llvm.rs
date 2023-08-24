use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, fs, rc::Rc};

use crate::ast::{Expr, Function, Ident, Infix, Literal, Program, Statement, Type};
use anyhow::bail;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};
use log::info;

use super::CodeGen;

#[derive(Debug)]
struct LLVMCodeGen<'a, 'f> {
    ctx: &'a Context,
    program: Program,
    builder: Builder<'a>,
    module: Module<'a>,
    scopes: Rc<RefCell<Vec<HashMap<Rc<str>, (PointerValue<'a>, Type)>>>>,
    fns: Rc<RefCell<HashMap<Rc<str>, (FunctionValue<'a>, &'f Function)>>>,
}

// impl<'a> Into<AnyTypeEnum<'a>> for Type {
//     fn into(self) -> AnyTypeEnum<'a> {
//         match self {
//             Type::UnsignedInteger => AnyTypeEnum::IntType(In),
//         }
//     }
// }

/// # Main codegen part
/// https://llvm.org/docs/LangRef.html
///
/// ## How to store the let statements name, types of
impl<'a, 'f> LLVMCodeGen<'a, 'f> {
    pub fn new(ctx: &'a Context, program: Program, module_name: &str) -> Self {
        Self {
            ctx,
            program,
            builder: ctx.create_builder(),
            module: ctx.create_module(module_name),
            scopes: Rc::new(RefCell::new(vec![])),
            fns: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    // let a: usize = 535;
    // -> %a = alloca i32
    // -> store i32 535, i32* %a
    //
    // for nested infix:
    // let some: usize = 343 + a;
    // -> %some = alloca i32
    // -> sum
    // this function should return the name of the register that will have the value stored after doing all op.
    fn compile_expr(&self, expr: &Expr) -> BasicValueEnum {
        match &expr {
            Expr::Literal(l) => match l {
                Literal::UnsignedInteger(v) => self
                    .ctx
                    .i64_type()
                    .const_int(*v as u64, false)
                    .as_basic_value_enum(),
                Literal::Bool(v) => self
                    .ctx
                    .bool_type()
                    .const_int(*v as u64, false)
                    .as_basic_value_enum(),
                Literal::Char(c) => self
                    .ctx
                    .i8_type()
                    .const_int(*c as u64, false)
                    .as_basic_value_enum(),
            },
            Expr::Ident(i) => {
                // ## What to do?
                // - get the pointer from the scope,
                // - do a load and store the pointer to this with the ler.
                if let Some((ptr, ty_)) = self
                    .scopes
                    .as_ref()
                    .borrow()
                    .iter()
                    .rev()
                    .find_map(|s| s.get(&i.0))
                {
                    match ty_ {
                        Type::Bool => self.builder.build_load(self.ctx.bool_type(), *ptr, ""),
                        Type::Char => self.builder.build_load(self.ctx.i8_type(), *ptr, ""),
                        Type::UnsignedInteger => {
                            self.builder.build_load(self.ctx.i64_type(), *ptr, "")
                        }
                        Type::SignedInteger => {
                            self.builder.build_load(self.ctx.i64_type(), *ptr, "")
                        }
                    }
                } else {
                    panic!("ident doesn't exists.")
                }
            }

            Expr::Infix(i, l, r) => {
                let l_val = self.compile_expr(l);
                let r_val = self.compile_expr(r);
                info!("lval, rval: {:?}, {:?}", &l_val, &r_val);
                match (l_val, r_val) {
                    (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match i {
                        Infix::Plus => self.builder.build_int_add(l, r, "").as_basic_value_enum(),
                        Infix::Minus => self.builder.build_int_sub(l, r, "").as_basic_value_enum(),
                        Infix::Multiply => {
                            self.builder.build_int_mul(l, r, "").as_basic_value_enum()
                        }
                        Infix::Divide => self
                            .builder
                            .build_int_unsigned_div(l, r, "")
                            .as_basic_value_enum(),
                        _ => unimplemented!("plz implement other infix for int values."),
                    },
                    _ => unimplemented!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn compile_fn(&self, f: &'f Function) -> FunctionValue<'a> {
        self.scopes.as_ref().borrow_mut().push(HashMap::new());
        let params = f
            .params
            .iter()
            .map(|(_i, t)| match t {
                Type::UnsignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i64_type()),
                Type::SignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i64_type()),
                Type::Bool => BasicMetadataTypeEnum::IntType(self.ctx.bool_type()),
                Type::Char => BasicMetadataTypeEnum::IntType(self.ctx.i8_type()),
            })
            .collect::<Vec<_>>();
        let fn_type = match &f.return_type {
            Some(v) => match v {
                Type::UnsignedInteger => self.ctx.i64_type().fn_type(&params, false),
                Type::SignedInteger => self.ctx.i64_type().fn_type(&params, false),
                Type::Bool => self.ctx.i64_type().fn_type(&params, false),
                Type::Char => self.ctx.i8_type().fn_type(&params, false),
            },
            None => self.ctx.void_type().fn_type(&params, false),
        };

        let fn_val = self.module.add_function(&f.name, fn_type, None);
        self.fns
            .as_ref()
            .borrow_mut()
            .insert(Rc::clone(&f.name), (fn_val, f));

        // Append a main function block
        let main_block_name = format!("main_{}_block", f.name);
        let main_block = self
            .ctx
            .append_basic_block(fn_val, &main_block_name.as_str());
        self.builder.position_at_end(main_block);

        for stmt in &f.body {
            self.compile_statemt(stmt);
        }

        fn_val
    }

    fn compile_let(&self, i: &Ident, ty_: &Option<Type>, expr: &Expr) {
        let ptr = match ty_.clone().unwrap() {
            Type::UnsignedInteger => self.builder.build_alloca(self.ctx.i64_type(), ""),
            Type::SignedInteger => self.builder.build_alloca(self.ctx.i64_type(), ""),
            Type::Bool => self.builder.build_alloca(self.ctx.bool_type(), ""),
            Type::Char => self.builder.build_alloca(self.ctx.i8_type(), ""),
        };
        let val = self.compile_expr(expr);
        self.builder.build_store(ptr, val);

        let mut scopes = self.scopes.as_ref().borrow_mut();
        let c_scope = scopes.last_mut().unwrap();
        if c_scope.contains_key(&i.0) {
            // TODO: Move to result & bail
            panic!("already declared in this scope.")
        }
        // TODO: remove unwrap

        c_scope.insert(Rc::clone(&i.0), (ptr, ty_.clone().unwrap()));
    }

    fn compile_statemt(&self, stmt: &'f Statement) {
        match &stmt {
            Statement::Function(f) => {
                let _ = self.compile_fn(f);
            }
            Statement::Let(i, ty_, expr) => self.compile_let(i, ty_, expr),
            _ => panic!("not yet supported."),
        }
    }

    pub fn compile(&'f self) -> String {
        for stmt in &self.program {
            self.compile_statemt(stmt);
        }
        self.module.print_to_string().to_string()
    }
}

pub fn generate(module_name: &str, program: Program) -> anyhow::Result<()> {
    let context = Context::create();
    let llvm = LLVMCodeGen::new(&context, program, module_name);
    fs::write("./a.ll", llvm.compile())?;

    let res = std::process::Command::new("llc")
        .arg("./a.ll")
        .output()
        .expect("error while compiling using llc");
    if !res.status.success() {
        bail!("can't run llc {:?}", "./a.ll");
    }

    let res = std::process::Command::new("clang")
        .arg("./a.s")
        .output()
        .expect("error while running clang");

    if !res.status.success() {
        bail!("can't run clang {:?}", "./a.s");
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use env_logger::init;
    use inkwell::{
        context,
        passes::PassManagerSubType,
        types::{AsTypeRef, BasicMetadataTypeEnum, BasicType},
        values::{AsValueRef, BasicMetadataValueEnum, BasicValue, PointerValue},
        AddressSpace,
    };

    use crate::{
        ast::Token,
        lexer::{lexer::Lexer, logos_lexer::LexError},
        parser::Parser,
    };

    use super::*;

    #[test]
    fn test_expr_codegen<'a>() {
        let ctx = Context::create();
        let module = "something";
        let source = "23 + 34 - 5;";
        let program = Parser::new(module, source).parse().unwrap();
        println!("program: {:?}", &program);

        let llvm_codegen = LLVMCodeGen::new(&ctx, program, module);
        let s = llvm_codegen.compile();
        println!("{:?}", s);
    }

    #[test]
    fn test_codegen_function<'a>() {
        init();
        let ctx = Context::create();
        let module = "something";
        let source = r#"
        fun return_something(a: usize) => bool {
            let something: bool = false;
            let another_var: usize = 3535 + 35;
            let a: usize = another_var + 353;
            // return something;
        }
        "#;
        let ast = Parser::new(module, source).parse().unwrap();
        println!("{:?}", &ast);
        let llvm_codegen = LLVMCodeGen::new(&ctx, ast, module);
        println!("{}", llvm_codegen.compile());
    }

    #[test]
    fn test_llvm_infix_codegen() {
        let ctx = Context::create();
        let builder = ctx.create_builder();
        let module = ctx.create_module("test");

        let fn_val = module.add_function(
            "do_something",
            ctx.i64_type()
                .fn_type(&[BasicMetadataTypeEnum::IntType(ctx.i64_type())], false),
            None,
        );
        let main_block = ctx.append_basic_block(fn_val, "main_block");
        builder.position_at_end(main_block);
        // let l_val = ctx.i64_type().const_int(345, true);
        // let l_val = fn_val.get_nth_param(0).unwrap().into_int_value();

        // parse: let v: i64 = 234 + (23 - 345);
        let ptr = builder.build_alloca(ctx.i64_type(), "");
        let instr_val = builder.build_store(ptr, ctx.i64_type().const_int(345, false));
        let l_val = builder.build_load(ctx.i64_type(), ptr, "").into_int_value();

        let temp1 = builder.build_int_sub(ctx.i64_type().const_int(23, true), l_val, "");
        let v = builder.build_int_add(ctx.i64_type().const_int(234, true), temp1, "");
        builder.build_return(Some(&v));
        println!("{}", module.print_to_string().to_string());
    }

    #[test]
    fn test_inkwell_codegen() {
        let context = Context::create();
        let module = context.create_module("example");
        let builder = context.create_builder();

        let fn_val = module.add_function(
            "do_something",
            context.i64_type().fn_type(
                &[
                    BasicMetadataTypeEnum::IntType(context.i64_type()),
                    BasicMetadataTypeEnum::IntType(context.i64_type()),
                ],
                false,
            ),
            None,
        );
        let fn_params = fn_val.get_params();
        let block_1 = context.append_basic_block(fn_val, "blk1");
        builder.position_at_end(block_1);
        let sum = builder.build_int_add(
            fn_params[0].into_int_value(),
            fn_params[1].into_int_value(),
            "sum",
        );
        builder.build_return(Some(&sum));

        // let's do main function,
        let main_fn_val = module.add_function("main", context.i64_type().fn_type(&[], false), None);
        let main_block = context.append_basic_block(main_fn_val, "main_call");
        builder.position_at_end(main_block);

        // Try calling the function.
        // Having a `puts` function
        let puts_fn_type = context.i32_type().fn_type(
            &[BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(1u16)),
            )],
            false,
        );

        let hello_string = context.const_string(b"Hello, World!\n", false);
        let puts_fn = module.add_function("puts", puts_fn_type, None);

        builder.build_call(
            fn_val,
            &[
                BasicMetadataValueEnum::IntValue(context.i64_type().const_int(23, false)),
                BasicMetadataValueEnum::IntValue(context.i64_type().const_int(46, false)),
            ],
            "do_something",
        );
        // unsafe {
        //     builder.build_call(
        //         puts_fn,
        //         &[BasicMetadataValueEnum::PointerValue(PointerValue::new(
        //             hello_string.as_,
        //         ))],
        //         "",
        //     );
        // }
        builder.build_return(Some(&context.i64_type().const_int(0, false)));

        module.verify().expect("module verification error.");
        module.print_to_file("./build/something.ll").unwrap();
        println!("LLVM IR:\n{}", module.print_to_string().to_string());
    }

    #[test]
    fn test_cranelift_codegen() {}
}
