use super::CodeGen;
use crate::ast::Condition;
use crate::ast::{Expr, Function, Ident, Infix, Literal, Program, Statement, Type};
use anyhow::bail;
use anyhow::Result;
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::IntValue;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyTypeEnum, BasicMetadataTypeEnum},
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};
use inkwell::{AddressSpace, IntPredicate};
use log::info;
use std::borrow::BorrowMut;
use std::{cell::RefCell, collections::HashMap, fs, rc::Rc};

#[derive(Debug)]
struct Value<'a> {
    ty: Type,
    val: BasicValueEnum<'a>,
}

impl<'a> Value<'a> {
    fn new(ty: Type, val: BasicValueEnum<'a>) -> Self {
        Self { ty, val }
    }

    pub fn ty(&self) -> Type {
        self.ty
    }

    pub fn val(&self) -> BasicValueEnum<'a> {
        self.val
    }

    pub fn assert_if_not(ty: &Type, expected_ty: &Type) -> Result<()> {
        if !ty.eq(&expected_ty) {
            bail!(
                "type mismatch: expected {:?} but got {:?}.",
                expected_ty,
                ty
            );
        }
        Ok(())
    }
}

#[derive(Debug)]
struct LLVMCodeGen<'ctx, 'f> {
    ctx: &'ctx Context,
    program: Program,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    scopes: Rc<RefCell<Vec<HashMap<Rc<str>, (PointerValue<'ctx>, Type)>>>>,
    fns: Rc<RefCell<HashMap<Rc<str>, (FunctionValue<'ctx>, &'f Function)>>>,
    extern_fns: Rc<RefCell<HashMap<Rc<str>, FunctionValue<'ctx>>>>,
    current_fn: Rc<RefCell<Option<(FunctionValue<'ctx>, BasicBlock<'ctx>)>>>,
}

// impl<'ctx> Into<AnyTypeEnum<'ctx>> for Type {
//     fn into(self) -> AnyTypeEnum<'ctx> {
//         match self {
//             Type::UnsignedInteger => AnyTypeEnum::IntType(In),
//         }
//     }
// }

/// # Main codegen part
/// https://llvm.org/docs/LangRef.html
///
/// ## How to store the let statements name, types of
impl<'ctx, 'f> LLVMCodeGen<'ctx, 'f> {
    pub fn new(ctx: &'ctx Context, program: Program, module_name: &str) -> Self {
        let ll = Self {
            ctx,
            program,
            builder: ctx.create_builder(),
            module: ctx.create_module(module_name),
            scopes: Rc::new(RefCell::new(vec![])),
            fns: Rc::new(RefCell::new(HashMap::new())),
            extern_fns: Rc::new(RefCell::new(HashMap::new())),
            current_fn: Rc::new(RefCell::new(None)),
        };
        ll.init_builtins();
        ll
    }

    fn i8_llvm_str(&self, v: &str) -> Vec<IntValue> {
        v.bytes()
            .into_iter()
            .map(|v| self.ctx.i8_type().const_int(v as u64, false))
            .collect::<Vec<IntValue>>()
    }

    fn init_builtins(&self) {
        let printftp = self.ctx.i32_type().fn_type(
            &[self.ctx.i8_type().ptr_type(AddressSpace::from(0)).into()],
            false,
        );
        let printf =
            self.module
                .add_function("printf", printftp, Some(inkwell::module::Linkage::External));

        self.extern_fns
            .as_ref()
            .borrow_mut()
            .insert("printf".into(), printf);
    }

    fn compile_infix_expr(&self, i: &Infix, l: &Expr, r: &Expr) -> Result<Value<'ctx>> {
        let l_val = self.compile_expr(l)?;
        let r_val = self.compile_expr(r)?;
        info!("lval, rval: {:?}, {:?}", &l_val.val(), &r_val.val());

        if !l_val.ty().eq(&r_val.ty()) {
            bail!("type mismatch.");
        }

        match (l_val.val(), r_val.val()) {
            // FIXME: `IntValue` can represent i64, u64, char, bool etc.
            // TODO: do `icmp` when encountering comparison operators.
            (BasicValueEnum::IntValue(l), BasicValueEnum::IntValue(r)) => match i {
                Infix::Plus => Ok(Value::new(
                    l_val.ty(),
                    self.builder.build_int_add(l, r, "").as_basic_value_enum(),
                )),
                Infix::Minus => Ok(Value::new(
                    l_val.ty(),
                    self.builder.build_int_sub(l, r, "").as_basic_value_enum(),
                )),
                Infix::Multiply => Ok(Value::new(
                    l_val.ty(),
                    self.builder.build_int_mul(l, r, "").as_basic_value_enum(),
                )),
                Infix::Divide => Ok(Value::new(
                    l_val.ty(),
                    self.builder
                        .build_int_unsigned_div(l, r, "")
                        .as_basic_value_enum(),
                )),
                Infix::GreaterThan => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::SGT, l, r, "")
                        .as_basic_value_enum(),
                )),
                _ => unimplemented!("plz implement other infix for int values."),
            },
            _ => unimplemented!(),
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
    fn compile_expr(&self, expr: &Expr) -> Result<Value<'ctx>> {
        match &expr {
            Expr::Literal(l) => match l {
                Literal::UnsignedInteger(v) => Ok(Value::new(
                    Type::UnsignedInteger,
                    self.ctx
                        .i64_type()
                        .const_int(*v as u64, false)
                        .as_basic_value_enum(),
                )),
                Literal::Bool(v) => Ok(Value::new(
                    Type::Bool,
                    self.ctx
                        .bool_type()
                        .const_int(*v as u64, false)
                        .as_basic_value_enum(),
                )),
                Literal::Char(v) => Ok(Value::new(
                    Type::Char,
                    self.ctx
                        .i8_type()
                        .const_int(*v as u64, false)
                        .as_basic_value_enum(),
                )),
                Literal::String(s) => {
                    let arr = s
                        .as_ref()
                        .bytes()
                        .into_iter()
                        .map(|v| self.ctx.i8_type().const_int(v as u64, false))
                        .collect::<Vec<IntValue>>();
                    Ok(Value::new(
                        Type::String,
                        self.ctx
                            .i8_type()
                            .const_array(arr.as_slice())
                            .as_basic_value_enum(),
                    ))
                }
            },
            Expr::Ident(i) => {
                if let Some((ptr, ty_)) = self
                    .scopes
                    .as_ref()
                    .borrow()
                    .iter()
                    .rev()
                    .find_map(|s| s.get(&i.0))
                {
                    match ty_ {
                        Type::Bool => Ok(Value::new(
                            *ty_,
                            self.builder.build_load(self.ctx.bool_type(), *ptr, ""),
                        )),
                        Type::Char => Ok(Value::new(
                            *ty_,
                            self.builder.build_load(self.ctx.i8_type(), *ptr, ""),
                        )),
                        Type::UnsignedInteger => Ok(Value::new(
                            *ty_,
                            self.builder.build_load(self.ctx.i64_type(), *ptr, ""),
                        )),
                        Type::SignedInteger => Ok(Value::new(
                            *ty_,
                            self.builder.build_load(self.ctx.i64_type(), *ptr, ""),
                        )),
                        Type::String => Ok(Value::new(
                            *ty_,
                            self.builder.build_load(
                                self.ctx.i8_type().ptr_type(AddressSpace::default()),
                                *ptr,
                                "",
                            ),
                        )),
                    }
                } else {
                    panic!("ident doesn't exists.")
                }
            }

            Expr::Infix(i, l, r) => Ok(self.compile_infix_expr(i, l, r)?),

            Expr::Call(fn_call) => {
                let llvm_type_args = fn_call
                    .parameters
                    .iter()
                    .map(|a| self.compile_expr(a).unwrap().val().into())
                    .collect::<Vec<BasicMetadataValueEnum>>();
                if let Some(v) = self.fns.as_ref().borrow().get(fn_call.name.as_ref()) {
                    match self
                        .builder
                        .build_call(v.0, &llvm_type_args, "")
                        .try_as_basic_value()
                    {
                        Either::Left(val) => Ok(Value::new(v.1.return_type.unwrap(), val)),
                        Either::Right(v) => {
                            bail!("got right");
                        }
                    }
                } else {
                    if let Some(f) = self.extern_fns.as_ref().borrow().get(fn_call.name.as_ref()) {
                        // let gep = self.builder.build_gep(self.ctx.i8_type().array_type(), ptr, ordered_indexes, name)
                        self.builder.build_call(
                            *f,
                            &[
                                self.ctx
                                    .i8_type()
                                    .const_array(&self.i8_llvm_str("%d"))
                                    .into(),
                                *llvm_type_args.first().unwrap(),
                            ],
                            "",
                        );
                        // Ok(self.ctx.i8_type().const_int(0, false).into())
                        unimplemented!("plz implement extern fn call.")
                    } else {
                        bail!("function isn't declared yet.");
                    }
                }
            }
            Expr::Prefix(p, expr) => {
                unimplemented!("plz implement prefix expr compilation ser.")
            }
            _ => unimplemented!(),
        }
    }

    fn compile_fn(&self, f: &'f Function) -> Result<FunctionValue<'ctx>> {
        self.scopes.as_ref().borrow_mut().push(HashMap::new());
        let params = f
            .params
            .iter()
            .map(|(_i, t)| match t {
                Type::UnsignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i64_type()),
                Type::SignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i64_type()),
                Type::Bool => BasicMetadataTypeEnum::IntType(self.ctx.bool_type()),
                Type::Char => BasicMetadataTypeEnum::IntType(self.ctx.i8_type()),
                Type::String => BasicMetadataTypeEnum::PointerType(
                    self.ctx.i8_type().ptr_type(AddressSpace::default()),
                ),
            })
            .collect::<Vec<_>>();
        let fn_type = match &f.return_type {
            Some(v) => match v {
                Type::UnsignedInteger => self.ctx.i64_type().fn_type(&params, false),
                Type::SignedInteger => self.ctx.i64_type().fn_type(&params, false),
                Type::Bool => self.ctx.i64_type().fn_type(&params, false),
                Type::Char => self.ctx.i8_type().fn_type(&params, false),
                Type::String => self
                    .ctx
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&params, false),
            },
            None => self.ctx.void_type().fn_type(&params, false),
        };

        let fn_val = self.module.add_function(&f.name, fn_type, None);

        // Append a main function block
        let main_block_name = format!("main_{}_block", f.name);
        let main_block = self
            .ctx
            .append_basic_block(fn_val, &main_block_name.as_str());
        self.builder.position_at_end(main_block);
        *self.current_fn.as_ref().borrow_mut() = Some((fn_val, main_block));

        for stmt in &f.body {
            self.compile_stmt(stmt)?;
        }

        Ok(fn_val)
    }

    fn compile_return(&self, expr: &Expr) -> Result<()> {
        self.builder
            .build_return(Some(&self.compile_expr(expr)?.val()));
        Ok(())
    }

    // fn compile_call

    fn compile_let(&self, i: &Ident, ty_: &Option<Type>, expr: &Expr) -> Result<()> {
        let val = self.compile_expr(expr)?;
        let ptr = match ty_.clone().unwrap() {
            Type::UnsignedInteger => self.builder.build_alloca(self.ctx.i64_type(), ""),
            Type::SignedInteger => self.builder.build_alloca(self.ctx.i64_type(), ""),
            Type::Bool => self.builder.build_alloca(self.ctx.bool_type(), ""),
            Type::Char => self.builder.build_alloca(self.ctx.i8_type(), ""),
            Type::String => {
                if let BasicValueEnum::ArrayValue(av) = val.val() {
                    self.builder.build_alloca(av.get_type(), "")
                } else {
                    bail!("Type string with not `ArrayValue`");
                }
            }
        };
        self.builder.build_store(ptr, val.val());

        let mut scopes = self.scopes.as_ref().borrow_mut();
        let c_scope = scopes.last_mut().unwrap();
        if c_scope.contains_key(&i.0) {
            // TODO: Move to result & bail
            bail!("already declared in this scope.")
        }
        // TODO: remove unwrap

        c_scope.insert(Rc::clone(&i.0), (ptr, ty_.clone().unwrap()));
        Ok(())
    }

    fn compile_condition(&self, cond: &'f Condition) -> Result<()> {
        let condition = self.compile_expr(&cond.condition)?;

        Value::assert_if_not(&Type::Bool, &condition.ty())?;

        let current_fn = self.current_fn.as_ref().borrow().unwrap();
        let current_fn_block = current_fn.1.clone();
        let if_block = self.ctx.append_basic_block(current_fn.0, "");
        let mut else_block: Option<BasicBlock<'ctx>> = None;
        let new_block = self.ctx.append_basic_block(current_fn.0, "");

        if cond.else_body.is_none() {
            self.builder.build_conditional_branch(
                condition.val().into_int_value(),
                if_block,
                new_block,
            );
        } else {
            else_block = Some(self.ctx.append_basic_block(current_fn.0, ""));
            self.builder.build_conditional_branch(
                condition.val().into_int_value(),
                if_block,
                else_block.unwrap(),
            );
        }

        // compile if block
        self.builder.position_at_end(if_block);
        // (*self.current_fn.as_ptr().borrow_mut()).1 = if_block;
        self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = if_block;
        // FIXME: Above to support nested if-else.
        for stmt in &cond.if_body {
            self.compile_stmt(stmt)?;
        }

        match cond.if_body.last().unwrap() {
            Statement::Return(_) => {}
            _ => {
                self.builder.build_unconditional_branch(new_block);
            }
        };

        // create new block for remaining
        self.builder.position_at_end(new_block);

        // compile else block
        if cond.else_body.is_some() {
            self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = else_block.unwrap();
            self.builder.position_at_end(else_block.unwrap());
            for stmt in cond.else_body.as_ref().unwrap() {
                self.compile_stmt(stmt)?;
            }
            self.builder.position_at_end(new_block);
            self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = current_fn_block;
        }

        self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = current_fn_block;

        Ok(())
    }

    fn compile_stmt(&self, stmt: &'f Statement) -> Result<()> {
        info!("COMPILER: compiling {:?}", &stmt);
        match &stmt {
            Statement::Function(f) => {
                let fn_val = self.compile_fn(f)?;
                self.fns
                    .as_ref()
                    .borrow_mut()
                    .insert(Rc::clone(&f.name), (fn_val, f));

                Ok(())
            }
            Statement::Let(i, ty_, expr) => self.compile_let(i, ty_, expr),
            Statement::Return(e) => self.compile_return(e),
            Statement::Expr(e) => {
                self.compile_expr(e)?;
                Ok(())
            }
            Statement::If(c) => self.compile_condition(c),
            _ => bail!("not yet supported."),
        }
    }

    pub fn compile(&'f self) -> Result<String> {
        for stmt in &self.program {
            self.compile_stmt(stmt)?;
        }
        Ok(self.module.print_to_string().to_string())
    }
}

pub fn generate(module_name: &str, program: Program) -> anyhow::Result<()> {
    let context = Context::create();
    let llvm = LLVMCodeGen::new(&context, program, module_name);
    fs::write("./a.ll", llvm.compile()?)?;

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
        AddressSpace, IntPredicate,
    };

    use crate::{
        ast::Token,
        lexer::{lexer::Lexer, logos_lexer::LexError},
        parser::Parser,
    };

    use super::*;

    #[test]
    fn test_expr_codegen() {
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
        fun main() => usize {
            // let v = "%d\n";
            printf(353);
            return 0;
        }
        "#;
        let ast = Parser::new(module, source).parse().unwrap();
        println!("{:?}", &ast);
        let llvm_codegen = LLVMCodeGen::new(&ctx, ast, module);
        println!("{}", llvm_codegen.compile().unwrap());
    }

    #[test]
    fn test_codegen_str<'a>() {
        init();
        let ctx = Context::create();
        let module = "something";
        let source = r#"
        fun return_something(a: usize) => bool {
            let something: bool = false;
            let another_var: usize = 3535 + 35;
            let s: str = "this is a string";
            return false;
        }
        "#;
        let ast = Parser::new(module, source).parse().unwrap();
        println!("{:?}", &ast);
        let llvm_codegen = LLVMCodeGen::new(&ctx, ast, module);
        println!("{}", llvm_codegen.compile().unwrap());
    }

    #[test]
    fn test_conditional_branch() {
        let ctx = Context::create();
        let builder = ctx.create_builder();
        let module = ctx.create_module("test");

        let fn_val = module.add_function(
            "do_something",
            ctx.i64_type()
                .fn_type(&[BasicMetadataTypeEnum::IntType(ctx.i64_type())], false),
            None,
        );
        let main_block = ctx.append_basic_block(fn_val, "");
        builder.position_at_end(main_block);

        let ptr = builder.build_alloca(ctx.i64_type(), "");

        let cmp = builder.build_int_compare(
            IntPredicate::EQ,
            ctx.i64_type().const_int(10, false),
            ctx.i64_type().const_int(10, false),
            "",
        );
        builder.build_store(ptr, cmp);

        let if_block = ctx.append_basic_block(fn_val, "");
        let else_block = ctx.append_basic_block(fn_val, "");

        builder.build_conditional_branch(cmp, if_block, else_block);
        builder.position_at_end(if_block);
        builder.build_return(Some(&ctx.i64_type().const_int(445, false)));

        builder.position_at_end(else_block);
        builder.build_return(Some(&ctx.i64_type().const_int(445, false)));

        builder.position_at_end(main_block);
        builder.build_return(Some(&ctx.i64_type().const_int(0, false)));
        println!("{}", module.print_to_string().to_string());
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
