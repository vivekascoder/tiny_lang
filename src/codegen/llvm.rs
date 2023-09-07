use crate::ast::{Condition, ExternFunction, FunctionCall, Struct, While};
use crate::ast::{Expr, Function, Ident, Infix, Literal, Program, Statement, Type};
use crate::error::TinyError;
use anyhow::{anyhow, bail};
use anyhow::{ensure, Result};
use either::Either;
use inkwell::basic_block::BasicBlock;
use inkwell::module::Linkage;
use inkwell::types::{BasicType, BasicTypeEnum, StructType};
use inkwell::values::IntValue;
use inkwell::values::{BasicMetadataValueEnum, StructValue};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::BasicMetadataTypeEnum,
    values::{BasicValue, BasicValueEnum, FunctionValue, PointerValue},
};
use inkwell::{AddressSpace, IntPredicate};
use log::info;
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
        self.ty.clone()
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
    file_path: String,
    scopes: Rc<RefCell<Vec<HashMap<Rc<str>, (PointerValue<'ctx>, Type)>>>>,
    fns: Rc<RefCell<HashMap<Rc<str>, (FunctionValue<'ctx>, &'f Function)>>>,
    structs: Rc<RefCell<HashMap<Rc<str>, (StructType<'ctx>, Rc<Struct>)>>>,
    extern_fns: Rc<RefCell<HashMap<Rc<str>, FunctionValue<'ctx>>>>,
    current_fn: Rc<RefCell<Option<(FunctionValue<'ctx>, BasicBlock<'ctx>)>>>,
}

/// # Main codegen part
/// https://llvm.org/docs/LangRef.html
impl<'ctx, 'f> LLVMCodeGen<'ctx, 'f> {
    pub fn new(ctx: &'ctx Context, program: Program, module_name: &str) -> Self {
        Self {
            ctx,
            program,
            builder: ctx.create_builder(),
            module: ctx.create_module(module_name),
            file_path: module_name.into(),
            scopes: Rc::new(RefCell::new(vec![])),
            fns: Rc::new(RefCell::new(HashMap::new())),
            structs: Rc::new(RefCell::new(HashMap::new())),
            extern_fns: Rc::new(RefCell::new(HashMap::new())),
            current_fn: Rc::new(RefCell::new(None)),
        }
    }

    fn get_basic_metadata_type_enum(&self, ty: &Type) -> BasicMetadataTypeEnum<'ctx> {
        match ty {
            Type::UnsignedInteger => self.ctx.i32_type().into(),
            Type::SignedInteger => self.ctx.i32_type().into(),
            Type::Bool => self.ctx.bool_type().into(),
            Type::Char => self.ctx.i8_type().into(),
            Type::String => self.ctx.i8_type().ptr_type(AddressSpace::default()).into(),
            Type::Ptr(p) => match p.as_ref() {
                Type::UnsignedInteger => self.ctx.i32_type().ptr_type(AddressSpace::from(0)).into(),
                Type::Char => self.ctx.i8_type().ptr_type(AddressSpace::from(0)).into(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn get_basic_type_enum(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        info!("getting the corresponding type for {:?}", ty);
        match ty {
            Type::UnsignedInteger => self.ctx.i32_type().into(),
            Type::SignedInteger => self.ctx.i32_type().into(),
            Type::Bool => self.ctx.bool_type().into(),
            Type::Char => self.ctx.i8_type().into(),
            Type::String => self.ctx.i8_type().ptr_type(AddressSpace::default()).into(),
            Type::Ptr(p) => match p.as_ref() {
                Type::UnsignedInteger => self.ctx.i32_type().ptr_type(AddressSpace::from(0)).into(),
                Type::Char => self.ctx.i8_type().ptr_type(AddressSpace::from(0)).into(),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        }
    }

    fn push_scope(&self) {
        self.scopes.as_ref().borrow_mut().push(HashMap::new());
    }

    fn pop_current_scope(&self) {
        info!(
            "scope popped: {:?}",
            self.scopes.as_ref().borrow_mut().pop()
        );
    }

    fn i8_llvm_str(&self, v: &str) -> Vec<IntValue> {
        v.bytes()
            .into_iter()
            .map(|v| self.ctx.i8_type().const_int(v as u64, false))
            .collect::<Vec<IntValue>>()
    }

    fn insert_extfunc(&self, v: (Rc<str>, FunctionValue<'ctx>)) {
        self.extern_fns.as_ref().borrow_mut().insert(v.0, v.1);
    }

    fn compile_infix_expr(&self, i: &Infix, l: &Expr, r: &Expr) -> Result<Value<'ctx>> {
        let l_val = self.compile_expr(l)?;
        let r_val = self.compile_expr(r)?;
        info!("lval, rval: {:?}, {:?}", &l_val.val(), &r_val.val());

        if !l_val.ty().eq(&r_val.ty()) {
            bail!("type mismatch.");
        }

        match (l_val.val(), r_val.val()) {
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
                Infix::LessThan => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::SLT, l, r, "")
                        .as_basic_value_enum(),
                )),
                Infix::DoubleEqual => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::EQ, l, r, "")
                        .as_basic_value_enum(),
                )),
                Infix::NotEqual => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::NE, l, r, "")
                        .as_basic_value_enum(),
                )),
                Infix::GreaterThanEqual => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::SGE, l, r, "")
                        .as_basic_value_enum(),
                )),
                Infix::LessThanEqual => Ok(Value::new(
                    Type::Bool,
                    self.builder
                        .build_int_compare(IntPredicate::SLE, l, r, "")
                        .as_basic_value_enum(),
                )),
                _ => unimplemented!("plz implement other infix for int values."),
            },
            _ => unimplemented!(),
        }
    }

    fn compile_expr(&self, expr: &Expr) -> Result<Value<'ctx>> {
        match &expr {
            Expr::Literal(l) => match l {
                Literal::UnsignedInteger(v) => Ok(Value::new(
                    Type::UnsignedInteger,
                    self.ctx
                        .i32_type()
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
            Expr::Ident(i) => Ok(self.compile_expr_ident(i)?),
            Expr::Infix(i, l, r) => Ok(self.compile_infix_expr(i, l, r)?),
            Expr::Call(fn_call) => Ok(self.compile_expr_call(fn_call)?),
            Expr::Prefix(p, expr) => {
                unimplemented!("plz implement prefix expr compilation ser.")
            }
            Expr::StructInstance(name, fields) => {
                Ok(self.compile_expr_struct_instance(name, fields)?)
            }
            Expr::StructAccessIdent(idents) => self.compile_struct_access_ident(idents),
            _ => unimplemented!(),
        }
    }

    fn compile_struct_access_ident(&self, idents: &Vec<Rc<str>>) -> Result<Value<'ctx>> {
        let struct_instance_nm = idents.first().unwrap();
        let scopes = self.scopes.as_ref().borrow();
        let (ptr_val, ptr_type) = scopes
            .iter()
            .rev()
            .find_map(|s| s.get(struct_instance_nm))
            .ok_or_else(|| {
                anyhow!(
                    "{}",
                    TinyError::new_compilation_error(
                        self.file_path.clone().into(),
                        format!("struct instance `{}` isn't defined.", struct_instance_nm)
                    )
                )
            })?;

        let struct_nm = match ptr_type {
            Type::Struct(nm) => Rc::clone(nm),
            _ => {
                bail!(
                    "{}",
                    TinyError::new_compilation_error(
                        self.file_path.clone().into(),
                        format!("pointer type is not struct.")
                    )
                )
            }
        };
        let structs = self.structs.as_ref().borrow();

        let struct_ = match structs.get(&struct_nm) {
            Some(v) => v,
            _ => {
                bail!("struct doesn't exists.");
            }
        };

        if idents.len() > 2 {
            bail!("not supprted nested access yet.");
        }
        let access_field = idents.last().unwrap();
        let index = struct_
            .1
            .as_ref()
            .fields
            .iter()
            .position(|x| &x.0 .0 == access_field)
            .ok_or(anyhow!(
                "field `{}` doesn't exist in the struct defination.",
                access_field
            ))?;
        let field = struct_
            .1
            .as_ref()
            .fields
            .iter()
            .find(|x| &x.0 .0 == access_field)
            .ok_or(anyhow!(
                "field `{}` doesn't exist in the struct defination.",
                access_field
            ))?;

        let ptr = unsafe {
            self.builder.build_gep(
                struct_.0,
                *ptr_val,
                &[
                    self.ctx.i32_type().const_int(0, false),
                    self.ctx.i32_type().const_int(index as u64, false),
                ],
                "",
            )
        };

        // TODO: use Rc<Type> to reduce .clone()
        Ok(Value {
            ty: Type::Ptr(Box::new(field.1.clone())),
            val: ptr.into(),
        })
    }

    fn compile_expr_ident(&self, i: &Ident) -> Result<Value<'ctx>> {
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
                    ty_.clone(),
                    self.builder.build_load(self.ctx.bool_type(), *ptr, ""),
                )),
                Type::Char => Ok(Value::new(
                    ty_.clone(),
                    self.builder.build_load(self.ctx.i8_type(), *ptr, ""),
                )),
                Type::UnsignedInteger => Ok(Value::new(
                    ty_.clone(),
                    self.builder.build_load(self.ctx.i32_type(), *ptr, ""),
                )),
                Type::SignedInteger => Ok(Value::new(
                    ty_.clone(),
                    self.builder.build_load(self.ctx.i32_type(), *ptr, ""),
                )),
                Type::String => Ok(Value::new(
                    ty_.clone(),
                    self.builder.build_load(
                        self.ctx.i8_type().ptr_type(AddressSpace::default()),
                        *ptr,
                        "",
                    ),
                )),
                _ => unimplemented!(),
            }
        } else {
            panic!("ident doesn't exists.")
        }
    }

    fn compile_expr_call(&self, fn_call: &FunctionCall) -> Result<Value<'ctx>> {
        let llvm_type_args = fn_call
            .parameters
            .iter()
            .map(|a| self.compile_expr(a).unwrap().val().into())
            .collect::<Vec<BasicMetadataValueEnum>>();
        if let Some(v) = self.fns.as_ref().borrow().get(fn_call.name.as_ref()) {
            self.push_scope();
            match self
                .builder
                .build_call(v.0, &llvm_type_args, "")
                .try_as_basic_value()
            {
                Either::Left(val) => {
                    self.pop_current_scope();
                    Ok(Value::new(v.1.return_type.clone().unwrap(), val))
                }
                Either::Right(_) => {
                    bail!("got right");
                }
            }
        } else {
            if let Some(f) = self.extern_fns.as_ref().borrow().get(fn_call.name.as_ref()) {
                // let gep = self.builder.build_gep(self.ctx.i8_type().array_type(), ptr, ordered_indexes, name)
                self.builder.build_call(
                    *f,
                    &[
                        // self.builder
                        // .build_gep(pointee_ty, ptr, ordered_indexes, name),
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

    fn compile_expr_struct_instance(
        &self,
        name: &Rc<str>,
        fields: &Vec<(Ident, Expr)>,
    ) -> Result<Value<'ctx>> {
        // FIXME: case where you didn't added some field.
        let mut values: Vec<BasicValueEnum> = vec![];
        if let Some((struct_type, struct_)) = self.structs.as_ref().borrow().get(name) {
            let fields_stack = struct_.fields.clone();
            for (field_name, field_expr) in fields {
                let expr_result = self.compile_expr(field_expr)?;
                values.push(expr_result.val);
                if let Some(v) = fields_stack.iter().find(|f| &f.0 == field_name) {
                    if expr_result.ty() != v.1 {
                        bail!(
                            "field {} is expected to have type {:?}, got {:?} instead.",
                            v.0 .0,
                            &v.1,
                            expr_result.ty()
                        );
                    }
                } else {
                    bail!(
                        "field {} is not defined in struct type {}",
                        field_name.0,
                        name
                    );
                }
            }
            Ok(Value {
                ty: Type::Struct(Rc::clone(&struct_.name)),
                val: struct_type.const_named_struct(&values).into(),
            })
        } else {
            bail!("struct {}, is not defined.", name);
        }
    }

    fn compile_fn(&self, f: &'f Function) -> Result<FunctionValue<'ctx>> {
        self.scopes.as_ref().borrow_mut().push(HashMap::new());
        let params = f
            .params
            .iter()
            .map(|(_i, t)| match t {
                Type::UnsignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i32_type()),
                Type::SignedInteger => BasicMetadataTypeEnum::IntType(self.ctx.i32_type()),
                Type::Bool => BasicMetadataTypeEnum::IntType(self.ctx.bool_type()),
                Type::Char => BasicMetadataTypeEnum::IntType(self.ctx.i8_type()),
                Type::String => BasicMetadataTypeEnum::PointerType(
                    self.ctx.i8_type().ptr_type(AddressSpace::default()),
                ),
                _ => unimplemented!(),
            })
            .collect::<Vec<_>>();
        let fn_type = match &f.return_type {
            Some(v) => match v {
                Type::UnsignedInteger => self.ctx.i32_type().fn_type(&params, false),
                Type::SignedInteger => self.ctx.i32_type().fn_type(&params, false),
                Type::Bool => self.ctx.i32_type().fn_type(&params, false),
                Type::Char => self.ctx.i8_type().fn_type(&params, false),
                Type::String => self
                    .ctx
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&params, false),
                _ => unimplemented!(),
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

        // Delete the scope?

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
            Type::UnsignedInteger => self.builder.build_alloca(self.ctx.i32_type(), ""),
            Type::SignedInteger => self.builder.build_alloca(self.ctx.i32_type(), ""),
            Type::Bool => self.builder.build_alloca(self.ctx.bool_type(), ""),
            Type::Char => self.builder.build_alloca(self.ctx.i8_type(), ""),
            Type::String => {
                if let BasicValueEnum::ArrayValue(av) = val.val() {
                    self.builder.build_alloca(av.get_type(), "")
                } else {
                    bail!("Type string with not `ArrayValue`");
                }
            }
            Type::Struct(ref name) => {
                if let Some((struct_type, _)) = self.structs.as_ref().borrow().get(name) {
                    self.builder
                        .build_alloca(struct_type.as_basic_type_enum(), "")
                } else {
                    bail!("struct {}, is not defined.", name);
                }
            }
            Type::Ptr(ref p) => match p.as_ref() {
                Type::UnsignedInteger => self
                    .builder
                    .build_alloca(self.ctx.i32_type().ptr_type(AddressSpace::from(0)), ""),
                _ => unimplemented!(),
            },
            _ => unimplemented!(),
        };
        self.builder.build_store(ptr, val.val());

        let mut scopes = self.scopes.as_ref().borrow_mut();
        let c_scope = scopes.last_mut().unwrap();
        if c_scope.contains_key(&i.0) {
            bail!("already declared in this scope.");
        }
        // TODO: remove unwrap
        info!("inserted {} with value {:?}", &i.0, (&ptr, ty_));
        c_scope.insert(Rc::clone(&i.0), (ptr, ty_.clone().unwrap()));
        Ok(())
    }

    fn compile_condition(&self, cond: &'f Condition) -> Result<()> {
        self.push_scope();
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
            self.push_scope();
            self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = else_block.unwrap();
            self.builder.position_at_end(else_block.unwrap());
            for stmt in cond.else_body.as_ref().unwrap() {
                self.compile_stmt(stmt)?;
            }
            match cond.else_body.as_ref().unwrap().last().unwrap() {
                Statement::Return(_) => {}
                _ => {
                    self.builder.build_unconditional_branch(new_block);
                }
            };
            self.builder.position_at_end(new_block);
            self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = current_fn_block;
            self.pop_current_scope();
        }

        self.current_fn.as_ref().borrow_mut().as_mut().unwrap().1 = current_fn_block;

        Ok(())
    }

    fn compile_while(&self, while_: &'f While) -> Result<()> {
        self.push_scope();
        let current_fn = self.current_fn.as_ref().borrow().as_ref().unwrap().0;
        let loop_cond = self.ctx.append_basic_block(current_fn, "");
        self.builder.build_unconditional_branch(loop_cond);
        self.builder.position_at_end(loop_cond);
        let cond = self.compile_expr(&while_.condition)?;
        Value::assert_if_not(&Type::Bool, &cond.ty())?;

        let loop_body = self.ctx.append_basic_block(current_fn, "");
        let new_block = self.ctx.append_basic_block(current_fn, "");
        self.builder
            .build_conditional_branch(cond.val().into_int_value(), loop_body, new_block);

        self.builder.position_at_end(loop_body);
        for stmt in &while_.body {
            self.compile_stmt(stmt)?;
        }

        // If you encounter a return statement ?
        // create new block and

        self.builder.build_unconditional_branch(loop_cond);

        self.builder.position_at_end(new_block);
        self.pop_current_scope();

        Ok(())
    }

    fn compile_mutate(&self, ident: &Ident, expr: &Expr, is_ptr: bool) -> Result<()> {
        let new_val = self.compile_expr(expr)?;

        if let Some((ptr, ty_)) = self
            .scopes
            .as_ref()
            .borrow()
            .iter()
            .rev()
            .find_map(|s| s.get(&ident.0))
        {
            if !is_ptr {
                Value::assert_if_not(&new_val.ty(), ty_)?;
                self.builder.build_store(*ptr, new_val.val());
            } else {
                ensure!(ty_.is_ptr(), "type is not pointer.");
                // Load the pointer from the ident's pointer,
                let loaded_ptr = self.builder.build_load(
                    self.ctx.i32_type().ptr_type(AddressSpace::from(0)),
                    *ptr,
                    "",
                );
                // then build store.
                self.builder
                    .build_store(loaded_ptr.into_pointer_value(), new_val.val());
            }
        }

        Ok(())
    }

    fn compile_external_fn(&self, f: &ExternFunction) -> Result<FunctionValue<'ctx>> {
        let params = f
            .params
            .iter()
            .map(|(_i, t)| self.get_basic_metadata_type_enum(t))
            .collect::<Vec<_>>();
        let fn_type = match &f.return_type {
            Some(v) => match v {
                Type::UnsignedInteger => self.ctx.i32_type().fn_type(&params, false),
                Type::SignedInteger => self.ctx.i32_type().fn_type(&params, false),
                Type::Bool => self.ctx.i32_type().fn_type(&params, false),
                Type::Char => self.ctx.i8_type().fn_type(&params, false),
                Type::String => self
                    .ctx
                    .i8_type()
                    .ptr_type(AddressSpace::default())
                    .fn_type(&params, false),
                Type::Ptr(p) => match p.as_ref() {
                    Type::UnsignedInteger => self
                        .ctx
                        .i32_type()
                        .ptr_type(AddressSpace::from(0))
                        .fn_type(&params, false),
                    Type::Char => self
                        .ctx
                        .i8_type()
                        .ptr_type(AddressSpace::from(0))
                        .fn_type(&params, false),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            },
            None => self.ctx.void_type().fn_type(&params, false),
        };

        let fn_val = self
            .module
            .add_function(&f.name, fn_type, Some(Linkage::External));
        Ok(fn_val)
    }

    fn compile_struct_def(&self, struct_: &Rc<Struct>) -> Result<()> {
        let struct_val = self.ctx.struct_type(
            struct_
                .fields
                .iter()
                .map(|(_, t)| self.get_basic_type_enum(t))
                .collect::<Vec<_>>()
                .as_slice(),
            false,
        );
        self.ctx.struct_type(
            &struct_
                .as_ref()
                .fields
                .iter()
                .map(|(_, ty)| self.get_basic_type_enum(ty))
                .collect::<Vec<_>>(),
            false,
        );
        // let type_alias = self.ctx.opaque_struct_type(&struct_.name);
        // type_alias.set_body(
        //     &struct_
        //         .as_ref()
        //         .fields
        //         .iter()
        //         .map(|(_, ty)| self.get_basic_type_enum(ty))
        //         .collect::<Vec<_>>(),
        //     false,
        // );

        self.structs
            .as_ref()
            .borrow_mut()
            .insert(Rc::clone(&struct_.name), (struct_val, Rc::clone(struct_)));
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
            Statement::While(while_) => self.compile_while(while_),
            Statement::Mutate(var, expr, is_ptr) => self.compile_mutate(var, expr, *is_ptr),
            Statement::ExterFunction(ex_fun) => {
                let fn_ = self.compile_external_fn(ex_fun)?;
                self.extern_fns
                    .as_ref()
                    .borrow_mut()
                    .insert(Rc::clone(&ex_fun.name), fn_);
                Ok(())
            }
            Statement::StructDef(s) => self.compile_struct_def(s),
        }
    }

    pub fn compile(&'f self) -> Result<String> {
        let mut have_main = false;
        for stmt in &self.program {
            match &stmt {
                Statement::Function(f) => {
                    if f.name == "main".into() {
                        have_main = true;
                    }
                }
                Statement::ExterFunction(_) | Statement::StructDef(_) => {}
                _ => {
                    bail!("top level statements can't be {:?}", &stmt);
                }
            };
            self.compile_stmt(stmt)?;
        }

        if !have_main {
            bail!(
                "{}",
                TinyError::new_compilation_error(
                    self.file_path.to_string().into(),
                    "tiny program doesn't have a main function".into()
                )
            )
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
        bail!(
            "can't run llc {:?}\n{}",
            "./a.ll",
            String::from_utf8(res.stderr)?
        );
    }

    let res = std::process::Command::new("clang")
        .arg("./a.s")
        .output()
        .expect("error while running clang");

    if !res.status.success() {
        bail!(
            "can't run clang {:?}\n{}",
            "./a.s",
            String::from_utf8(res.stderr)?
        );
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
        values::{AnyValue, AsValueRef, BasicMetadataValueEnum, BasicValue, PointerValue},
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

        // Building a struct
        /*
        struct Student {
            age: i32,
            is_good: bool,
            nested_struct: Data
        }
        struct Data {
            sub: i32,
        }
        */

        let data_struct = ctx.struct_type(&[ctx.i32_type().into()], false);
        let student_struct = ctx.struct_type(
            &[
                ctx.i32_type().into(),
                ctx.bool_type().into(),
                data_struct.into(),
            ],
            false,
        );

        // construct a student struct.
        let data = data_struct.const_named_struct(&[ctx.i32_type().const_int(32, false).into()]);
        let student_instance = student_struct.const_named_struct(&[
            ctx.i32_type().const_int(100, true).into(),
            ctx.bool_type().const_int(0, false).into(),
            data.into(),
        ]);
        println!(
            "student type: {}",
            student_instance.print_to_string().to_string()
        );
        let struct_ptr = builder.build_alloca(student_struct, "");
        builder.build_store(struct_ptr, student_instance);
        // ..

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
        let g = module.add_global(hello_string.get_type(), Some(AddressSpace::from(0)), "sf");
        // g.set_linkage(Linkage::Internal);
        g.set_initializer(&g);
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
