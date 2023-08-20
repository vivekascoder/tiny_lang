use crate::ast::{Expr, Function, Literal, Program, Statement, Type};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType},
    values::FunctionValue,
};

use super::CodeGen;

struct LLVMCodeGen<'a> {
    ctx: &'a Context,
    program: Program,
    builder: Builder<'a>,
    module: Module<'a>,
}

/// https://llvm.org/docs/LangRef.html
impl<'a> LLVMCodeGen<'a> {
    pub fn new(ctx: &'a Context, program: Program, module_name: &str) -> Self {
        Self {
            ctx,
            program,
            builder: ctx.create_builder(),
            module: ctx.create_module(module_name),
        }
    }

    fn codegen_for_expr_statement(&self, expr: Expr) {
        // match &expr {
        // Expr::Literal(l) => match l {
        //     // Literal::
        // },
        // Expr::Infix(i, l, r) => {

        // }
        // _ => unimplemented!(),
        // }
    }

    fn function_codegen(&self, f: &Function) -> FunctionValue<'_> {
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

        self.module.add_function(&f.name, fn_type, None)
    }
}

impl<'a> CodeGen for LLVMCodeGen<'a> {
    fn generate(&self) -> String {
        for stmt in &self.program {
            match &stmt {
                Statement::Function(f) => {
                    self.function_codegen(f);
                }
                _ => panic!("not yet supported."),
            }
        }
        self.module.print_to_string().to_string()
    }
}

#[cfg(test)]
mod tests {
    use inkwell::{
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
    fn test_expr_codegen() {
        let ctx = Context::create();
        let module = "something";
        let source = "23 + 34 - 5;";
        let program = Parser::new(module, source).parse().unwrap();
        println!("program: {:?}", &program);

        let llvm_codegen = LLVMCodeGen::new(&ctx, program, module);
        println!("{:?}", llvm_codegen.generate());
    }

    #[test]
    fn test_codegen_function() {
        let ctx = Context::create();
        let module = "something";
        let source = r#"
        fun return_something(a: usize) => bool {
            let something = false;
            let another_var = 3535 + 35;
            return something;
        }
        "#;
        let llvm_codegen =
            LLVMCodeGen::new(&ctx, Parser::new(module, source).parse().unwrap(), module);
        println!("{:?}", llvm_codegen.generate());
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
