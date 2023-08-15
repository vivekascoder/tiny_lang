use super::CodeGen;
use inkwell::{builder::Builder, context::Context, module::Module};

struct LLVMCodeGen<'a, 'ctx> {
    ctx: &'ctx Context,
    builder: &'a Builder<'ctx>,
    module: &'a Module<'ctx>,
}

/// https://llvm.org/docs/LangRef.html
impl<'a, 'ctx> LLVMCodeGen<'a, 'ctx> {
    fn generate(&self) -> Vec<u8> {
        todo!()
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

    use super::*;

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
}
