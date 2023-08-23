pub mod llvm;

pub trait CodeGen<'a> {
    fn generate(&self) -> String;
}
