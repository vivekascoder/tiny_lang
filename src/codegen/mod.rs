pub mod llvm;

pub trait CodeGen {
    fn generate(&self) -> String;
}
