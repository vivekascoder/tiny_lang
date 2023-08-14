pub mod wasm;

pub trait CodeGen {
    fn generate(&self) -> Vec<u8>;
}
