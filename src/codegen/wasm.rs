use super::CodeGen;
use crate::ast::{Program, Statement};
use wasm_encoder::Module;

struct WASM {
    ast: Program,
}

impl CodeGen for WASM {
    fn generate(&self) -> Vec<u8> {
        let mut module = Module::new();
        let _ = self
            .ast
            .into_iter()
            .map(|stmt| match stmt {
                Statement::Function(f) => {
                    module.
                }
                _ => {
                    unimplemented!("implement this too")
                }
            })
            .collect();
        todo!()
    }
}

#[cfg(test)]
mod tests {
    fn create_mod() {}
}
