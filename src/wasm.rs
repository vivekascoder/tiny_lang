use crate::interpreter::Interpreter;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() {
    console_error_panic_hook::set_once();
}

#[wasm_bindgen]
pub fn run_tiny(source: &str) -> String {
    let mut interpreter = Interpreter::new("playground.tiny", source);
    match interpreter.eval_with_output() {
        Ok(output) if output.is_empty() => "(no output)".to_string(),
        Ok(output) => output,
        Err(err) => format!("error: {err}"),
    }
}
