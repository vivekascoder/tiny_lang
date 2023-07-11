#[cfg(test)]
pub mod test_interpreter;
#[cfg(test)]
pub mod test_lexer;
#[cfg(test)]
pub mod test_parser;

use std::sync::Once;

static INIT: Once = Once::new();

pub fn setup() {
    let _ = env_logger::builder().is_test(true).try_init();
}
