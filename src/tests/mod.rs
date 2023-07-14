#[cfg(test)]
pub mod test_interpreter;
#[cfg(test)]
pub mod test_lexer;
#[cfg(test)]
pub mod test_parser;

pub fn setup() {
    let _ = env_logger::builder().is_test(true).try_init();
}
