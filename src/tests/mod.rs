#[cfg(test)]
pub mod test_interpreter;
#[cfg(test)]
pub mod test_lexer;
#[cfg(test)]
pub mod test_parser;

#[cfg(test)]
pub mod test_logos_lexer;

pub fn setup() {
    let _ = env_logger::builder().is_test(true).try_init();
}
