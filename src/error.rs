use std::process::exit;

pub fn error(message: &str, line: usize, column: usize) {
    eprintln!("Error ln. {} col. {}: {message}", line + 1, column + 1);
    exit(1);
}

pub fn error_plain(message: &str) {
    eprintln!("Error: {message}");
    exit(1);
}
