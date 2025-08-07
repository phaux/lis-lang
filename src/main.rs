#![warn(clippy::pedantic)]

mod parser;
mod runtime;
mod tokenizer;

use std::io::Read;

use runtime::Runtime;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename)?
    } else {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        buffer
    };
    let mut vm = Runtime::new();
    vm.exec_str(&input)?;
    Ok(())
}
