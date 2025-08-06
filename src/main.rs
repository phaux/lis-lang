#![warn(clippy::pedantic)]

mod ast;
mod parser;
mod tokenizer;
mod vm;

use std::io::Read;

use vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename)?
    } else {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        buffer
    };
    let mut vm = Vm::new();
    vm.exec_str(&input)?;
    Ok(())
}
