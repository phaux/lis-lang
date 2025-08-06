mod ast;
mod parser;
mod tokenizer;
mod vm;

use vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).ok_or("arg 1 missing")?;
    let input = std::fs::read_to_string(filename)?;
    let mut vm = Vm::new();
    vm.exec_str(&input);
    Ok(())
}
