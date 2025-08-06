mod ast;
mod parser;
mod tokenizer;
mod vm;

use vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = match std::env::args().nth(1) {
        Some(filename) => {
            let input = std::fs::read_to_string(filename)?;
            input
        }
        None => {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer)?;
            buffer
        }
    };
    let mut vm = Vm::new();
    vm.exec_str(&input)?;
    Ok(())
}
