mod ast;
mod parser;
mod tokenizer;
mod vm;

use parser::Parser;
use vm::Vm;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let filename = std::env::args().nth(1).ok_or("arg 1 missing")?;
    let input = std::fs::read_to_string(filename)?;
    let mut parser = Parser::new(&input);
    let ast = parser.parse_prog();
    let mut vm = Vm::new();
    vm.exec(&ast);
    Ok(())
}
