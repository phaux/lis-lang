#![warn(clippy::pedantic)]

pub(crate) mod ast;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod state;
pub(crate) mod vm;

use std::{io::Read, rc::Rc};

use self::{parser::Parser, state::Scope, vm::exec_prog};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let input = if let Some(filename) = std::env::args().nth(1) {
        std::fs::read_to_string(filename)?
    } else {
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        buffer
    };
    let scope = Rc::new(Scope::default());
    let ast = Parser::new(&input).parse_prog()?;
    dbg!(&ast);
    let result = exec_prog(Rc::clone(&scope), &ast)?;
    println!("{result:?}");
    Ok(())
}
