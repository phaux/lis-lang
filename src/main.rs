#![warn(clippy::pedantic)]
#![allow(clippy::missing_errors_doc)]
#![cfg_attr(
    not(test),
    warn(clippy::panic, clippy::unwrap_used, clippy::expect_used)
)]

pub mod ast;
pub mod parser;
pub mod state;
pub mod tokenizer;
pub mod vm;

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
    exec_prog(scope, &ast)?;
    Ok(())
}
