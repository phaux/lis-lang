//! Main module.
//! Handles the execution of programs from the command line.

#![warn(clippy::pedantic)]

pub(crate) mod ast;
pub(crate) mod lexer;
pub(crate) mod parser;
pub(crate) mod state;
pub(crate) mod token;
pub(crate) mod vm;

use std::{io::Read, rc::Rc};

use self::{parser::Parser, state::Scope, vm::exec_prog};

fn main() {
    if let Err(e) = run() {
        eprintln!("{e}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), Box<dyn std::error::Error>> {
    let input = if let Some(filename) = std::env::args().nth(1) {
        // Read from file
        std::fs::read_to_string(filename)?
    } else {
        // Read from stdin
        let mut buffer = String::new();
        std::io::stdin().read_to_string(&mut buffer)?;
        buffer
    };
    let scope = Rc::new(Scope::default());
    let ast = Parser::new(&input).parse_prog()?;
    let result = exec_prog(Rc::clone(&scope), &ast)?;
    println!("{result:?}");
    Ok(())
}
