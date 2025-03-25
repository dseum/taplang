//! This is the parser and interpreter for the 'Foo' language. See `tutorial.md` in the repository's root to learn
//! about it.
mod ast;
mod lexer;
mod parser;
// mod eval;
use chumsky::prelude::*;
use parser::command_parser;
// use eval::eval;


fn main() {
    let usage = "Run `cargo run --examples/sample.tap`";
    let src = std::fs::read_to_string(std::env::args().nth(1).expect(usage)).expect(usage);
    let json = command_parser()
        .parse(&src)
        .into_result();
    println!("{:#?}", json);
}