use std::{env, process};

use ariadne::{Color, Fmt};

mod ast;
mod util;
mod errors;

mod lexer;
mod parser;
mod compiler;

mod tests;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let file = args.next().unwrap_or_else(|| {
        eprintln!("{} Missing required argument: filename", "Error:".fg(Color::Red));
        process::exit(1)
    });
    
    let lexer = lexer::Lexer::from_file(file);
    
    let parser = parser::Parser::new(lexer);

    println!("{:#?}", &parser);
}
