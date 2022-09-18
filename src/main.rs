use std::{env, process};

use ariadne::{Fmt, Color};

mod constants;
mod errors;

mod lexer;
mod parser;
mod interpreter;

mod tests;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let file = args.next().unwrap_or_else(|| {
        eprintln!("{} Missing required argument: filename", "Error:".fg(Color::Red));
        process::exit(1);
    });
    
    dbg!(parser::Parser::new(lexer::Lexer::from_file(file)));
}
