use std::{env, process};

use ariadne::{Color, Fmt};

mod util;
mod errors;

mod lex;
mod parse;
mod compile;
mod run;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let file = args.next().unwrap_or_else(|| {
        eprintln!("{} Missing required argument: filename", "Error:".fg(Color::Red));
        process::exit(1)
    });
    
    let lexer = lex::lexer::Lexer::from_file(file);
    
    let parser = parse::parser::Parser::new(lexer);

    let compiler = compile::compiler::Compiler::new(parser);

    println!("{:#?}", &compiler);
}
