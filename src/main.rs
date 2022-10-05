use std::{env, process};

use ariadne::{Color, Fmt};

mod operators;
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

    macro_rules! parse_args {
        ( $( $var:ident: $arg:literal, )* ) => {
            $( let mut $var = false; )*
            
            for arg in args {
                match arg.as_str() {
                    $(
                        $arg => {
                            if $var {
                                eprintln!("{} Duplicate argument '{arg}'", "Error:".fg(Color::Red));
                                process::exit(1)
                            }
                            $var = true;
                        }
                    )*
                    _ => {
                        eprintln!("{} Invalid argument '{arg}'", "Error:".fg(Color::Red));
                        process::exit(1)
                    }
                }
            }
        }
    }

    parse_args! {
        warnings: "-w",
    };
    
    let lexer = lex::lexer::Lexer::from_file(file);
    let parser = parse::parser::Parser::new(lexer);
    println!("{:#?}", &parser);
    let compiler = compile::compiler::Compiler::new(parser, warnings);
    println!("{:#?}", &compiler);
}
