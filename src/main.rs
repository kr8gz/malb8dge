#![allow(unstable_name_collisions)] // i just want to use intersperse

use std::env;

use crate::{
    lex::lexer::Lexer,
    parse::parser::Parser,
    run::interpreter::Interpreter,
    util::errors::Error,
};

mod util;

mod lex;
mod parse;
mod compile;
mod run;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let file = args.next().unwrap_or_else(|| Error::simple("Missing required argument: filename".into()));

    macro_rules! parse_args {
        ( $( $var:ident: $arg:literal, )* ) => {
            $( let mut $var = false; )*
            
            for arg in args {
                match arg.as_str() {
                    $(
                        $arg => {
                            if $var { Error::simple(format!("Duplicate argument '{arg}'")); }
                            $var = true;
                        }
                    )*
                    _ => Error::simple(format!("Invalid argument '{arg}'"))
                }
            }
        }
    }

    parse_args! {
        debug: "-d",
        debug_lexer: "-dl",
        debug_parser: "-dp",
        debug_interpreter: "-di",
        warnings: "-w",
    };

    macro_rules! handle {
        ( $expr:expr ) => { $expr.unwrap_or_else(|e| e.eprint()) }
    }
    
    let mut lexer = Lexer::from_file(&file);
    handle!(lexer.lex());
    if debug || debug_lexer { println!("{:#?}", &lexer); }
    
    let mut parser = Parser::new(lexer);
    handle!(parser.parse());
    if debug || debug_parser { println!("{:#?}", &parser); }
    
    let mut interpreter = Interpreter::new();
    handle!(interpreter.run(parser));
    if debug || debug_interpreter { println!("{:#?}", &interpreter); }
}
