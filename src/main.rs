use std::env;

use crate::{lex::lexer::Lexer, compile::compiler::Compiler, parse::parser::Parser, run::interpreter::Interpreter};

mod util;

mod lex;
mod parse;
mod compile;
mod run;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let file = args.next().unwrap_or_else(|| util::errors::simple("Missing required argument: filename".into()));

    macro_rules! parse_args {
        ( $( $var:ident: $arg:literal, )* ) => {
            $( let mut $var = false; )*
            
            for arg in args {
                match arg.as_str() {
                    $(
                        $arg => {
                            if $var { util::errors::simple(format!("Duplicate argument '{arg}'")); }
                            $var = true;
                        }
                    )*
                    _ => util::errors::simple(format!("Invalid argument '{arg}'"))
                }
            }
        }
    }

    parse_args! {
        debug: "-d",
        warnings: "-w",
    };

    macro_rules! handle {
        ( $expr:expr ) => {
            $expr.unwrap_or_else(|e| e.eprint(&file))
        }
    }
    
    let mut lexer = Lexer::from_file(&file);
    handle!(lexer.lex());
    if debug { println!("{:#?}", &lexer); }
    
    let mut parser = Parser::new(lexer);
    handle!(parser.parse());
    if debug { println!("{:#?}", &parser); }
    
    let mut compiler = Compiler::new();
    handle!(compiler.compile(parser.statements));
    if debug { println!("{:#?}", &compiler); }
    
    handle!(Interpreter::run(compiler));
}
