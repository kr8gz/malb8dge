#![allow(unstable_name_collisions)] // i just want to use intersperse

use std::{env, fs, io::{self, Write}, process};

use ariadne::{Fmt, Color};

use crate::{
    lex::lexer::Lexer,
    parse::parser::Parser,
    run::{interpreter::Interpreter},
    util::errors::Error,
};

mod lex;
mod parse;
mod run;
mod util;

fn main() {
    let mut args = env::args();
    args.next(); // first arg is the location of the .exe

    let mut is_shell = false;
    let file = args.next().unwrap_or_else(|| {
        is_shell = true;
        "shell".into()
    });

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
                    _ => Error::simple(format!("Unknown argument '{arg}'"))
                }
            }
        }
    }

    parse_args! {
        debug: "-d",
        debug_lexer: "-l",
        debug_parser: "-p",
        debug_interpreter: "-i",
    };
    
    let mut code = String::new();
    let mut interpreter = Interpreter::new(is_shell);

    macro_rules! handle {
        ( $expr:expr => $next:expr ) => {
            match $expr {
                Ok(_) => $next,
                Err(e) => {
                    e.eprint(&file, &code);
                    if !is_shell { process::exit(1) }
                }
            }
        }
    }

    macro_rules! run {
        ( $eval:expr, $offset:expr ) => {
            let mut lexer = Lexer::new(&$eval, $offset);
            handle!(lexer.lex() => {
                if debug || debug_lexer { println!("{:#?}", &lexer); }
                let mut parser = Parser::new(lexer);
                handle!(parser.parse() => {
                    if debug || debug_parser { println!("{:#?}", &parser); }
                    handle!(interpreter.run(parser) => {
                        if debug || debug_interpreter { println!("{:#?}", &interpreter); }
                    })
                })
            })
        }
    }

    if is_shell {
        const GRAY: Color = Color::Fixed(8);
        
        loop {
            let offset = code.len();

            let mut eval = String::new();
            print!("{}", ">>> ".fg(GRAY));
            io::stdout().flush().expect("pls stop causing my io stuff to fail");

            loop {
                let mut line = String::new();
                io::stdin().read_line(&mut line).expect("can you stop inputting invalid utf8");
    
                if line.trim().is_empty() {
                    print!("\x1b[A   \r"); // overwrite the ... prompt above
                    io::stdout().flush().expect("noo omy hack didnt work :(:sob::");
                    break
                } else {
                    eval.push_str(&line.replace("\r\n", "\n"));
                    print!("{}", "... ".fg(GRAY));
                    io::stdout().flush().expect("how do you manage to do this anyway");
                }
            }

            code.push_str(&eval);
            eval.pop(); // remove last newline
            run!(eval, offset);
        }
    }
    
    else {
        code = fs::read_to_string(&file).unwrap_or_else(|err| Error::simple(err));
        run!(code.replace("\r\n", "\n"), 0);
    }
}
