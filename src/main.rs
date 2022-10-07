use std::env;

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
    
    let lexer = lex::lexer::Lexer::from_file(file);
    let parser = parse::parser::Parser::new(lexer);
    if debug { println!("{:#?}", &parser); }
    let compiler = compile::compiler::Compiler::new(parser, warnings);
    if debug { println!("{:#?}", &compiler); }
    run::interpreter::Interpreter::run(compiler);
}
