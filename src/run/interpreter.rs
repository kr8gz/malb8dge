use std::process;

use crate::{util::{*, errors::Error}, parse::{parser::Parser, ast::*}};

use super::types::*;

#[derive(Debug)]
pub struct Interpreter {
    variables: Vec<Option<Value>>,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            variables: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn run(&mut self, parser: Parser) -> Result<()> {
        for stmt in parser.statements {
            self.run_node(&stmt)?;
        }
        Ok(())
    }

    fn run_node(&mut self, node: &Node) -> Result<Value> {
        use NodeType::*;

        let Node { data, pos } = node;

        match data {
            Statements(stmts) => {
                if let Some((last, rest)) = stmts.split_last() {
                    for stmt in rest {
                        self.run_node(stmt)?;
                    }
                    self.run_node(last)
                } else {
                    Ok(ValueType::Null().into_value(&node.pos))
                }
            }

            // ...

            Exit(expr) => {
                if let Some(expr) = expr.as_ref() {
                    println!("{}", self.run_node(expr)?.data.as_string());
                }
                process::exit(0)
            }

            // ...

            Print { value, mode } => {
                use PrintMode::*;

                let value = self.run_node(value)?;
                match mode {
                    Default      => println!("{}", value.as_joined_list_string("")),
                    NoNewline    => print!  ("{}", value.as_joined_list_string("")),
                    Spaces       => println!("{}", value.as_joined_list_string(" ")),
                    SplitNewline => println!("{}", value.as_joined_list_string("\n")),
                }
                Ok(value)
            }

            // ...

            FragmentString(frags) => {
                Ok(ValueType::String(
                    frags.iter().map(|f| match f {
                        ParsedFragment::Expr(node) => Ok(self.run_node(node)?.as_string()),
                        ParsedFragment::Literal(lit) => Ok(lit.into()),
                    }).collect::<Result<String>>()?
                ).into_value(pos))
            }

            Literal(lit) => {
                Ok(lit.clone().into_value(pos))
            }

            _ => todo!("remove this catch-all arm when done")
        }
    }
}
