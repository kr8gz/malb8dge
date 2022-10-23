use std::{process, collections::HashMap};

use crate::{util::{*, errors::Error}, parse::{parser::Parser, ast::*}};

use super::types::*;

#[derive(Debug)]
pub struct Interpreter {
    scopes: Vec<Scope>,
    variables: Vec<Value>,
    var_count: usize,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            scopes: Vec::new(),
            variables: Vec::new(),
            var_count: 0,
            call_stack: Vec::new(),
        }
    }

    fn new_scope(&mut self, scope: usize) -> usize {
        let id = self.scopes.len();
        self.scopes[scope].children.push(id);
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: Some(scope),
            children: Vec::new(),
        });
        id
    }

    fn check_var(&self, name: &str, scope: usize, pos: &Pos) -> Result<usize> {
        self.get_var(name, scope).ok_or_else(|| {
            Error::err("Use of undefined variable")
                .label(pos.clone(), format!("Couldn't find variable #{name}# in this scope"))
        })
    }

    fn get_var(&self, name: &str, scope: usize) -> Option<usize> {
        let mut scope = &self.scopes[scope];
        loop {
            match scope.vars.get(name) {
                Some(var_id) => return Some(*var_id),
                None => match scope.parent {
                    Some(scope_id) => scope = &self.scopes[scope_id],
                    None => return None,
                },
            }
        }
    }

    fn new_var(&mut self, name: &str, scope: usize, pos: &Pos) -> usize {
        let id = self.var_count;
        self.var_count += 1;
        self.scopes[scope].vars.insert(name.into(), id);
        self.variables.push(ValueType::Null().into_value(pos));
        id
    }

    pub fn run(&mut self, parser: Parser) -> Result<()> {
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });

        for stmt in parser.statements {
            self.run_node(&stmt, 0)?;
        }

        Ok(())
    }

    fn run_node(&mut self, node: &Node, scope: usize) -> Result<Value> {
        use NodeType::*;

        let Node { data, pos } = node;

        let value = match data {
            Statements(stmts) => {
                if let Some((last, rest)) = stmts.split_last() {
                    let inner = self.new_scope(scope);
                    for stmt in rest {
                        self.run_node(stmt, inner)?;
                    }
                    self.run_node(last, inner)?
                } else {
                    ValueType::Null().into_value(&node.pos)
                }
            }

            // ...

            Exit(expr) => {
                if let Some(expr) = expr.as_ref() {
                    println!("{}", self.run_node(expr, scope)?.data.as_string());
                }
                process::exit(0)
            }

            // ...

            UnaryOp { target, op_type, op } => {
                operators::run_unary_op(self.run_node(target, scope)?, *op_type, op, pos)?.into_value(pos)
            }

            BinOp { a, op, b } => {
                operators::run_bin_op(self.run_node(a, scope)?, self.run_node(b, scope)?, op, pos)?.into_value(pos)
            }

            // ...

            Print { value, mode } => {
                use PrintMode::*;

                let value = self.run_node(value, scope)?;
                match mode {
                    Default      => println!("{}", value.as_joined_list_string("")),
                    NoNewline    => print!  ("{}", value.as_joined_list_string("")),
                    Spaces       => println!("{}", value.as_joined_list_string(" ")),
                    SplitNewline => println!("{}", value.as_joined_list_string("\n")),
                }
                value
            }

            // ...

            Variable(name) => self.variables[self.check_var(name, scope, pos)?].clone(),

            FragmentString(frags) => {
                ValueType::String(
                    frags.iter().map(|f| match f {
                        ParsedFragment::Expr(node) => Ok(self.run_node(node, scope)?.as_string()),
                        ParsedFragment::Literal(lit) => Ok(lit.into()),
                    }).collect::<Result<String>>()?
                ).into_value(pos)
            }

            Literal(lit) => lit.clone().into_value(pos),

            _ => todo!("remove this catch-all arm when done")
        };

        Ok(value)
    }
}
