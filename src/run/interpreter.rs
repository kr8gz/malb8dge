use std::{io::{self, Write}, process, collections::HashMap};

use crate::{util::{*, errors::Error, operators::OpType, self}, parse::{parser::Parser, ast::*}};

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
                .label(pos.clone(), format!("Variable #{name}# is not defined in this scope"))
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

        if let Some((last, rest)) = parser.statements.split_last() {
            for stmt in rest {
                self.run_node(stmt, 0)?;
            }
            let value = self.run_node(last, 0)?;
            if !matches!(last.data, NodeType::Print { .. }) {
                println!("{}", value.as_string());
            }
        }

        Ok(())
    }

    fn run_node(&mut self, node: &Node, scope: usize) -> Result<Value> {
        use NodeType::*;

        let Node { data, pos } = node;

        macro_rules! value {
            ( $type:ident($($value:expr)?) ) => {
                value!(ValueType::$type($($value)?))
            };

            ( $value:expr ) => {
                $value.into_value(pos)
            };
        }

        let value = match data {
            Statements(stmts) => {
                if let Some((last, rest)) = stmts.split_last() {
                    let inner = self.new_scope(scope);
                    for stmt in rest {
                        self.run_node(stmt, inner)?;
                    }
                    self.run_node(last, inner)?
                } else {
                    value!(Null())
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

            If { cond, on_true, on_false } => {
                let block = if self.run_node(cond, scope)?.as_bool() { on_true } else { on_false };
                if let Some(block) = block.as_ref() {
                    let inner = self.new_scope(scope);
                    self.run_node(block, inner)?
                } else {
                    value!(Null())
                }
            }

            // ...

            Function { index } => value!(Function(*index)),

            UnaryOp { target, op_type, op } => {
                value!(operators::run_unary_op(self.run_node(target, scope)?, *op_type, op, pos)?)
            }

            BinOp { a, op, b } => {
                value!(operators::run_bin_op(self.run_node(a, scope)?, self.run_node(b, scope)?, op, pos)?)
            }

            Compare { first, chain } => value!(Boolean((|| {
                let mut first = self.run_node(first, scope)?;
                for (op, second) in chain {
                    let second = self.run_node(second, scope)?;

                    let ord = first.partial_cmp(&second).ok_or_else(|| {
                        let l_type = first.type_name();
                        let r_type = second.type_name();
                        Error::err("Type error")
                            .label(first.pos.clone(), format!("This has type #{l_type}#"))
                            .label(second.pos.clone(), format!("This has type #{r_type}#"))
                            .label(
                                pos.clone(),
                                format!("Cannot compare types #{l_type}# and #{r_type}#")
                            )
                    })?;

                    let res = match op.as_str() {
                        "<" => ord.is_lt(),
                        ">" => ord.is_gt(),
                        "<=" => ord.is_le(),
                        ">=" => ord.is_ge(),
                        "!=" => ord.is_ne(),
                        "==" => ord.is_eq(),
                        _ => unreachable!()
                    };

                    if !res { return Ok(false) }
                    first = second;
                }

                Ok(true)
            })()?)),

            // ...

            Index { target, index } => {
                let index = self.run_node(index, scope)?;
                let mut i = index.data.as_int().ok_or_else(|| {
                    Error::err("Type error")
                        .label(pos.clone(), "Expected an integer for list index")
                        .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", index.as_repr_string()))
                })?;

                let target = self.run_node(target, scope)?;

                macro_rules! index {
                    ( $len:expr ) => {
                        {
                            let len = $len as f64;
                            if i < 0.0 {
                                i += len;
                            }
                            if i < 0.0 || i >= len {
                                return Err(
                                    Error::err("Index out of bounds")
                                        .label(target.pos.clone(), format!("Length of this is #{len}#"))
                                        .label(index.pos.clone(), format!("Index is #{i}#"))
                                )
                            }
                            i
                        }
                    }
                }

                match target.data {
                    ValueType::List(list) => list[index!(list.len()) as usize].clone(),
                    ValueType::String(s) => value!(String(s.chars().nth(index!(s.len()) as usize).unwrap().to_string())),
                    ValueType::Number(num) => value!(Number(index!(num))),
                    _ => return Err(
                        Error::err("Type error")
                            .label(pos.clone(), "Expected a list to index")
                            .label(target.pos.clone(), format!("Cannot convert #{}# to a list", target.as_repr_string()))
                    )
                }
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
            
            Input { prompt, mode } => {
                use InputMode::*;

                if let Some(prompt) = prompt.as_ref() {
                    print!("{}", self.run_node(prompt, scope)?.as_string());
                    io::stdout().flush().expect("hgow did not can flush prompt :>(");
                }

                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("hgow did not can read line :>(");

                util::trim_nl(&mut input);
                let value = value!(String(input));

                match mode {
                    Default => value,
                    Number => value!(operators::run_unary_op(value, OpType::After, "$", pos)?),
                    NumberList => value!(operators::run_unary_op(value, OpType::After, "#$", pos)?),
                }
            }

            Group(inner) => self.run_node(inner, scope)?,

            List(list) => value!(List(
                list.iter()
                    .map(|node| self.run_node(node, scope))
                    .collect::<Result<_>>()?
            )),

            Variable(name) => self.variables[self.check_var(name, scope, pos)?].clone(),

            FragmentString(frags) => value!(String(
                frags.iter()
                    .map(|f| match f {
                        ParsedFragment::Expr(node) => Ok(self.run_node(node, scope)?.as_string()),
                        ParsedFragment::Literal(lit) => Ok(lit.into()),
                    })
                    .collect::<Result<_>>()?
            )),

            Literal(lit) => value!(lit.clone()),

            _ => todo!("remove this catch-all arm when done")
        };

        Ok(value)
    }
}
