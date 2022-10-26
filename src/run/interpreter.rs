use std::{io::{self, Write}, process, collections::HashMap};

use ariadne::{Fmt, Color};

use crate::{util::{*, errors::Error, operators::OpType}, parse::{parser::Parser, ast::*}};

use super::types::*;

const GREEN: Color = Color::Fixed(2);

#[derive(Debug)]
pub struct Interpreter {
    is_shell: bool,

    memory: Stack,
    variables: Vec<usize>,
    var_count: usize,
    scopes: Vec<Scope>,

    functions: Vec<Function>,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn new(is_shell: bool) -> Self {
        Self {
            is_shell,

            memory: Stack::new(),
            variables: Vec::new(),
            var_count: 0,
            scopes: Vec::new(),

            functions: Vec::new(),
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
            let mut msg = format!("Variable #{name}# is not defined");
            if self.scopes.iter().any(|scope| scope.vars.get(name).is_some()) {
                msg += " in this scope";
            }
            Error::err("Use of undefined variable").label(pos.clone(), msg)
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

    fn new_var(&mut self, name: &str, pos: &Pos, scope: usize) -> usize {
        let id = self.var_count;
        self.var_count += 1;
        self.scopes[scope].vars.insert(name.into(), id);
        self.variables.push(self.memory.push(ValueType::Null().into_value(pos)));
        id
    }

    fn id_to_str(&self, id: usize) -> String {
        self.memory[id].as_string(&self.memory)
    }

    pub fn run(&mut self, mut parser: Parser) -> Result<()> {
        self.functions.append(&mut parser.functions);

        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });

        if let Some((last, rest)) = parser.statements.split_last() {
            for stmt in rest {
                self.run_node(stmt, 0)?;
            }

            let id = self.run_node(last, 0)?;
            let mut value = self.id_to_str(id);
            if !matches!(last.data, NodeType::Print { .. }) {
                if self.is_shell {
                    value = value.fg(GREEN).to_string()
                };
                println!("{value}");
            }
        }

        Ok(())
    }

    fn run_node(&mut self, node: &Node, scope: usize) -> Result<usize> {
        use NodeType::*;

        let Node { data, pos } = node;

        macro_rules! push {
            ( $value:expr ) => {
                {
                    let value = $value.into_value(pos);
                    self.memory.push(value)
                }
            }
        }

        macro_rules! run {
            ( $var:ident ) => { let $var = self.run_node($var, scope)?; }
        }
        
        macro_rules! index {
            ( $target:expr, $index:expr ) => {
                {
                    let target = &self.memory[$target];
                    let index = &self.memory[$index];

                    let mut i = index.data.as_int().ok_or_else(|| {
                        Error::err("Type error")
                            .label(pos.clone(), "Expected an integer for list index")
                            .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", index.as_repr_string(&self.memory)))
                    })?;

                    let len = match &target.data {
                        ValueType::List(list) => list.len() as f64,
                        ValueType::String(s) => s.len() as f64,
                        _ => return Err(
                            Error::err("Type error")
                                .label(pos.clone(), "Expected a list to index")
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a list", target.as_repr_string(&self.memory)))
                        )
                    };
                    
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
                    i as usize
                }
            }
        }

        macro_rules! assign {
            ( $target:ident, $value:expr ) => {
                let value = $value;

                match &$target.data {
                    NodeType::Variable(name) => {
                        let id = self.get_var(name, scope).unwrap_or_else(|| self.new_var(name, pos, scope));
                        self.variables[id] = value;
                    }

                    NodeType::Index { target, index } => {
                        run!(target);
                        run!(index);
                        let index = index!(target, index);

                        let mut target_value = self.memory[target].clone();
                        match &mut target_value.data {
                            ValueType::List(list) => list[index] = value,
                            ValueType::String(s) => {
                                s.replace_range(
                                    s.char_indices()
                                        .nth(index)
                                        .map(|(pos, ch)| pos..pos + ch.len_utf8())
                                        .unwrap(),
                                    &self.id_to_str(value),
                                );
                            }
                            _ => unreachable!()
                        };

                        self.memory[target] = target_value;
                    }

                    _ => unreachable!()
                }
            }
        }

        Ok(match data {
            Statements(stmts) => {
                if let Some((last, rest)) = stmts.split_last() {
                    let inner = self.new_scope(scope);
                    for stmt in rest {
                        self.run_node(stmt, inner)?;
                    }
                    self.run_node(last, inner)?
                } else {
                    push!(ValueType::Null())
                }
            }

            // ...

            Exit(expr) => {
                if let Some(expr) = expr.as_ref() {
                    run!(expr);
                    println!("{}", self.id_to_str(expr));
                }
                process::exit(0)
            }

            Assign { target, value } => {
                run!(value);
                assign!(target, value);
                value
            }

            AugmentedAssign { target, op, value } => {
                let value = self.run_node(value, scope)?;

                match &target.data {
                    NodeType::Variable(name) => {
                        let id = self.check_var(name, scope, pos)?;
                        self.variables[id] = push!(operators::run_bin_op(&mut self.memory, self.variables[id], value, op, pos)?);
                        self.variables[id]
                    }

                    NodeType::Index { target, index } => {
                        run!(target);
                        run!(index);
                        let index = index!(target, index);
                        let mut target_value = self.memory[target].clone();

                        let ret = match &mut target_value.data {
                            ValueType::List(list) => {
                                list[index] = push!(operators::run_bin_op(&mut self.memory, list[index], value, op, pos)?);
                                list[index]
                            }

                            _ => {
                                let type_name = target_value.type_name();
                                return Err(
                                    Error::err("Type error")
                                        .label(target_value.pos, format!("This has type #{type_name}#"))
                                        .label(pos.clone(), format!("Cannot use augmented assignment on index of type #{type_name}#"))
                                )
                            }
                        };

                        self.memory[target] = target_value;
                        ret
                    }

                    _ => unreachable!()
                }
            }

            MultipleAssign { targets, targets_pos, value } => {
                run!(value);
                let value = &self.memory[value];
                let values = value.as_list(&self.memory, pos).ok_or_else(|| {
                    Error::err("Type error")
                        .label(pos.clone(), "Expected a list to unpack")
                        .label(value.pos.clone(), format!("Cannot convert #{}# to a list", value.as_repr_string(&self.memory)))
                })?;

                if targets.len() != values.len() {
                    return Err(
                        Error::err("Value error")
                            .label(pos.clone(), "Expected lists of equal length for multiple assignment")
                            .label(targets_pos.clone(), format!("This has length #{}#", targets.len()))
                            .label(value.pos.clone(), format!("This has length #{}#", values.len()))
                    )
                }

                let mut list = Vec::new();
                for (target, value) in targets.iter().zip(values) {
                    let value = self.memory.push(value);
                    assign!(target, value);
                    list.push(value);
                }

                push!(ValueType::List(list))
            }

            // ...

            If { cond, on_true, on_false } => {
                let cond = self.run_node(cond, scope)?;
                let block = if self.memory[cond].as_bool() { on_true } else { on_false };
                if let Some(block) = block.as_ref() {
                    let inner = self.new_scope(scope);
                    self.run_node(block, inner)?
                } else {
                    push!(ValueType::Null())
                }
            }

            // ...

            Function { index } => push!(ValueType::Function(*index)),

            UnaryOp { target, op_type, op } => {
                let target = self.run_node(target, scope)?;
                push!(operators::run_unary_op(&mut self.memory, target, *op_type, op, pos)?)
            }

            BinOp { a, op, b } => {
                let a = self.run_node(a, scope)?;
                let b = self.run_node(b, scope)?;
                push!(operators::run_bin_op(&mut self.memory, a, b, op, pos)?)
            }

            Compare { first, chain } => {
                let res = (|| {
                    run!(first);
                    let mut first_value = self.memory[first].clone();

                    for (op, second) in chain {
                        run!(second);
                        let second_value = self.memory[second].clone();

                        let ord = first_value.partial_cmp(&second_value).ok_or_else(|| {
                            let l_type = first_value.type_name();
                            let r_type = second_value.type_name();
                            Error::err("Type error")
                                .label(first_value.pos.clone(), format!("This has type #{l_type}#"))
                                .label(second_value.pos.clone(), format!("This has type #{r_type}#"))
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
                        first_value = second_value;
                    }

                    Ok(true)
                })()?;

                push!(ValueType::Boolean(res))
            }

            Increment { target, mode } => {
                let value = push!(ValueType::Number(1.0));
                let op = if mode.add() { "+" } else { "-" };

                match &target.data {
                    NodeType::Variable(name) => {
                        let id = self.check_var(name, scope, pos)?;
                        let ret = self.variables[id];
                        self.variables[id] = push!(operators::run_bin_op(&mut self.memory, self.variables[id], value, op, pos)?);
                        if mode.aft() { ret } else { self.variables[id] }
                    }

                    NodeType::Index { target, index } => {
                        run!(target);
                        run!(index);
                        let index = index!(target, index);
                        let mut target_value = self.memory[target].clone();

                        let ret = match &mut target_value.data {
                            ValueType::List(list) => {
                                let ret = list[index];
                                list[index] = push!(operators::run_bin_op(&mut self.memory, list[index], value, op, pos)?);
                                if mode.aft() { ret } else { list[index] }
                            }

                            _ => {
                                let type_name = target_value.type_name();
                                return Err(
                                    Error::err("Type error")
                                        .label(target_value.pos, format!("This has type #{type_name}#"))
                                        .label(
                                            pos.clone(),
                                            format!("Cannot {}crement index of type #{type_name}#", if mode.add() { "in" } else { "de" })
                                        )
                                )
                            }
                        };

                        self.memory[target] = target_value;
                        ret
                    }

                    _ => unreachable!()
                }
            }

            // FnCall { target, args } => {
            //    
            // }

            Index { target, index } => {
                run!(target);
                run!(index);
                let index = index!(target, index);

                match &self.memory[target].data {
                    ValueType::List(list) => list[index],
                    ValueType::String(s) => push!(ValueType::String(s.chars().nth(index).unwrap().into())),
                    _ => unreachable!()
                }
            }

            // ...

            Print { value, mode } => {
                use PrintMode::*;
                let value = self.run_node(value, scope)?;

                macro_rules! fmt {
                    ( $sep:literal ) => { self.memory[value].as_joined_list_string(&self.memory, $sep) }
                }

                let mut formatted_value = match mode {
                    Default      => fmt!(""),
                    NoNewline    => fmt!(""),
                    Spaces       => fmt!(" "),
                    SplitNewline => fmt!("\n"),
                };

                if self.is_shell {
                    formatted_value = formatted_value.fg(GREEN).to_string();
                }
                
                match mode {
                    NoNewline => print!("{formatted_value}"),
                    _ => println!("{formatted_value}")
                }

                value
            }
            
            Input { prompt, mode } => {
                use InputMode::*;

                if let Some(prompt) = prompt.as_ref() {
                    run!(prompt);
                    print!("{}", self.id_to_str(prompt));
                    io::stdout().flush().expect("hgow did not can flush prompt :>(");
                }

                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("hgow did not can read line :>(");
                let value = ValueType::String(input.lines().next().unwrap().into());

                macro_rules! op {
                    ( $op:literal ) => {
                        {
                            let value = push!(value);
                            operators::run_unary_op(&mut self.memory, value, OpType::After, $op, pos)?
                        }
                    }
                }

                push!(match mode {
                    Default => value,
                    Number => op!("$"),
                    NumberList => op!("#$"),
                })
            }

            Group(inner) => self.run_node(inner, scope)?,

            List(list) => push!(ValueType::List(
                list.iter()
                    .map(|node| self.run_node(node, scope))
                    .collect::<Result<_>>()?
            )),

            Variable(name) => self.variables[self.check_var(name, scope, pos)?],

            FragmentString(frags) => push!(ValueType::String(
                frags.iter()
                    .map(|f| match f {
                        ParsedFragment::Literal(lit) => Ok(lit.into()),
                        ParsedFragment::Expr(node) => {
                            run!(node);
                            Ok(self.id_to_str(node))
                        }
                    })
                    .collect::<Result<_>>()?
            )),

            Literal(lit) => push!(lit.clone()),

            _ => todo!("remove this catch-all arm when done")
        })
    }
}
