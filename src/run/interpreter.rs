use std::{io::{self, Write}, process, collections::HashMap};

use ariadne::{Fmt, Color};

use crate::{util::{*, errors::Error, operators::OpType}, parse::{parser::Parser, ast::*}};

use super::types::*;

#[derive(Debug)]
pub struct Interpreter {
    is_shell: bool,

    memory: Stack,
    variables: Vec<usize>,
    var_count: usize,
    scopes: Vec<Scope>,

    functions: Vec<Function>,
    call_stack: Vec<Pos>,

    _return: bool,
    _break: bool,
    _continue: bool,
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

            _return: false,
            _break: false,
            _continue: false,
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

    fn shell_green(&self, s: String) -> String {
        if self.is_shell {
            s.fg(Color::Green).to_string()
        } else {
            s
        }
    }

    pub fn run(&mut self, mut parser: Parser) -> Result<()> {
        self.functions.append(&mut parser.functions);

        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });

        if self.is_shell {
            if let Some((last, rest)) = parser.statements.split_last() {
                for stmt in rest {
                    self.run_node(stmt, 0)?;
                }
                let id = self.run_node(last, 0)?;
                let value = &self.memory[id];
                if !matches!(value.data, ValueType::Null()) {
                    println!("{}", value.as_repr_string(&self.memory).fg(Color::Fixed(14))); // light blue
                }
            }
        } else {
            for stmt in &parser.statements {
                self.run_node(stmt, 0)?;
            }
        }

        Ok(())
    }

    fn run_iter_mode(&mut self, mode: &IterMode, list: Vec<usize>, pos: &Pos) -> Result<usize> {
        use IterMode::*;

        macro_rules! push {
            ( $value:expr ) => {
                {
                    let value = $value.into_value(pos);
                    self.memory.push(value)
                }
            }
        }

        macro_rules! op_reduce {
            ( $op:literal ) => {
                match list.split_first() {
                    Some((first, rest)) => {
                        rest.iter().copied()
                            .try_fold(*first, |a, b| {
                                let value = operators::run_bin_op(&mut self.memory, a, b, $op, pos)?;
                                Ok(push!(value))
                            })?
                    }
                    None => push!(ValueType::Null())
                }
            }
        }

        Ok(match mode {
            Sum => op_reduce!("+"),
            Product => op_reduce!("*"),
            All => op_reduce!("&"),
            AllBool => op_reduce!("&&"),
            Any => op_reduce!("|"),
            AnyBool => op_reduce!("||"),

            AllEqual => push!(ValueType::Boolean(crate::unique!(self.memory, list).len() == 1)),
            AllUnequal => push!(ValueType::Boolean(list.len() == crate::unique!(self.memory, list).len())),
            
            Min => op_reduce!(".*"),
            Max => op_reduce!("^*"),

            MostFreq => {
                let mut counts = HashMap::new();
                for id in list {
                    *counts.entry(id).or_insert(0) += 1;
                }
                counts.into_iter().max_by_key(|(_, count)| *count).map(|(id, _)| id).unwrap_or(0)
            }

            Default | Map => push!(ValueType::List(list)),
            Unique => push!(ValueType::List(crate::unique!(self.memory, list))),
            
            Print => {
                for id in list {
                    let value = self.memory[id].as_joined_list_string(&self.memory, "");
                    println!("{}", self.shell_green(value));
                }
                push!(ValueType::Null())
            }
            PrintNoSpaces => {
                for id in list {
                    let value = self.memory[id].as_joined_list_string(&self.memory, "");
                    print!("{}", self.shell_green(value));
                }
                push!(ValueType::Null())
            }

            SortAsc => {
                // list.sort_by(|a, b| self.memory[a].compare(&self.memory[b], pos));
                // push!(ValueType::List(list))
                todo!("sort asc")
            }

            SortDesc => {
                // list.sort_by(|a, b| self.memory[a].compare(&self.memory[b], pos));
                // list.reverse();
                // push!(ValueType::List(list))
                todo!("sort desc")
            }

            Filter => {
                push!(ValueType::List(list.into_iter().filter(|&v| self.memory[v].as_bool()).collect()))
            }
        })
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
            ( $var:ident ) => {
                let $var = self.run_node($var, scope)?;
            };

            ( $var:ident or null ) => {
                match $var.as_ref() {
                    Some($var) => self.run_node($var, scope)?,
                    None => push!(ValueType::Null())
                }
            };
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

            Return(expr) => {
                let ret = run!(expr or null);
                self._return = true;
                ret
            }

            Break(expr) => {
                let ret = run!(expr or null);
                self._break = true;
                ret
            }

            Continue(expr) => {
                let ret = run!(expr or null);
                self._continue = true;
                ret
            }

            Exit(expr) => {
                if let Some(expr) = expr.as_ref() {
                    run!(expr);
                    println!("{}", self.shell_green(self.id_to_str(expr)));
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
                let value = self.memory[value].clone();
                let values = value.as_list(&mut self.memory, pos).ok_or_else(|| {
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

            Loop { mode, block } => {
                let mut ret = Vec::new();
                loop {
                    let value = self.run_node(block, scope)?;
                    ret.push(value);

                    if self._break {
                        break self.run_iter_mode(mode, ret, pos)?
                    } else if self._return {
                        break value
                    }
                }
            }

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

                        let ord = first_value.compare(&second_value, pos)?;
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

            BraceIter { target, mode } => {
                run!(target);
                let target = self.memory[target].clone();
                let list = target.as_list(&mut self.memory, pos).ok_or_else(|| {
                    Error::err("Type error")
                        .label(pos.clone(), "Expected a list for brace iterating thing")
                        .label(target.pos.clone(), format!("Cannot convert #{}# to a list", target.as_repr_string(&self.memory)))
                })?;
                self.run_iter_mode(mode, list, pos)?
            }

            // ...

            Print { value, mode } => {
                use PrintMode::*;
                let value = self.run_node(value, scope)?;

                macro_rules! fmt {
                    ( $sep:literal ) => { self.shell_green(self.memory[value].as_joined_list_string(&self.memory, $sep)) }
                }

                let formatted_value = match mode {
                    Default      => fmt!(""),
                    NoNewline    => fmt!(""),
                    Spaces       => fmt!(" "),
                    SplitNewline => fmt!("\n"),
                };
                
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
                    print!("{}", self.shell_green(self.id_to_str(prompt)));
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
