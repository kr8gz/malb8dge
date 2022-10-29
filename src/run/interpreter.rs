use std::{io::{self, Write}, process, collections::HashMap, cmp::Ordering};

use ariadne::{Fmt, Color};
use itertools::Itertools;
use rand::Rng;

use crate::{util::{*, errors::Error, operators::OpType}, parse::{parser::Parser, ast::*}};

use super::types::*;

macro_rules! _print {
    ( $interpreter:ident, $($arg:tt)* ) => {
        {
            print!($($arg)*);
            $interpreter.no_nl = true;
        }
    }
}

macro_rules! _println {
    ( $interpreter:ident, $($arg:tt)* ) => {
        {
            println!($($arg)*);
            $interpreter.no_nl = false;
        }
    }
}

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

    no_nl: bool,
}

impl Interpreter {
    pub fn new(is_shell: bool) -> Self {
        Self {
            is_shell,

            memory: Stack::new(),
            variables: Vec::new(),
            var_count: 0,
            scopes: vec![
                Scope {
                    vars: HashMap::new(),
                    parent: None,
                    children: Vec::new(),
                }
            ],

            functions: Vec::new(),
            call_stack: Vec::new(),

            _return: false,
            _break: false,
            _continue: false,

            no_nl: false,
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

    fn check_var(&mut self, name: &str, pos: &Pos, scope: usize) -> Result<usize> {
        self.get_var(name, pos, scope).ok_or_else(|| {
            let mut msg = format!("Variable #{name}# is not defined");
            if self.scopes.iter().any(|scope| scope.vars.get(name).is_some()) {
                msg += " in this scope";
            }
            Error::err("Use of undefined variable").label(pos.clone(), msg)
        })
    }

    fn get_var(&mut self, name: &str, pos: &Pos, scope: usize) -> Option<usize> {
        let mut scope_data = &self.scopes[scope];
        loop {
            match scope_data.vars.get(name) {
                Some(var_id) => return Some(*var_id),
                None => match scope_data.parent {
                    Some(scope_id) => scope_data = &self.scopes[scope_id],
                    None => return (name == "&").then(|| self.new_var(name, pos, scope))
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

        if self.is_shell {
            if let Some((last, rest)) = parser.statements.split_last() {
                for stmt in rest {
                    self.run_node(stmt, 0)?;
                }
                
                let id = self.run_node(last, 0)?;
                let value = &self.memory[id];

                if self.no_nl {
                    println!();
                    self.no_nl = false;
                }

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
                                let value = self.run_bin_op(a, b, $op, pos)?;
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
                let mut values = Vec::new();
                let mut counts = Vec::new();
                for id in list {
                    let value = &self.memory[id].data;
                    match values.iter().position(|&v| v == value) {
                        Some(pos) => counts[pos] += 1,
                        None => {
                            values.push(value);
                            counts.push(1);
                        }
                    }
                }

                push!(
                    values
                        .into_iter()
                        .zip(counts)
                        .max_by_key(|(_, count)| *count)
                        .map(|(value, _)| value.clone())
                        .unwrap_or(ValueType::Null())
                )
            }

            Default | Map => push!(ValueType::List(list)),
            Unique => push!(ValueType::List(crate::unique!(self.memory, list))),
            
            Print => {
                for id in list {
                    let value = self.memory[id].as_joined_list_string(&self.memory, "");
                    _println!(self, "{}", self.shell_green(value));
                }
                push!(ValueType::Null())
            }
            PrintNoSpaces => {
                for id in list {
                    let value = self.memory[id].as_joined_list_string(&self.memory, "");
                    _print!(self, "{}", self.shell_green(value));
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
                        let id = self.get_var(name, pos, scope).unwrap_or_else(|| self.new_var(name, pos, scope));
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
                    _println!(self, "{}", self.shell_green(self.id_to_str(expr)));
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
                        let id = self.check_var(name, pos, scope)?;
                        self.variables[id] = push!(self.run_bin_op(self.variables[id], value, op, pos)?);
                        self.variables[id]
                    }

                    NodeType::Index { target, index } => {
                        run!(target);
                        run!(index);
                        let index = index!(target, index);
                        let mut target_value = self.memory[target].clone();

                        let ret = match &mut target_value.data {
                            ValueType::List(list) => {
                                list[index] = push!(self.run_bin_op(list[index], value, op, pos)?);
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
                push!(self.run_unary_op(target, *op_type, op, pos)?)
            }

            BinOp { a, op, b } => {
                let a = self.run_node(a, scope)?;
                let b = self.run_node(b, scope)?;
                push!(self.run_bin_op(a, b, op, pos)?)
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
                        let id = self.check_var(name, pos, scope)?;
                        let ret = self.variables[id];
                        self.variables[id] = push!(self.run_bin_op(self.variables[id], value, op, pos)?);
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
                                list[index] = push!(self.run_bin_op(list[index], value, op, pos)?);
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
                    NoNewline => _print!(self, "{formatted_value}"),
                    _ => _println!(self, "{formatted_value}")
                }

                value
            }
            
            Input { prompt, mode } => {
                use InputMode::*;

                if let Some(prompt) = prompt.as_ref() {
                    run!(prompt);
                    _print!(self, "{}", self.shell_green(self.id_to_str(prompt)));
                    io::stdout().flush().expect("hgow did not can flush prompt :>(");
                }

                let mut input = String::new();
                io::stdin().read_line(&mut input).expect("hgow did not can read line :>(");
                let value = ValueType::String(input.lines().next().unwrap().into());

                macro_rules! op {
                    ( $op:literal ) => {
                        {
                            let value = push!(value);
                            self.run_unary_op(value, OpType::After, $op, pos)?
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

            Variable(name) => {
                let id = self.check_var(name, pos, scope)?;
                self.variables[id]
            }

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

    fn run_unary_op(&mut self, target_id: usize, op_type: OpType, op: &str, pos: &Pos) -> Result<ValueType> {
        use ValueType::*;
    
        let mut target = self.memory[target_id].clone();
                
        let repr = match op_type {
            OpType::Before => format!("{}x", op),
            OpType::After => format!("x{}", op),
            _ => panic!("specified type isn't a unary operator")
        };
    
        macro_rules! unary_ops {
            (
                $(
                    $impl_op:literal {
                        $(
                            $( % convert $from:pat => $to:expr; )+
                        )?
                        $( $a:pat => $ret:expr; )*
                    }
                )*
            ) => {
    
                match op {
                    $(
                        $impl_op => {
                            $(
                                match &target.data {
                                    $( $from => target.data = $to, )+
                                    _ => ()
                                }
                            )?
    
                            match target.data {
                                $( $a => $ret, )*
                                #[allow(unreachable_patterns)] // not every operator matches all types
                                _ => {
                                    let type_name = self.memory[target_id].type_name();
                                    return Err(
                                        Error::err("Type error")
                                            .label(target.pos.clone(), format!("This has type #{type_name}#"))
                                            .label(pos.clone(), format!("#{repr}# is not implemented for type #{type_name}#"))
                                    )
                                }
                            }
                        }
                    )*
                    _ => unimplemented!("{repr}")
                }
            }
        }
    
        let ret = match op_type {
            OpType::Before => unary_ops! {
                "!" {
                    a   =>  Boolean(!a.as_bool());
                }
    
                "^" {
                    a   =>  List({
                                let i = a.as_int().ok_or_else(|| {
                                    Error::err("Type error")
                                        .label(pos.clone(), format!("Expected an integer for #{repr}#"))
                                        .label(target.pos.clone(), format!("Cannot convert #{}# to an integer", a.as_repr_string(&self.memory)))
                                })? as i64;
                                if i < 0 { i+1..1 } else { 0..i }
                                    .map(|i| self.memory.push(ValueType::Number(i as f64).into_value(pos)))
                                    .collect()
                            });
                }
    
                "-" {
                    a   =>  Number(-a.as_num().ok_or_else(|| {
                                Error::err("Type error")
                                    .label(pos.clone(), format!("Expected a number for {op}x"))
                                    .label(target.pos.clone(), format!("Cannot convert #{}# to a number", a.as_repr_string(&self.memory)))
                            })?);
                }
    
                "." {
                    
                }
    
                "`" {
    
                }
    
                "``" {
    
                }
    
                "*" {
                    a => {
                        let id = self.get_var("&", pos, 0).unwrap();
                        self.variables[id] = target_id;
                        a
                    };
                }
    
                "@" {
                    % convert Boolean(_) | Null() => String(target.as_repr_string(&self.memory));
    
                    String(a)   => String(a.reverse());
                    List(mut a) => List({ a.reverse(); a });
                    Number(a)   => Number(a.abs().to_string().reverse().parse::<f64>().unwrap().copysign(a));
                }
    
                "^^" {
    
                }
    
                "#" {
                    % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                    Number(a) => Number(a.abs());
                }
    
                "'" {
    
                }
            },
    
            OpType::After => unary_ops! {
                "?\\" {
                    % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                    Number(a) => Number(rand::thread_rng().gen_range({ let a = a.abs() as i64; 1.min(a)..=1.max(a) }) as f64);
                }

                "^^" {
    
                }
    
                "##" {
    
                }
    
                "#\\" {
    
                }
    
                "#$" {
    
                }
    
                "_" {
                    % convert Boolean(_) | Null() => String(target.as_string(&self.memory));
    
                    String(a)   =>  Number(a.len() as f64);
                    List(a)     =>  Number(a.len() as f64);
                    Number(a)   =>  Number(a.to_string().len() as f64);
                }
    
                "``" {
    
                }
    
                "$$" {
    
                }
    
                "$" {
                    a   =>  Number(a.as_num().ok_or_else(|| Error::err("Value error")
                                .label(pos.clone(), "Found conversion to number")
                                .label(target.pos, format!("Cannot convert #{}# to a number", a.as_repr_string(&self.memory)))
                            )?);
                }
    
                "'" {
                    % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                    Number(a) => Number(a.round());
                }
    
                "`" {
                    % convert a @ (Boolean(_) | Null()) => Number(a.as_int().unwrap());
                    Number(a) => Number(a.trunc());
                }
            },
    
            _ => unreachable!()
        };
    
        Ok(ret)
    }
    
    fn run_bin_op(&mut self, lhs_id: usize, rhs_id: usize, op: &str, pos: &Pos) -> Result<ValueType> {
        use ValueType::*;
    
        let mut lhs = self.memory[lhs_id].clone();
        let mut rhs = self.memory[rhs_id].clone();
    
        macro_rules! push {
            ( $value:expr ) => { self.memory.push($value.into_value(pos)) }
        }
    
        macro_rules! bin_ops {
            (
                $(
                    $op:literal {
                        $(
                            % convert
                            $( $from:pat => $to:expr; )+
                        )?
                        $(
                            % one way
                            $( $a1:pat, $b1:pat => $ret1:expr; )+
                        )?
                        $(
                            % both ways
                            $( $a2:pat, $b2:pat => $ret2:expr; )+
                        )?
                    }
                )*
            ) => {
                match op {
                    $(
                        $op => {
                            $(
                                match &lhs.data {
                                    $( $from => lhs.data = $to, )+
                                    _ => ()
                                }
                                match &rhs.data {
                                    $( $from => rhs.data = $to, )+
                                    _ => ()
                                }
                            )?
    
                            match (lhs.data.clone(), rhs.data.clone()) {
                                $(
                                    $(
                                        ($a1, $b1) => $ret1,
                                    )+
                                )?
                                $(
                                    $(
                                        ($a2, $b2) => $ret2,
                                        
                                        #[allow(unused_assignments)] // they can be used in the op implementations
                                        ($b2, $a2) => {
                                            (lhs, rhs) = (rhs, lhs);
                                            $ret2
                                        }
                                    )+
                                )?
                                #[allow(unreachable_patterns)] // not every operator matches all types
                                _ => {
                                    let l_type = self.memory[lhs_id].type_name();
                                    let r_type = self.memory[rhs_id].type_name();
                                    return Err(
                                        Error::err("Type error")
                                            .label(lhs.pos, format!("This has type #{l_type}#"))
                                            .label(rhs.pos, format!("This has type #{r_type}#"))
                                            .label(pos.clone(), format!("Binary #{op}# is not implemented for types #{l_type}# and #{r_type}#"))
                                    )
                                }
                            }
                        }
                    )*
                    _ => unimplemented!("a {op} b")
                }
            }
        }
    
        #[macro_export]
        macro_rules! unique {
            ( $memory:expr, $iter:expr ) => {
                {
                    let mut seen = Vec::new();
                    let mut unique = Vec::new();
                    for el in $iter {
                        if !seen.contains(&$memory[el].data) {
                            unique.push(el);
                            seen.push($memory[el].data.clone());
                        }
                    }
                    unique
                }
            }
        }
    
        let ret = bin_ops! {
            "||" {
                % one way
                a, b => Boolean(a.as_bool() || b.as_bool());
            }
        
            "&&" {
                % one way
                a, b => Boolean(a.as_bool() && b.as_bool());
            }
        
            "|" {
                % one way
                a, b => if a.as_bool() { a } else { b };
            }
            
            "&" {
                % one way
                a, b => if a.as_bool() { b } else { a };
            }
        
            "-?" {
                // something with 1 and -1 lol
            }
        
            ".." {
        
            }
        
            "#" {
        
            }
        
            "?\\" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   => Number(rand::thread_rng().gen_range(a.min(b) as i64..=a.max(b) as i64) as f64);
            }
        
            "%" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number(a % b);
            }
        
            "+" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number(a + b);
                List(a),    List(b)     =>  List(a.into_iter().chain(b.into_iter()).collect());
                String(a),  String(b)   =>  String(a + &b);
                String(a),  b @ List(_) =>  String(a + &b.as_joined_list_string(&self.memory, ""));
                
                List(a),    _           =>  List(a.into_iter().chain([rhs_id]).collect());
                String(a),  _           =>  String(a + &rhs.as_string(&self.memory));
                _,          String(b)   =>  String(lhs.as_string(&self.memory) + &b);
            }
        
            "-" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                List(mut a), List(b)    =>  List({
                                                for b_id in b {
                                                    if let Some(pos) = a.iter().position(|&el| self.memory[el].data == self.memory[b_id].data) {
                                                        a.remove(pos);
                                                    }
                                                };
                                                a
                                            });
                List(mut a), _          =>  List({
                                                if let Some(pos) = a.iter().position(|&el| self.memory[el].data == self.memory[rhs_id].data) {
                                                    a.remove(pos);
                                                }
                                                a
                                            });
        
                String(a),  String(b)   =>  String({
                                                if a.len() == 1 && b.len() == 1 {
                                                    (a.chars().next().unwrap()..=b.chars().next().unwrap()).collect()
                                                } else {
                                                    return Err(
                                                        Error::err("Value error")
                                                            .label(pos.clone(), "Expected strings with #length 1# for character range")
                                                            .label(lhs.pos, format!("This string has length #{}#", a.len()))
                                                            .label(rhs.pos, format!("This string has length #{}#", b.len()))
                                                    )
                                                }
                                            });
        
                String(a),  Number(b)   =>  String({
                                                if b >= 0.0 {
                                                    a.chars().dropping_back(b as usize).collect()
                                                } else {
                                                    return Err(
                                                        Error::err("Value error")
                                                            .label(
                                                                pos.clone(),
                                                                format!(
                                                                    "Expected #number >= 0# for right side of {} {op} {}",
                                                                    lhs.type_name(), rhs.type_name()
                                                                ),
                                                            )
                                                            .label(rhs.pos, format!("{b} is not >= 0"))
                                                    )
                                                }
                                            });
        
                Number(a),  Number(b)   =>  Number(a - b);
            }
        
            "*" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number(a * b);
                List(a),    List(b)     =>  List(unique!(self.memory, a.into_iter().chain(b.into_iter())));
        
                % both ways
                List(a),    Number(b)   =>  List({
                                                let mut res = Vec::new();
                                                for _ in 0..b.abs() as usize {
                                                    res.extend(a.iter().map(|&v| self.memory.push(self.memory[v].clone())));
                                                }
                                                if b < 0.0 { res.reverse(); }
                                                res
                                            });
                
                String(mut a), Number(b) => String({
                                                a = a.repeat(b.abs() as usize);
                                                if b < 0.0 { a = a.reverse(); }
                                                a
                                            });
            }
        
            "/" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number(a / b);
                List(a),    List(b)     =>  List(unique!(self.memory, a.iter().chain(b.iter()).cloned().filter(|id| a.contains(id) && b.contains(id))));
            }
        
            "//" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number((a / b).trunc());
            }
        
            "+-" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  List(vec![push!(Number(a + b)), push!(Number(a - b))]);
            }
        
            "/%" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  List(vec![push!(Number((a / b).trunc())), push!(Number(a % b))]);
            }
        
            "^" {
                % convert
                x @ (Boolean(_) | Null()) => Number(x.as_int().unwrap());
        
                % one way
                Number(a),  Number(b)   =>  Number(a.powf(b));
                List(a),    List(b)     =>  List(unique!(self.memory, a.iter().chain(b.iter()).cloned().filter(|id| a.contains(id) != b.contains(id))));
                
                List(a),    String(b)   =>  String(a.into_iter().map(|el| self.memory[el].as_string(&self.memory)).join(&b));
                String(a),  String(b)   =>  String(a.chars().map(|c| c.to_string()).intersperse(b).collect());
            }
        
            ".*" {
                % one way
                a, b => match lhs.compare(&rhs, pos)? {
                    Ordering::Less | Ordering::Equal => a,
                    _ => b
                };
            }
        
            "^*" {
                % one way
                a, b => match lhs.compare(&rhs, pos)? {
                    Ordering::Greater => a,
                    _ => b
                };
            }
        
            "@" {
        
            }
        };
    
        Ok(ret)
    }
}
