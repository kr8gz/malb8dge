#![allow(unstable_name_collisions)] // i just want to use intersperse

use std::{io::{self, Write}, process};

use ariadne::{Fmt, Color::*};

use itertools::Itertools;

use crate::{
    compile::{compiler::Compiler, instructions::*},
    util::{*, errors::{self, Error}, Pos, operators::{self, *}},
    parse::ast::*
};

use super::types::*;

pub struct Interpreter {
    constants: Vec<ValueType>,
    functions: Vec<Function>,
    memory: Stack<Value>,

    vars: Vec<Option<usize>>,
    stack: Vec<usize>,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn run(compiler: Compiler) -> Result<()> {
        Self {
            constants: compiler.constants.0,
            functions: compiler.functions,
            memory: Stack::new(),

            vars: vec![None; compiler.var_count],
            stack: Vec::new(),
            call_stack: Vec::new(),
        }.run_func(0)?;

        // TODO implicitly print last returned?

        Ok(())
    }

    fn to_int(&self, value: &ValueType) -> Option<f64> {
        use ValueType::*;
        match value {
            List(list) => Some(list.len() as f64),
            String(s) => s.parse::<f64>().ok().map(|n| n.floor()),
            Number(num) => Some(num.floor()),
            Boolean(true) => Some(1.0),
            Boolean(false) | Null() => Some(0.0),
            _ => None
        }
    }

    fn to_num(&self, value: &ValueType) -> Option<f64> {
        use ValueType::*;
        match value {
            Number(num) => Some(*num),
            _ => self.to_int(value)
        }
    }

    fn to_bool(&self, value: &ValueType) -> bool {
        use ValueType::*;
        match value {
            List(list) => !list.is_empty(),
            String(s) => !s.is_empty(),
            Number(num) => *num != 0.0,
            Boolean(_) | Function(_) => true,
            Null() => false,
        }
    }

    fn to_string(&self, value: &ValueType) -> String {
        use ValueType::*;
        match value {
            Function(_) => "<function>".into(),
            List(list) => {
                format!("[{}]", list.iter().map(|&v| self.repr_string(&self.memory[v].data)).collect::<Vec<_>>().join(", "))
            },
            Boolean(b) => b.to_string(),
            String(s) => s.clone(),
            Number(num) => num.to_string(),
            Null() => "".into(),
        }
    }

    fn repr_string(&self, value: &ValueType) -> String {
        use ValueType::*;
        match value {
            String(s) => format!("\"{s}\""),
            Null() => "null".into(),
            _ => self.to_string(value),
        }
    }

    fn join_list(&self, list: &[usize], sep: &str) -> String {
        list.iter()
            .map(|&v| self.to_string(&self.memory[v].data))
            .join(sep)
    }

    fn run_func(&mut self, func: usize) -> Result<Option<usize>> {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        let mut returned = None;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            macro_rules! push {
                ( Value, $value:expr ) => {
                    {
                        let value = $value; // most confusing error ever lmao
                        let id = self.memory.push(value);
                        push!(Id, id);
                        id
                    }
                };

                ( Id, $id:expr ) => {
                    self.stack.push($id)
                };
                
                ( $type:ident $(, $data:expr)? ) => {
                    push!(Value, ValueType::$type( $($data)? ).into_value(&instr.pos))
                };
            }

            macro_rules! pop {
                ( Value ) => {
                    &self.memory[pop!(Id)]
                };

                ( Id ) => {
                    self.stack.pop().unwrap()
                };
            }

            macro_rules! value {
                ( StackTop ) => {
                    value!(*self.stack.last().unwrap())
                };

                ( $id:expr ) => {
                    &self.memory[$id].data
                };
            }

            macro_rules! clone {
                ( $id:expr ) => {
                    if let ValueType::List(list) = value!($id) {
                        push!(List, list.clone())
                    } else {
                        $id
                    }
                }
            }

            macro_rules! unary_ops {
                (
                    $target:ident, $op:ident, $type:ident
                    $(
                        $impl_op:literal {
                            $(
                                $( % convert $from:pat => $to:expr; )+
                            )?
                            $( $a:pat => $ret:expr; )*
                        }
                    )*
                ) => {
                    use ValueType::*;
                    
                    let repr = match OpType::$type {
                        OpType::Before => format!("{}x", $op),
                        OpType::After => format!("x{}", $op),
                        _ => panic!("specified type isn't a unary operator")
                    };

                    let err = Error::err("Type error")
                        .label($target.pos.clone(), format!("This has type #{}#", $target.type_name()))
                        .label(
                            instr.pos.clone(),
                            format!("#{repr}# is not implemented for type #{}#", $target.type_name())
                        );

                    match $op {
                        $(
                            $impl_op => {
                                $(
                                    match &$target.data {
                                        $( $from => $target = $to.into_value(&$target.pos), )+
                                        _ => ()
                                    }
                                )?

                                match $target.data {
                                    $(
                                        $a => {
                                            push!(Value, $ret.into_value(&instr.pos));
                                        }
                                    )*
                                    #[allow(unreachable_patterns)] // not every operator matches all types
                                    _ => return Err(err)
                                }
                            }
                        )*
                        _ => panic!("no implementation found for {repr}")
                    };
                }
            }

            #[allow(unused_parens)] // I HATE RUST ANALYZER
            match instr.data {
                Instruction::LoadConst(id) => {
                    push!(Value, self.constants[id].clone().into_value(&instr.pos));
                }

                Instruction::LoadVar(id) => {
                    match self.vars[id] {
                        Some(v) => push!(Id, v),
                        None => panic!("hopefully this shouldn't happen"),
                    };
                }

                Instruction::StoreVar(id) => {
                    self.vars[id] = Some(pop!(Id));
                }

                Instruction::StoreIndex => {
                    let value_id = pop!(Id);

                    let index = pop!(Value);
                    let mut i = self.to_int(&index.data).ok_or_else(|| {
                        Error::err("Type error")
                            .label(instr.pos.clone(), "Expected an integer for list index")
                            .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", self.repr_string(&index.data)))
                    })?;

                    let target_id = pop!(Id);
                    let mut target = self.memory[target_id].clone();

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
                    
                    match &mut target.data {
                        ValueType::List(list) => {
                            let index = index!(list.len());
                            list[index as usize] = value_id;
                        }

                        ValueType::String(s) => {
                            let index = index!(s.len());
                            s.replace_range(
                                s
                                    .char_indices()
                                    .nth(index as usize)
                                    .map(|(pos, ch)| pos..pos + ch.len_utf8())
                                    .unwrap(),
                                &self.to_string(value!(value_id)),
                            );
                        }

                        _ => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected a list to assign to")
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a list", self.repr_string(&target.data)))
                        )
                    };

                    self.memory[target_id] = target;
                }
                
                Instruction::BuildList(len) => {
                    let mut list = Vec::new();
                    for _ in 0..len {
                        list.push(pop!(Id));
                    }
                    list.reverse();
                    push!(List, list);
                }
            
                Instruction::BeforeOp(op_id) => {
                    let op = operators::id_sym(op_id);

                    let target_id = pop!(Id);
                    let mut target = self.memory[target_id].clone();

                    unary_ops! {
                        target, op, Before

                        "!" {
                            a   =>  Boolean(!self.to_bool(&a));
                        }

                        "^" {
                            a   =>  List({
                                        let i = self.to_int(&a).ok_or_else(|| {
                                            Error::err("Type error")
                                                .label(instr.pos.clone(), format!("Expected an integer for {op}x"))
                                                .label(target.pos.clone(), format!("Cannot convert #{}# to an integer", self.repr_string(&a)))
                                        })? as i64;
                                        if i < 0 { i..0 } else { 0..i }.map(|i| push!(Number, i as f64)).collect()
                                    });
                        }

                        "?\\" {

                        }

                        "-" {
                            a   =>  Number(-self.to_num(&a).ok_or_else(|| {
                                        Error::err("Type error")
                                            .label(instr.pos.clone(), format!("Expected a number for {op}x"))
                                            .label(target.pos.clone(), format!("Cannot convert #{}# to a number", self.repr_string(&a)))
                                    })?);
                        }

                        "." {
                            
                        }

                        "`" {

                        }

                        "``" {

                        }

                        "*" {

                        }

                        "@" {
                            % convert (Boolean(_) | Null()) => String(self.repr_string(value!(target_id)));

                            String(a)   => String(a.reverse());
                            List(mut a) => List({ a.reverse(); a });
                            Number(a)   => Number(a.abs().to_string().reverse().parse::<f64>().unwrap().copysign(a));
                        }

                        "^^" {

                        }

                        "#" {
                            
                        }

                        "'" {

                        }
                    }
                }

                Instruction::AfterOp(op_id) => {
                    let op = operators::id_sym(op_id);

                    let target_id = pop!(Id);
                    let mut target = self.memory[target_id].clone();

                    unary_ops! {
                        target, op, After
                        
                        "^^" {

                        }

                        "##" {

                        }

                        "#\\" {

                        }

                        "#$" {

                        }

                        "_" {
                            % convert (Boolean(_) | Null()) => String(self.to_string(value!(target_id)));

                            String(a)   =>  Number(a.len() as f64);
                            List(a)     =>  Number(a.len() as f64);
                            Number(a)   =>  Number(a.to_string().len() as f64);
                        }

                        "``" {

                        }

                        "$$" {

                        }

                        "$" {
                            % convert a @ (Boolean(_) | Null()) => Number(self.to_int(a).unwrap());

                            a @ Number(_)   =>  a;
                            String(a)       =>  Number(a.parse().map_err(|_| Error::err("Value error")
                                                    .label(instr.pos.clone(), "Found conversion to number")
                                                    .label(target.pos, format!("Cannot convert #\"{a}\"# to a number"))
                                                )?);
                        }

                        "'" {
                            
                        }

                        "`" {
                            a => String(self.to_string(&a));
                        }
                    }
                }

                Instruction::BinaryOp(op_id) => {
                    let op = operators::id_sym(op_id);

                    let mut b_id = pop!(Id);
                    let mut rhs = self.memory[b_id].clone();
                    let rhs_type = rhs.type_name();

                    let mut a_id = pop!(Id);
                    let mut lhs = self.memory[a_id].clone();
                    let lhs_type = lhs.type_name();

                    macro_rules! bin_ops {
                        (
                            $(
                                $op:literal {
                                    $(
                                        % convert
                                        // can use the x_id vars to get the unconverted version
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
                            use ValueType::*;
                            
                            let err = Error::err("Type error")
                                .label(lhs.pos.clone(), format!("This has type #{lhs_type}#"))
                                .label(rhs.pos.clone(), format!("This has type #{rhs_type}#"))
                                .label(
                                    instr.pos.clone(),
                                    format!("Binary #{op}# is not implemented for types #{lhs_type}# and #{rhs_type}#")
                                );

                            match op {
                                $(
                                    $op => {
                                        $(
                                            match &lhs.data {
                                                $( $from => lhs = $to.into_value(&lhs.pos), )+
                                                _ => ()
                                            }
                                            match &rhs.data {
                                                $( $from => rhs = $to.into_value(&rhs.pos), )+
                                                _ => ()
                                            }
                                        )?

                                        match (lhs.data, rhs.data) {
                                            $(
                                                $(
                                                    ($a1, $b1) => {
                                                        push!(Value, $ret1.into_value(&instr.pos));
                                                    }
                                                )+
                                            )?
                                            $(
                                                $(
                                                    ($a2, $b2) => {
                                                        push!(Value, $ret2.into_value(&instr.pos));
                                                    }
                                                    
                                                    #[allow(unused_assignments)] // they can be used in the op implementations
                                                    ($b2, $a2) => {
                                                        (a_id, b_id) = (b_id, a_id);
                                                        push!(Value, $ret2.into_value(&instr.pos));
                                                    }
                                                )+
                                            )?
                                            #[allow(unreachable_patterns)] // not every operator matches all types
                                            _ => return Err(err)
                                        }
                                    }
                                )*
                                _ => panic!("no implementation found for a {op} b")
                            };
                        }
                    }

                    macro_rules! set_helper {
                        ( $a:ident, $b:ident ) => { $a.iter().chain($b.iter()).unique().copied() }
                    }
                    
                    bin_ops! {
                        "||" {
                            % one way
                            a, b => Boolean(self.to_bool(&a) || self.to_bool(&b));
                        }

                        "&&" {
                            % one way
                            a, b => Boolean(self.to_bool(&a) && self.to_bool(&b));
                        }

                        "|" {
                            % one way
                            a, b => if self.to_bool(&a) { a } else { b };
                        }
                        
                        "&" {
                            % one way
                            a, b => if self.to_bool(&a) { b } else { a };
                        }

                        // compare operators (would probably be a lot of copy paste so maybe theres a better solution like with PartialOrd or something)

                        "-?" {
                            // something with 1 and -1 lol
                        }

                        ".." {

                        }
                        
                        // might need a Range value type

                        "#" {

                        }

                        "?\\" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            // % one way
                            // Number(a),  Number(b)   =>  Number( random number from a to b ); // what about floats
                        }

                        "%" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a % b);
                        }

                        "+" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a + b);
                            List(a),    List(b)     =>  List(a.into_iter().chain(b.into_iter()).collect());
                            String(a),  String(b)   =>  String(a + &b);
                            String(a),  List(b)     =>  String(a + &self.join_list(&b, ""));

                            String(a),  _           =>  String(a + &self.to_string(value!(b_id)));
                            _,          String(b)   =>  String(self.to_string(value!(a_id)) + &b);

                            % both ways
                            List(a),    _           =>  List(a.into_iter().chain([b_id]).collect());
                        }

                        "-" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            List(mut a), List(b)    =>  List({ for b_id in b { a.remove_element(&b_id); }; a });
                            List(mut a), _          =>  List({                 a.remove_element(&b_id);    a });

                            String(a),  String(b)   =>  String({
                                                            if a.len() == 1 || b.len() == 1 {
                                                                (a.chars().next().unwrap()..b.chars().next().unwrap()).collect()
                                                            } else {
                                                                return Err(
                                                                    Error::err("Value error")
                                                                        .label(instr.pos.clone(), format!(
                                                                            "Expected strings of {} for character range",
                                                                            "length 1".fg(Green),
                                                                        ))
                                                                        .label(lhs.pos, format!(
                                                                            "This string has length #{a}#",
                                                                            // a.len().fg(if a.len() == 1 { Green } else { Red }),
                                                                        ))
                                                                        .label(rhs.pos, format!(
                                                                            "This string has length #{b}#",
                                                                            // b.len().fg(if b.len() == 1 { Green } else { Red }),
                                                                        ))
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
                                                                            instr.pos.clone(),
                                                                            format!("Expected #number >= 0# for right side of {lhs_type} {op} {rhs_type}"),
                                                                        )
                                                                        .label(rhs.pos, format!("{b} is not >= 0"))
                                                                )
                                                            }
                                                        });

                            Number(a),  Number(b)   =>  Number(a - b);
                        }

                        "*" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a * b);
                            List(a),    List(b)     =>  List(set_helper!(a, b).collect());

                            % both ways
                            List(a),    Number(b)   =>  List({
                                                            let mut res = Vec::new();
                                                            for _ in 0..b.abs() as usize {
                                                                res.extend(a.iter().map(|&id| clone!(id)));
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
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a / b);
                            List(a),    List(b)     =>  List(set_helper!(a, b).filter(|id| a.contains(id) && b.contains(id)).collect());
                        }

                        "//" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number((a / b).trunc());
                        }

                        "+-" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  List(vec![push!(Number, a + b), push!(Number, a - b)]);
                        }

                        "/%" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  List(vec![push!(Number, a / b), push!(Number, a % b)]);
                        }

                        "^" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a.powf(b));
                            List(a),    List(b)     =>  List(set_helper!(a, b).filter(|id| a.contains(id) != b.contains(id)).collect());
                            
                            List(a),    String(b)   =>  String(a.into_iter().map(|id| self.to_string(value!(id))).join(&b));
                            String(a),  String(b)   =>  String(a.chars().map(|c| c.to_string()).intersperse(b).collect());
                        }

                        ".*" {

                        }

                        "^*" {
                            
                        }

                        "@" {

                        }
                    }
                }

                Instruction::BinaryIndex => {
                    let index = pop!(Value);
                    let mut i = self.to_int(&index.data).ok_or_else(|| {
                        Error::err("Type error")
                            .label(instr.pos.clone(), "Expected an integer for list index")
                            .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", self.repr_string(&index.data)))
                    })?;

                    let target = pop!(Value);

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

                    match &target.data {
                        ValueType::List(list) => {
                            push!(Id, list[index!(list.len()) as usize]);
                        }

                        ValueType::String(s) => {
                            push!(String, s.chars().nth(index!(s.len()) as usize).unwrap().to_string());
                        }

                        ValueType::Number(num) => {
                            push!(Number, index!(*num));
                        }

                        _ => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected a list to index")
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a list", &self.to_string(&target.data)))
                        )
                    };
                }
            
                Instruction::Print(ref mode) => {
                    use PrintMode::*;

                    let data = value!(StackTop);
                    match data {
                        ValueType::List(list) => {
                            match mode {
                                Default | NoNewline       => print!("{}", self.join_list(list, "")),
                                Spaces  | NoNewlineSpaces => print!("{}", self.join_list(list, " ")),
                            }
                        }
                        _ => {
                            let value = self.to_string(data);
                            match mode {
                                Default | NoNewline       => print!("{}", value),
                                Spaces  | NoNewlineSpaces => print!("{}", value.chars().intersperse(' ').collect::<String>()),
                            }
                        }
                    }

                    if matches!(mode, Default | Spaces) {
                        println!();
                    }
                }

                Instruction::Input => {
                    let mut input = String::new();
                    io::stdout().flush().unwrap();
                    io::stdin().read_line(&mut input).unwrap_or_else(|err| {
                        errors::simple(err.to_string());
                    });

                    let id = self.memory.push(
                        ValueType::String(input.trim_end().into()).into_value(&instr.pos)
                    );
                    push!(Id, id);
                }

                Instruction::Jump(pos) => {
                    instr_pos = pos - 1; // will be incremented next iteration
                }

                Instruction::PopJumpIfFalse(pos) => {
                    let data = &pop!(Value).data;
                    if !self.to_bool(data) {
                        instr_pos = pos - 1;
                    }
                }
            
                Instruction::PopOne => {
                    pop!(Id);
                }

                Instruction::DupOne => {
                    self.stack.push(*self.stack.last().unwrap())
                }

                Instruction::DupTwo => {
                    self.stack.extend_from_within(self.stack.len() - 2..);
                }

                Instruction::RotFour => {
                    let last_three = self.stack.len() - 4;
                    self.stack[last_three..].rotate_right(1);
                }

                Instruction::Return => {
                    returned = Some(pop!(Id));
                }

                Instruction::Break => {
                    todo!("break")
                }

                Instruction::Continue => {
                    todo!("continue")
                }
            
                Instruction::Exit => {
                    process::exit(0);
                }
            }

            if returned.is_some() { break }
            instr_pos += 1;
        }

        Ok(returned)
    }
}
