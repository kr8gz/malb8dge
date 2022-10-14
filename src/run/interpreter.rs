#![allow(unstable_name_collisions)]

use std::{io::{self, Write}, process};

use ariadne::{Fmt, Color::*};

use itertools::Itertools;

use crate::{compile::{compiler::Compiler, instructions::*}, util::{*, errors::{self, Error}, Pos, operators}, parse::ast::*};

use super::types::*;

pub struct Interpreter {
    constants: Vec<Value>,
    functions: Vec<Function>,
    memory: Stack,

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
        }.run_func(0)
    }

    fn to_int(&self, value: &ValueType) -> Option<f64> {
        use ValueType::*;
        match value {
            List(list) => Some(list.len() as f64),
            String(s) => s.parse().ok().map(|n: f64| n.floor()),
            Number(num) => Some(num.floor()),
            Boolean(true) => Some(1.0),
            Boolean(false) | Null() => Some(0.0),
            _ => None
        }
    }

    fn to_string(&self, id: usize) -> String {
        use ValueType::*;
        match &self.memory[id].data {
            Function(_) => "<function>".into(),
            List(list) => {
                format!("[{}]", list.iter().map(|&v| self.repr_string(v)).collect::<Vec<_>>().join(", "))
            },
            Boolean(b) => b.to_string(),
            String(s) => s.clone(),
            Number(num) => num.to_string(),
            Null() => "".into(),
        }
    }

    fn repr_string(&self, id: usize) -> String {
        use ValueType::*;
        match &self.memory[id].data {
            String(s) => format!("\"{s}\""),
            Null() => "null".into(),
            _ => self.to_string(id),
        }
    }

    fn join_list(&self, list: &[usize], sep: &str) -> String {
        list.iter()
            .map(|&v| self.to_string(v))
            .join(sep)
    }

    fn run_func(&mut self, func: usize) -> Result<()> {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            macro_rules! push {
                ( Value, $value:expr ) => {
                    {
                        let value = $value; // most confusing error ever lmao
                        let id = self.memory.push(value);
                        self.stack.push(id);
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

            match instr.data {
                Instruction::LoadConst(id) => {
                    push!(Value, self.constants[id].clone());
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
                    let mut i = match self.to_int(&index.data) {
                        Some(i) => i,
                        None => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected an integer as list index")
                                .label(index.pos.clone(), "Cannot convert this to an integer")
                        )
                    };

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
                                            .label(target.pos.clone(), format!("Length of this is {len}"))
                                            .label(index.pos.clone(), format!("Index is {i}"))
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
                                &self.to_string(value_id),
                            );
                        }

                        _ => return Err(
                            Error::err("Type error")
                            .label(instr.pos.clone(), "Expected a list to assign to")
                            .label(target.pos.clone(), "Cannot convert this to a list")
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
            
                Instruction::UnaryOp(op_id) => {
                    todo!("run unary op {op_id}")
                }

                Instruction::BinaryOp(op_id) => {
                    let mut b_id = pop!(Id);
                    let mut rhs = self.memory[b_id].clone();
                    let rhs_type = rhs.type_name();

                    let mut a_id = pop!(Id);
                    let mut lhs = self.memory[a_id].clone();
                    let lhs_type = lhs.type_name();

                    let op = operators::id_sym(op_id);

                    macro_rules! bin_ops {
                        (
                            $(
                                $op:literal {
                                    $(
                                        % convert
                                        // can use the x_id vars to get the unconverted version
                                        $( $pre:pat => $expr:expr; )+
                                    )?
                                    $(
                                        % one way
                                        $( $a1:pat, $b1:pat => $ret1:ident($($expr1:expr)?); )+
                                    )?
                                    $(
                                        % both ways
                                        $( $a2:pat, $b2:pat => $ret2:ident($($expr2:expr)?); )+
                                    )?
                                }
                            )*
                        ) => {
                            use ValueType::*;
                            
                            let err = Error::err("Type error")
                            .label(lhs.pos.clone(), format!("This has type '{lhs_type}'"))
                            .label(rhs.pos.clone(), format!("This has type '{rhs_type}'"))
                            .label(
                                instr.pos.clone(),
                                format!("Binary '{op}' is not implemented for types '{lhs_type}' and '{rhs_type}'")
                            );

                            match op {
                                $(
                                    $op => {
                                        $(
                                            match &lhs.data {
                                                $( $pre => lhs = $expr.into_value(&lhs.pos), )+
                                                _ => ()
                                            }
                                            match &rhs.data {
                                                $( $pre => rhs = $expr.into_value(&rhs.pos), )+
                                                _ => ()
                                            }
                                        )?

                                        match ( lhs.data, rhs.data ) {
                                            $(
                                                $(
                                                    ( $a1, $b1 ) => {
                                                        push!($ret1 $(, $expr1)?);
                                                    }
                                                )+
                                            )?
                                            $(
                                                $(
                                                    ( $a2, $b2 ) => {
                                                        push!($ret2 $(, $expr2)?);
                                                    }
                                                    
                                                    #[allow(unused_assignments)] // they can be used in the op implementations
                                                    ( $b2, $a2 ) => {
                                                        (a_id, b_id) = (b_id, a_id);
                                                        push!($ret2 $(, $expr2)?);
                                                    }
                                                )+
                                            )?
                                            _ => return Err(err)
                                        }
                                    }
                                )*
                                _ => todo!("run binary op {op_id}")
                            };
                        }
                    }

                    macro_rules! set_helper {
                        ( $a:ident, $b:ident ) => { $a.iter().chain($b.iter()).unique().copied() }
                    }
                    
                    // basically writing std lmao
                    bin_ops! {
                        "||" {

                        }

                        "&&" {

                        }

                        "|" {

                        }
                        
                        "&" {

                        }

                        // compare operators (would probably be a lot of copy paste so maybe theres a better solution like with PartialOrd or something)

                        "-?" {

                        }

                        ".." {

                        }
                        
                        // might need a Range value type

                        "#" {

                        }

                        "?\\" {

                        }

                        "%" {
                            
                        }

                        "+" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            Number(a),  Number(b)   =>  Number(a + b);
                            List(a),    List(b)     =>  List(a.into_iter().chain(b.into_iter()).collect());
                            String(a),  String(b)   =>  String(a + &b);
                            String(a),  List(b)     =>  String(a + &self.join_list(&b, ""));

                            % both ways
                            List(a),    _           =>  List(a.into_iter().chain([b_id]).collect());
                            String(a),  _           =>  String(a + &self.to_string(b_id));
                        }

                        "-" {
                            % convert
                            x @ (Boolean(_) | Null()) => Number(self.to_int(x).unwrap());

                            % one way
                            List(mut a), List(b)    =>  List({
                                                            for b_id in b {
                                                                if let Some(pos) = a.iter().position(|id| *id == b_id) {
                                                                    a.remove(pos);
                                                                }
                                                            }
                                                            a
                                                        });

                            List(mut a), _          =>  List({
                                                            if let Some(pos) = a.iter().position(|id| *id == b_id) {
                                                                a.remove(pos);
                                                            }
                                                            a
                                                        });

                            String(a),  String(b)   =>  String({
                                                            if a.len() != 1 || b.len() != 1 {
                                                                return Err(
                                                                    Error::err("Value error")
                                                                        .label(instr.pos.clone(), "Expected strings of length 1 for character range")
                                                                        .label(lhs.pos, format!(
                                                                            "This string has length {}",
                                                                            a.len().fg(if a.len() == 1 { Green } else { Red }),
                                                                        ))
                                                                        .label(rhs.pos, format!(
                                                                            "This string has length {}",
                                                                            b.len().fg(if b.len() == 1 { Green } else { Red }),
                                                                        ))
                                                                )
                                                            }
                                                            (a.chars().next().unwrap()..b.chars().next().unwrap()).collect()
                                                        });

                            String(a),  Number(b)   =>  String(a.chars().dropping_back({
                                                            if b >= 0.0 {
                                                                b as usize
                                                            } else {
                                                                return Err(
                                                                    Error::err("Value error")
                                                                        .label(
                                                                            instr.pos.clone(),
                                                                            format!(
                                                                                "Expected {} for right side of {lhs_type} {op} {rhs_type}",
                                                                                "number >= 0".fg(Green),
                                                                            ),
                                                                        )
                                                                        .label(rhs.pos, format!("{} is not {}", b.fg(Red), ">= 0".fg(Green)))
                                                                )
                                                            }
                                                        }).collect());

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
                                                                res.extend(a.iter().map(|&id| {
                                                                    if let ValueType::List(list) = &self.memory[id].data {
                                                                        push!(List, list.clone()) // 3 * [3 * [x]]
                                                                    } else {
                                                                        id
                                                                    }
                                                                }));
                                                            }
                                                            if b < 0.0 { res.reverse(); }
                                                            res
                                                        });
                            
                            String(mut a), Number(b) => String({
                                                            a = a.repeat(b.abs() as usize);
                                                            if b < 0.0 { a = a.chars().rev().collect(); }
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
                    let target = pop!(Value);

                    match self.to_int(&index.data) {
                        Some(mut i) => {
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
                                                    .label(target.pos.clone(), format!("Length of this is {len}"))
                                                    .label(index.pos.clone(), format!("Index is {i}"))
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
                                        .label(target.pos.clone(), "Cannot convert this to a list")
                                )
                            };
                            
                        },
                        None => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected an integer as list index")
                                .label(index.pos.clone(), "Cannot convert this to an integer")
                        )
                    }
                }
            
                Instruction::Print(ref mode) => {
                    use PrintMode::*;

                    let id = *self.stack.last().unwrap();
                    match &self.memory[id].data {
                        ValueType::List(list) => {
                            match mode {
                                Default | NoNewline       => print!("{}", self.join_list(list, "")),
                                Spaces  | NoNewlineSpaces => print!("{}", self.join_list(list, " ")),
                            }
                        }
                        _ => {
                            let value = self.to_string(id);
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
            
                Instruction::PopOne => {
                    self.stack.pop();
                }

                Instruction::DupOne => {
                    self.stack.push(*self.stack.last().unwrap())
                }

                Instruction::DupTwo => {
                    self.stack.extend_from_within(self.stack.len() - 2..);
                }

                Instruction::RotThree => {
                    let last_three = self.stack.len() - 3;
                    self.stack[last_three..].rotate_right(1);
                }

                Instruction::RotFour => {
                    let last_three = self.stack.len() - 4;
                    self.stack[last_three..].rotate_right(1);
                }
            
                Instruction::Exit => {
                    process::exit(0);
                }
            }

            instr_pos += 1;
        }

        Ok(())
    }
}
