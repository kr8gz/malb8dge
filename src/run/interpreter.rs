#![allow(unstable_name_collisions)]

use std::{io::{self, Write}, process};

use itertools::Itertools;

use crate::{compile::{compiler::Compiler, instructions::*}, util::{errors::{self, Error}, Pos, operators}, parse::ast::*};

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
    pub fn run(compiler: Compiler) -> Result<(), Error> {
        Self {
            constants: compiler.constants.0,
            functions: compiler.functions,
            memory: Stack::new(),

            vars: vec![None; compiler.var_count],
            stack: Vec::new(),
            call_stack: Vec::new(),
        }.run_func(0)
    }

    fn to_int(&self, value: &ValueType) -> Option<i64> {
        use ValueType::*;
        match value {
            List(list) => Some(list.len() as i64),
            String(s)      => s.parse().ok(),
            Integer(int)      => Some(*int),
            Float(float)      => Some(*float as i64),
            Boolean(true)           => Some(1),
            Boolean(false) | Null() => Some(0),
            _ => None
        }
    }

    fn join_list(&self, list: &[usize], sep: &str) -> String {
        list.iter()
            .map(|&v| self.memory[v].to_string(&self.memory))
            .join(sep)
    }

    fn run_func(&mut self, func: usize) -> Result<(), Error> {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            macro_rules! push {
                ( Value, $value:expr ) => {
                    {
                        let id = self.memory.push($value);
                        self.stack.push(id)
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
                    let index = pop!(Value);
                    let index_pos = index.pos.clone();
                    let mut i = match self.to_int(&index.data) {
                        Some(i) => i,
                        None => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected an integer as list index")
                                .label(index_pos, "Cannot convert this to an integer")
                        )
                    };

                    let target = &mut self.memory[pop!(Id)];
                    let target_pos = target.pos.clone(); // TODO needed??

                    macro_rules! index {
                        ( $len:expr ) => {
                            {
                                let len = $len as i64;
                                if i < 0 {
                                    i += len;
                                }
                                if i < 0 || i >= len {
                                    return Err(
                                        Error::err("Index out of bounds")
                                            .label(target_pos, format!("Length of this is {len}"))
                                            .label(index_pos, format!("Index is {i}"))
                                    )
                                }
                                i
                            }
                        }
                    }
                    
                    match &mut target.data {
                        ValueType::List(list) => {
                            let index = index!(list.len());
                            list[index as usize] = pop!(Id);
                        }

                        ValueType::String(s) => {
                            let index = index!(s.len());
                            s.replace_range(
                                s
                                    .char_indices()
                                    .nth(index as usize)
                                    .map(|(pos, ch)| (pos..pos + ch.len_utf8()))
                                    .unwrap(),
                                "x",
                            );
                        }

                        _ => return Err(
                            Error::err("Type error")
                            .label(instr.pos.clone(), "Expected a list to index")
                            .label(target.pos.clone(), "Cannot convert this to a list")
                        )
                    };
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

                    let mut a_id = pop!(Id);
                    let mut lhs = self.memory[a_id].clone();

                    macro_rules! bin_ops {
                        (
                            $(
                                $op:literal {
                                    $(
                                        % convert {
                                            $( $pre:pat => $expr:expr; )+
                                        }
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

                            let op = operators::id_sym(op_id);
                            
                            let err = Error::err("Type error")
                            .label(lhs.pos.clone(), format!("This has type '{}'", lhs.type_name()))
                            .label(rhs.pos.clone(), format!("This has type '{}'", rhs.type_name()))
                            .label(
                                instr.pos.clone(),
                                format!(
                                    "Binary '{op}' is not implemented for types '{}' and '{}'",
                                    lhs.type_name(), rhs.type_name(),
                                )
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
                    
                    // basically writing std lmao
                    bin_ops! {
                        "+" {
                            % convert {
                                x @ Boolean(_) | x @ Null() => Integer(self.to_int(x).unwrap());
                            }

                            % one way
                            List(a),    List(b)     =>  List(a.into_iter().chain(b.into_iter()).collect());
                            String(a),  String(b)   =>  String(a + &b);
                            String(a),  List(b)     =>  String(a + &self.join_list(&b, ""));

                            Integer(a), Integer(b)  =>  Integer(a + b);
                            Float(a),   Float(b)    =>  Float(a + b);

                            % both ways
                            List(a),    _           =>  List(a.into_iter().chain([b_id]).collect());
                            String(a),  b           =>  String(a + &b.to_string(&self.memory)); // TODO check if bool and null convert to int here becaseUSE THE SOHULDNT

                            Integer(a), Float(b)    =>  Float(a as f64 + b);
                        }

                        "-" {
                            % convert {
                                x @ Boolean(_) | x @ Null() => Integer(self.to_int(x).unwrap());
                            }

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
                                                                        .label(lhs.pos, format!("This string has length {}", a.len()))
                                                                        .label(rhs.pos, format!("This string has length {}", b.len()))
                                                                )
                                                            }
                                                            (a.chars().next().unwrap()..b.chars().next().unwrap()).collect()
                                                        });

                            String(a),  Integer(b)  =>  String(a.chars().dropping_back(b as usize).collect());
                            String(a),  Float(b)    =>  String(a.chars().dropping_back(b as usize).collect());

                            Integer(a), Integer(b)  =>  Integer(a - b);
                            Float(a),   Float(b)    =>  Float(a - b);
                            Integer(a), Float(b)    =>  Float(a as f64 - b);
                            Float(a),   Integer(b)  =>  Float(a - b as f64);
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
                                        let len = $len as i64;
                                        if i < 0 {
                                            i += len;
                                        }
                                        if i < 0 || i >= len {
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

                                ValueType::Integer(int) => {
                                    push!(Integer, index!(*int));
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

                    let value = &self.memory[*self.stack.last().unwrap()];
                    match &value.data {
                        ValueType::List(list) => {
                            match mode {
                                Default | NoNewline       => print!("{}", self.join_list(list, "")),
                                Spaces  | NoNewlineSpaces => print!("{}", self.join_list(list, " ")),
                            }
                        }
                        _ => {
                            let value = value.to_string(&self.memory);
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
            
                Instruction::Exit => {
                    process::exit(0);
                }
            }

            instr_pos += 1;
        }

        Ok(())
    }
}
