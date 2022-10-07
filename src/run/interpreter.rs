#![allow(unstable_name_collisions)]

use std::{io::{self, Write}, process};

use ariadne::ReportKind;
use itertools::Itertools;

use crate::{compile::{compiler::Compiler, instructions::*}, util::{errors::{self, Error}, Pos, operators}, parse::ast::*};

use super::types::*;

pub struct Interpreter {
    file: String,

    constants: Vec<Value>,
    functions: Vec<Function>,
    memory: Stack<Value>,

    vars: Vec<Option<usize>>,
    stack: Vec<usize>,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn run(compiler: Compiler) {
        Self {
            file: compiler.file,

            constants: compiler.constants.0,
            functions: compiler.functions,
            memory: Stack::new(),

            vars: vec![None; compiler.var_count],
            stack: Vec::new(),
            call_stack: Vec::new(),
        }.run_func(0);
    }

    fn error(&self, msg: &str) -> Error {
        Error::new(self.file.clone(), msg, ReportKind::Error)
    }

    fn to_int(&self, value: &ValueType) -> Option<i64> {
        use ValueType::*;
        match value {
            List(list) => Some(list.len() as i64),
            String(s)      => s.parse().ok(),
            Integer(int)      => Some(*int),
            Float(float)      => Some(*float as i64),
            Boolean(true)           => Some(1),
            Boolean(false) | Null()   => Some(0),
            _ => None
        }
    }

    fn join_list(&self, list: &[usize], sep: &str) -> String {
        list.iter()
            .map(|&v| self.memory[v].to_string(&self.memory))
            .intersperse(sep.into())
            .collect::<String>()
    }

    fn run_func(&mut self, func: usize) {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            macro_rules! value {
                ( $type:ident $(, $data:expr)? ) => {
                    ValueType::$type( $($data)? ).into_value(&instr.pos)
                }
            }

            macro_rules! push_stored {
                ( $value:expr ) => {
                    let id = self.memory.push($value);
                    self.stack.push(id);
                }
            }

            macro_rules! pop_stored {
                () => {
                    &self.memory[self.stack.pop().unwrap()]
                }
            }

            match instr.data {
                Instruction::LoadConst(id) => {
                    push_stored!(self.constants[id].clone());
                }

                Instruction::LoadVar(id) => {
                    match self.vars[id] {
                        Some(v) => self.stack.push(v),
                        None => panic!("hopefully this shouldn't happen"),
                    };
                }

                Instruction::StoreVar(id) => {
                    self.vars[id] = Some(self.stack.pop().unwrap());
                }

                Instruction::StoreIndex => {
                    todo!("run // s1.s0 = s2 // pop all 3")
                }
                
                Instruction::BuildList(len) => {
                    let mut list = Vec::new();
                    for _ in 0..len {
                        list.push(self.stack.pop().unwrap());
                    }
                    list.reverse();

                    push_stored!(value!(List, list));
                }
            
                Instruction::UnaryOp(op_id) => {
                    todo!("run unary op {op_id}")
                }

                Instruction::BinaryOp(op_id) => {
                    let mut b_id = self.stack.pop().unwrap();
                    let rhs = &self.memory[b_id];

                    let mut a_id = self.stack.pop().unwrap();
                    let lhs = &self.memory[a_id];

                    macro_rules! bin_ops {
                        (
                            $(
                                $op:literal {
                                    % one way
                                    $(
                                        $a1:pat, $b1:pat => $ret1:ident($($expr1:expr)?);
                                    )+
                                    % both ways
                                    $(
                                        $a2:pat, $b2:pat => $ret2:ident($($expr2:expr)?);
                                    )+
                                }
                            )*
                        ) => {
                            use ValueType::*;

                            let op = operators::id_sym(op_id);
                            match op {
                                $(
                                    $op => match ( &lhs.data, &rhs.data ) {
                                        $(
                                            ( $a1, $b1 ) => {
                                                push_stored!(value!($ret1 $(, $expr1)?));
                                            }
                                        )+
                                        $(
                                            ( $a2, $b2 ) => {
                                                push_stored!(value!($ret2 $(, $expr2)?));
                                            }
                                            
                                            #[allow(unused_assignments)] // they can be used in the op implementations
                                            ( $b2, $a2 ) => {
                                                (a_id, b_id) = (b_id, a_id);
                                                push_stored!(value!($ret2 $(, $expr2)?));
                                            }
                                        )+
                                        _ => {
                                            self.error("Type error")
                                                .label(lhs.pos.clone(), format!("This has type '{}'", lhs.type_name()))
                                                .label(rhs.pos.clone(), format!("This has type '{}'", rhs.type_name()))
                                                .label(
                                                    instr.pos.clone(),
                                                    format!(
                                                        "Binary '{op}' is not implemented for types '{}' and '{}'",
                                                        lhs.type_name(), rhs.type_name(),
                                                    )
                                                )
                                                .eprint();
                                        }
                                    }
                                )*
                                _ => todo!("run binary op {op_id}")
                            };
                        }
                    }
                    
                    bin_ops! {
                        "+" {
                            % one way
                            List(a),    List(b)     =>  List(a.iter().chain(b.iter()).cloned().collect());
                            String(a),  String(b)   =>  String(a.clone() + b);
                            List(a),    String(_)   =>  List(a.iter().cloned().chain([b_id]).collect());
                            String(a),  List(b)     =>  String(a.clone() + &self.join_list(b, ""));

                            Integer(a), Integer(b)  =>  Integer(a + b);
                            Float(a),   Float(b)    =>  Float(a + b);
                            Boolean(a), Boolean(b)  =>  Integer(*a as i64 + *b as i64);

                            Null(),     Null()      =>  Null();

                            % both ways
                            List(a),    _           =>  List(a.iter().cloned().chain([b_id]).collect());
                            String(a),  b           =>  String(a.clone() + &b.to_string(&self.memory));

                            Integer(a), Float(b)    =>  Float(*a as f64 + b);

                            Boolean(a), Integer(b)  =>  Integer(*a as i64 + b);
                            Boolean(a), Float(b)    =>  Float(if *a { 1.0 } else { 0.0 } + b);

                            Null(),     Integer(b)  =>  Integer(*b);
                            Null(),     Float(b)    =>  Float(*b);
                            Null(),     Boolean(b)  =>  Integer(*b as i64);
                        }
                    }
                }

                Instruction::BinaryIndex => {
                    let index = pop_stored!();
                    let target = pop_stored!();

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
                                            self.error("Index out of bounds")
                                                .label(target.pos.clone(), format!("Length of this is {len}"))
                                                .label(index.pos.clone(), format!("Index is {i}"))
                                                .eprint();
                                        }
                                        i
                                    }
                                }
                            }

                            match &target.data {
                                ValueType::List(list) => {
                                    self.stack.push(list[index!(list.len()) as usize]);
                                }

                                ValueType::String(s) => {
                                    let s = s.chars().nth(index!(s.len()) as usize).unwrap().to_string();
                                    push_stored!(value!(String, s));
                                }

                                ValueType::Integer(int) => {
                                    push_stored!(value!(Integer, index!(*int)));
                                }

                                _ => {
                                    self.error("Type error")
                                        .label(instr.pos.clone(), "Expected a list to index")
                                        .label(target.pos.clone(), "Cannot convert this to a list")
                                        .eprint();
                                }
                            };
                            
                        },
                        None => {
                            self.error("Type error")
                                .label(instr.pos.clone(), "Expected an integer as list index")
                                .label(index.pos.clone(), "Cannot convert this to an integer")
                                .eprint();
                        }
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
                    self.stack.push(id);
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
    }
}
