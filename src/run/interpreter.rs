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

    fn run_func(&mut self, func: usize) {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            macro_rules! value {
                ( $type:ident, $data:expr ) => {
                    ValueType::$type($data).into_value(&instr.pos)
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
                    self.vars[id] = Some(self.stack.pop().unwrap())
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
                    use ValueType::*;

                    let rhs = pop_stored!();
                    let lhs = pop_stored!();

                    macro_rules! operators {
                        (
                            $(
                                ( $op_a:expr $(, $op_b:expr)? )

                                $op:literal {
                                    $(
                                        $n_a:ident: $t_a:ident $(, $n_b:ident: $t_b:ident)? -> $t_ret:ident = $expr:expr
                                    )+
                                }
                            )*
                        ) => {
                            let op = operators::id_sym(op_id);
                            match op {
                                $(
                                    $op => match ( &$op_a.data $(, &$op_b.data )? ) {
                                        $(
                                            ( $t_a($n_a) $(, $t_b($n_b))? ) => {
                                                push_stored!(value!($t_ret, $expr));
                                            }
                                        )+
                                        _ => {
                                            // TODO unary/binary errors
                                            self.error("Type error")
                                                .label($op_a.pos.clone(), format!("This has type '{}'", $op_a.type_name()))
                                             $( .label($op_b.pos.clone(), format!("This has type '{}'", $op_b.type_name())) )?
                                                .label(
                                                    instr.pos.clone(),
                                                    format!(
                                                        "Binary '{op}' is not implemented for types '{}' and '{}'",
                                                        $op_a.type_name(), $( $op_b.type_name(), )?
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
                    
                    operators! {
                        (lhs, rhs)

                        "+" {
                            a: Integer, b: Integer  -> Integer  = a + b
                            a: Integer, b: Float    -> Float    = *a as f64 + b
                            a: Float,   b: Float    -> Float    = a + b
                            a: Float,   b: Integer  -> Float    = a + *b as f64
                            // todo
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
                            let list = list.iter().map(|&v| self.memory[v].to_string(&self.memory)).collect::<Vec<_>>();
                            match mode {
                                Default | NoNewline       => print!("{}", list.join("")),
                                Spaces  | NoNewlineSpaces => print!("{}", list.join(" ")),
                            }
                        }
                        _ => {
                            let value = value.to_string(&self.memory);
                            match mode {
                                Default | NoNewline       => print!("{}", value),
                                #[allow(unstable_name_collisions)]
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
