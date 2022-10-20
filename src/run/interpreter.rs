use std::{io::{self, Write}, process};

use itertools::Itertools;

use crate::{
    compile::{compiler::Compiler, instructions::*},
    util::{*, errors::Error, Pos, operators::{self, *}},
    parse::ast::*
};

use super::types::*;

#[derive(Debug)]
pub struct Interpreter {
    constants: Vec<ValueType>,
    functions: Vec<Function>,
    memory: Stack<Value>,

    vars: Vec<Option<usize>>,
    stack: Vec<usize>,
    call_stack: Vec<Pos>,
}

impl Interpreter {
    pub fn new(compiler: Compiler) -> Self {
        Self {
            constants: compiler.constants.0,
            functions: compiler.functions,
            memory: Stack::new(),

            vars: vec![None; compiler.var_count],
            stack: Vec::new(),
            call_stack: Vec::new(),
        }
    }

    pub fn run(&mut self) -> Result<()> {
        self.run_func(0)
    }

    fn run_func(&mut self, func: usize) -> Result<()> {
        let instructions = &self.functions[func].instructions;
        let mut instr_pos = 0;

        while instr_pos < instructions.len() {
            let instr = &instructions[instr_pos];

            #[macro_export]
            macro_rules! memory_helper_macros {
                ( $memory:expr, $pos:expr ) => {
                    macro_rules! value {
                        ( $id:expr ) => {
                            &$memory[$id].data
                        };
                    }
            
                    macro_rules! store {
                        ( $data:expr ) => {
                            $memory.push($data.into_value($pos))
                        };
                    }
            
                    #[allow(unused_macros)] // clone! is used at least once (running operators)
                    macro_rules! clone {
                        ( $id:expr ) => {
                            if let ValueType::List(list) = value!($id) {
                                store!(ValueType::List(list.clone()))
                            } else {
                                $id
                            }
                        };
                    }
                }
            }
            
            memory_helper_macros!(self.memory, &instr.pos);

            macro_rules! push {
                ( Id, $id:expr ) => {
                    self.stack.push($id)
                };
    
                ( $data:expr ) => {
                    {
                        let id = store!($data);
                        push!(Id, id);
                        id
                    }
                };
            }
        
            macro_rules! pop {
                ( Id ) => {
                    self.stack.pop().unwrap()
                };
    
                ( Value ) => {
                    &self.memory[pop!(Id)]
                };
                
                ( Operand ) => {
                    {
                        let id = pop!(Id);
                        let cloned = self.memory[id].clone();
                        Operand {
                            id,
                            type_name: cloned.type_name(),
                            data: cloned.data,
                            pos: cloned.pos,
                        }
                    }
                };
            }

            match instr.data {
                Instruction::LoadConst(id) => {
                    push!(self.constants[id].clone());
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
                    let mut i = index.data.as_int().ok_or_else(|| {
                        Error::err("Type error")
                            .label(instr.pos.clone(), "Expected an integer for list index")
                            .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", index.data.as_repr_string(&self.memory)))
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
                                &value!(value_id).as_string(&self.memory),
                            );
                        }

                        _ => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected a list to assign to")
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a list", target.data.as_repr_string(&self.memory)))
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
                    push!(ValueType::List(list));
                }
            
                Instruction::BeforeOp(op_id) | Instruction::AfterOp(op_id) => {
                    let target = pop!(Operand);
                    let op = operators::id_sym(op_id);
                    let op_type = match &instr.data {
                        Instruction::BeforeOp(_) => OpType::Before,
                        Instruction::AfterOp(_) => OpType::After,
                        _ => unreachable!()
                    };
                    
                    let result = operators::run_unary_op(&mut self.memory, &instr.pos, target, op_type, op)?;
                    push!(result);
                }

                Instruction::BinaryOp(op_id) => {
                    let rhs = pop!(Operand);
                    let lhs = pop!(Operand);
                    let op = operators::id_sym(op_id);

                    let result = operators::run_bin_op(&mut self.memory, &instr.pos, lhs, rhs, op)?;
                    push!(result);
                }

                Instruction::BinaryIndex => {
                    let index = pop!(Value);
                    let mut i = index.data.as_int().ok_or_else(|| {
                        Error::err("Type error")
                            .label(instr.pos.clone(), "Expected an integer for list index")
                            .label(index.pos.clone(), format!("Cannot convert #{}# to an integer", index.data.as_repr_string(&self.memory)))
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
                            push!(ValueType::String(s.chars().nth(index!(s.len()) as usize).unwrap().to_string()));
                        }

                        ValueType::Number(num) => {
                            push!(ValueType::Number(index!(*num)));
                        }

                        _ => return Err(
                            Error::err("Type error")
                                .label(instr.pos.clone(), "Expected a list to index")
                                .label(target.pos.clone(), format!("Cannot convert #{}# to a list", target.data.as_repr_string(&self.memory)))
                        )
                    };
                }
            
                Instruction::Print(ref mode) => {
                    use PrintMode::*;

                    let data = value!(*self.stack.last().unwrap());
                    match data {
                        ValueType::List(list) => {
                            match mode {
                                Default | NoNewline       => print!("{}", join_list(list, "", &self.memory)),
                                Spaces  | NoNewlineSpaces => print!("{}", join_list(list, " ", &self.memory)),
                            }
                        }
                        _ => {
                            let value = data.as_string(&self.memory);
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
                        Error::simple(err.to_string());
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
                    if !data.as_bool() {
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
                    todo!("return")
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

            instr_pos += 1;
        }
        Ok(())
    }
}
