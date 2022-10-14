use std::collections::HashMap;

use crate::{parse::ast::*, util::{*, errors::Error, operators::{self, OpType}, Pos}, run::types::*};

use super::instructions::*;

#[derive(Debug)]
pub struct Compiler {
    pub constants: Stack,
    pub functions: Vec<Function>,
    pub scopes: Vec<Scope>,
    pub var_count: usize,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            constants: Stack::new(),
            functions: Vec::new(),
            scopes: Vec::new(),
            var_count: 0,
        }
    }

    fn push_instr(&mut self, instr: Instruction, pos: &Pos, func: usize) {
        self.functions[func].instructions.push(InstrData { data: instr, pos: pos.clone() });
    }

    fn push_const(&mut self, constant: ValueType, pos: &Pos, func: usize) {
        let id = self.constants.push(constant.into_value(pos));
        self.push_instr(Instruction::LoadConst(id), pos, func);
    }

    fn push_op(&mut self, op_type: OpType, op: &str, pos: &Pos, func: usize) {
        let id = operators::op_id(op_type, op);
        match op_type {
            OpType::Before | OpType::After => self.push_instr(Instruction::UnaryOp(id), pos, func),
            OpType::Binary => self.push_instr(Instruction::BinaryOp(id), pos, func),
            _ => panic!()
        }
    }

    fn new_var(&mut self, name: &str, scope: usize) -> usize {
        let id = self.var_count;
        self.var_count += 1;
        self.scopes[scope].vars.insert(name.into(), id);
        id
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

    pub fn compile(&mut self, statements: Vec<Node>) -> Result<()> {
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });
        self.functions.push(Function::new(Vec::new()));

        for statement in statements {
            self.compile_node(statement, false, 0, 0)?;
        }
        Ok(())
    }

    fn compile_node(&mut self, node: Node, is_expr: bool, scope: usize, func: usize) -> Result<()> {
        let Node { data, ref pos } = node;

        match data {
            NodeType::Statements(stmts) => {
                for stmt in stmts {
                    self.compile_node(stmt, false, scope, func)?;
                }
            }

            NodeType::Return(expr) => {
                todo!("return {expr:?}")
            }

            NodeType::Break(expr) => {
                todo!("break {expr:?}")
            }

            NodeType::Continue(expr) => {
                todo!("continue {expr:?}")
            }

            NodeType::Exit(expr) => {
                if let Some(expr) = *expr {
                    self.compile_node(expr, true, scope, func)?;
                    self.push_instr(Instruction::Print(PrintMode::Default), pos, func);
                }
                self.push_instr(Instruction::Exit, pos, func);
            }

            NodeType::Assign { target, op, value } => {
                if !op.is_empty() {
                    match target.data {
                        NodeType::Index { target, index } => {
                            self.compile_node(*target, true, scope, func)?;
                            self.compile_node(*index, true, scope, func)?;
                            self.push_instr(Instruction::DupTwo, pos, func); // duplicate target and index for storing later
                            self.push_instr(Instruction::BinaryIndex, pos, func);
                            self.compile_node(*value, true, scope, func)?;
                            self.push_op(OpType::Binary, &op, pos, func);
                            if is_expr {
                                self.push_instr(Instruction::DupOne, pos, func);
                                self.push_instr(Instruction::RotFour, pos, func);
                            }
                            self.push_instr(Instruction::StoreIndex, pos, func);
                        }
    
                        NodeType::Variable(name) => {
                            let id = self.compile_variable(&name, &target.pos, scope, func)?;
                            self.compile_node(*value, true, scope, func)?;
                            self.push_op(OpType::Binary, &op, pos, func);
                            if is_expr {
                                self.push_instr(Instruction::DupOne, pos, func);
                            }
                            self.push_instr(Instruction::StoreVar(id), pos, func);
                        }
    
                        _ => unreachable!()
                    }
                } else {
                    match target.data {
                        NodeType::Index { target, index } => {
                            self.compile_node(*target, true, scope, func)?;
                            self.compile_node(*index, true, scope, func)?;
                            self.compile_node(*value, true, scope, func)?;
                            if is_expr {
                                self.push_instr(Instruction::DupOne, pos, func);
                                self.push_instr(Instruction::RotFour, pos, func);
                            }
                            self.push_instr(Instruction::StoreIndex, pos, func);
                        }
    
                        NodeType::Variable(name) => {
                            self.compile_node(*value, true, scope, func)?;
                            if is_expr {
                                self.push_instr(Instruction::DupOne, pos, func);
                            }
                            let id = self.get_var(&name, scope).unwrap_or_else(|| self.new_var(&name, scope));
                            self.push_instr(Instruction::StoreVar(id), pos, func);
                        }
    
                        _ => unreachable!()
                    }
                }
            }

            NodeType::MultipleAssign { targets, value } => {
                todo!("multiple assign {targets:?}, {value:?}")
            }

            NodeType::If { cond, on_true, on_false } => {
                todo!("if {cond:?}, {on_true:?}, {on_false:?}")
            }

            NodeType::For { iter, vars, mode, block } => {
                todo!("for {iter:?}, {vars:?}, {mode:?}, {block:?}")
            }

            NodeType::While { cond, mode, block } => {
                todo!("while {cond:?}, {mode:?}, {block:?}")
            }

            NodeType::Loop { mode, block } => {
                todo!("loop {mode:?}, {block:?}")
            }

            NodeType::Function { args, block } => {
                todo!("function {args:?}, {block:?}")
            }

            NodeType::UnaryOp { target, op } => {
                // TODO if const args then run op at compile time
                self.compile_node(*target, true, scope, func)?;
                self.push_instr(Instruction::UnaryOp(op), pos, func);
            },

            NodeType::BinOp { a, op, b } => {
                // TODO if const args then run op at compile time
                self.compile_node(*a, true, scope, func)?;
                self.compile_node(*b, true, scope, func)?;
                self.push_instr(Instruction::BinaryOp(op), pos, func);
            },

            NodeType::Compare { first, chain } => {
                todo!("compare {first:?}, {chain:?}")
            }

            NodeType::Increment { target, mode } => {
                let compile_op_part = |s: &mut Compiler| {
                    s.push_const(ValueType::Number(1.0), pos, func);
                    s.push_op(OpType::Binary, if mode.add() { "+" } else { "-" }, pos, func);
                    if is_expr && mode.bef() {
                        s.push_instr(Instruction::DupOne, pos, func);
                        s.push_instr(Instruction::RotFour, pos, func);
                    }
                };
        
                match target.data {
                    NodeType::Index { target, index } => {
                        self.compile_node(*target, true, scope, func)?;
                        self.compile_node(*index, true, scope, func)?;
                        self.push_instr(Instruction::DupTwo, pos, func); // duplicate target and index for storing later
                        self.push_instr(Instruction::BinaryIndex, pos, func);
                        if is_expr && mode.aft() {
                            self.push_instr(Instruction::DupOne, pos, func);
                            self.push_instr(Instruction::RotFour, pos, func);
                        }
                        compile_op_part(self);
                        self.push_instr(Instruction::StoreIndex, pos, func);
                    }
        
                    NodeType::Variable(name) => {
                        let id = self.compile_variable(&name, pos, scope, func)?;
                        if is_expr && mode.aft() {
                            self.push_instr(Instruction::DupOne, pos, func);
                        }
                        compile_op_part(self);
                        self.push_instr(Instruction::StoreVar(id), pos, func);
                    }
        
                    _ => unreachable!()
                }
            }

            NodeType::FnCall { target, args } => {
                todo!("fn call {target:?}, {args:?}")
            }

            NodeType::Index { target, index } => {
                self.compile_node(*target, true, scope, func)?;
                self.compile_node(*index, true, scope, func)?;
                self.push_instr(Instruction::BinaryIndex, pos, func);
            }

            NodeType::Slice { target, start, stop, step } => {
                todo!("slice {target:?}, {start:?}, {stop:?}, {step:?}")
            }

            NodeType::BracketIndex { target, mode, value } => {
                todo!("bracket index {target:?}, {mode:?}, {value:?}")
            }

            NodeType::BracketIter { target, mode, expr } => {
                todo!("bracket iter {target:?}, {mode:?}, {expr:?}")
            }

            NodeType::BraceIter { target, mode } => {
                todo!("brace iter {target:?}, {mode:?}")
            }

            NodeType::Replace { target, mode, left, right } => {
                todo!("replace {target:?}, {mode:?}, {left:?}, {right:?}")
            }

            NodeType::CharReplace { target, mode, left, right } => {
                todo!("char replace {target:?}, {mode:?}, {left:?}, {right:?}")
            }

            NodeType::Print { value, mode } => {
                self.compile_node(*value, true, scope, func)?;
                self.push_instr(Instruction::Print(mode), pos, func);
            }

            NodeType::Input { prompt, mode } => {
                if let Some(prompt) = *prompt {
                    self.compile_node(prompt, true, scope, func)?;
                    self.push_instr(Instruction::Print(PrintMode::NoNewline), pos, func);
                    self.push_instr(Instruction::PopOne, pos, func);
                }
                self.push_instr(Instruction::Input, pos, func);
                match mode {
                    InputMode::Number => self.push_op(OpType::After, "$", pos, func),
                    InputMode::NumberList => self.push_op(OpType::After, "#$", pos, func),
                    _ => ()
                }
            }

            NodeType::Group(inner) => {
                self.compile_node(*inner, true, scope, func)?;
            }

            NodeType::List(list) => {
                let len = list.len();
                for el in list {
                    self.compile_node(el, true, scope, func)?;
                }
                self.push_instr(Instruction::BuildList(len), pos, func);
            }

            NodeType::Variable(var) => {
                self.compile_variable(&var, pos, scope, func)?;
            },

            NodeType::String(frags) => {
                todo!("string {frags:?}")
            }

            NodeType::Boolean(b) => self.push_const(ValueType::Boolean(b), pos, func),
            NodeType::Number(float) => self.push_const(ValueType::Number(float), pos, func),
            NodeType::Null => self.push_const(ValueType::Null(), pos, func),
        };

        if !is_expr {
            self.push_instr(Instruction::PopOne, pos, func);
        }

        Ok(())
    }

    fn compile_variable(&mut self, name: &str, pos: &Pos, scope: usize, func: usize) -> Result<usize> {
        match self.get_var(name, scope) {
            Some(id) => {
                self.push_instr(Instruction::LoadVar(id), pos, func);
                Ok(id)
            },
            None => Err(
                Error::err("Use of undefined variable")
                    .label(pos.clone(), format!("Couldn't find variable '{name}' in this scope"))
            )
        }
    }
}
