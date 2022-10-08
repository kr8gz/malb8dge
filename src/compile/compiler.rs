use std::collections::HashMap;

use crate::{parse::ast::*, util::{errors::Error, operators::{self, OpType}, Pos}, run::types::*};

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

    pub fn compile(&mut self, statements: Vec<Node>) -> Result<(), Error> {
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });
        self.functions.push(Function::new(Vec::new()));

        for statement in statements {
            self.compile_statement(statement, 0, 0)?;
        }
        Ok(())
    }

    fn compile_assign(&mut self, node: Node, is_expr: bool, scope: usize, func: usize) -> Result<(), Error> {
        if let Node { data: NodeType::Assign { target, op, value }, ref pos } = node {
            if !op.is_empty() {
                match target.data {
                    NodeType::Index { target, index } => {
                        self.compile_node(*target, scope, func)?;
                        self.compile_node(*index, scope, func)?;
                        self.push_instr(Instruction::DupTwo, pos, func); // duplicate target and index for storing later
                        self.push_instr(Instruction::BinaryIndex, pos, func);
                        self.compile_node(*value, scope, func)?;
                        self.push_op(OpType::Binary, &op, pos, func);
                        if is_expr { self.push_instr(Instruction::DupOne, pos, func); }
                        self.push_instr(Instruction::RotThree, pos, func);
                        self.push_instr(Instruction::StoreIndex, pos, func);
                    }

                    NodeType::Variable(name) => {
                        let id = self.compile_variable(&name, &target.pos, scope, func)?;
                        self.compile_node(*value, scope, func)?;
                        self.push_op(OpType::Binary, &op, pos, func);
                        if is_expr { self.push_instr(Instruction::DupOne, pos, func); }
                        self.push_instr(Instruction::StoreVar(id), pos, func);
                    }

                    _ => unreachable!()
                }
            } else {
                self.compile_node(*value, scope, func)?;
                if is_expr { self.push_instr(Instruction::DupOne, pos, func); }

                match target.data {
                    NodeType::Index { target, index } => {
                        self.compile_node(*target, scope, func)?;
                        self.compile_node(*index, scope, func)?;
                        self.push_instr(Instruction::StoreIndex, pos, func);
                    }

                    NodeType::Variable(name) => {
                        let id = self.get_var(&name, scope).unwrap_or_else(|| self.new_var(&name, scope));
                        self.push_instr(Instruction::StoreVar(id), pos, func);
                    }

                    _ => unreachable!()
                }
            }
            Ok(())
        } else {
            panic!("compile_assign did not get assign node");
        }
    }

    fn compile_node(&mut self, node: Node, scope: usize, func: usize) -> Result<(), Error> {
        match node.data {
            NodeType::Statements(stmts) => {
                for stmt in stmts {
                    self.compile_statement(stmt, scope, func)?;
                }
            }

            NodeType::Assign { .. } => {
                self.compile_assign(node, true, scope, func)?;
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
                self.compile_node(*target, scope, func)?;
                self.push_instr(Instruction::UnaryOp(op), &node.pos, func);
            },

            NodeType::BinOp { a, op, b } => {
                // TODO if const args then run op at compile time
                self.compile_node(*a, scope, func)?;
                self.compile_node(*b, scope, func)?;
                self.push_instr(Instruction::BinaryOp(op), &node.pos, func);
            },

            NodeType::Compare { first, chain } => {
                todo!("compare {first:?}, {chain:?}")
            }

            NodeType::FnCall { target, args } => {
                todo!("fn call {target:?}, {args:?}")
            }

            NodeType::Index { target, index } => {
                self.compile_node(*target, scope, func)?;
                self.compile_node(*index, scope, func)?;
                self.push_instr(Instruction::BinaryIndex, &node.pos, func);
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
                self.compile_node(*value, scope, func)?;
                self.push_instr(Instruction::Print(mode), &node.pos, func);
            }

            NodeType::Input { prompt, mode } => {
                if let Some(prompt) = *prompt {
                    self.compile_node(prompt, scope, func)?;
                    self.push_instr(Instruction::Print(PrintMode::NoNewline), &node.pos, func);
                    self.push_instr(Instruction::PopOne, &node.pos, func);
                }
                self.push_instr(Instruction::Input, &node.pos, func);
                match mode {
                    InputMode::Number => self.push_op(OpType::After, "$", &node.pos, func),
                    InputMode::NumberList => self.push_op(OpType::After, "#$", &node.pos, func),
                    _ => ()
                }
            }

            NodeType::Group(inner) => {
                self.compile_node(*inner, scope, func)?;
            }

            NodeType::List(list) => {
                let len = list.len();
                for el in list {
                    self.compile_node(el, scope, func)?;
                }
                self.push_instr(Instruction::BuildList(len), &node.pos, func);
            }

            NodeType::Variable(var) => {
                self.compile_variable(&var, &node.pos, scope, func)?;
            },

            NodeType::String(frags) => {
                todo!("string {frags:?}")
            }

            NodeType::Boolean(b) => self.push_const(ValueType::Boolean(b), &node.pos, func),
            NodeType::Integer(int) => self.push_const(ValueType::Integer(int), &node.pos, func),
            NodeType::Float(float) => self.push_const(ValueType::Float(float), &node.pos, func),
            NodeType::Null => self.push_const(ValueType::Null(), &node.pos, func),
            
            _ => unreachable!()
        };
        Ok(())
    }

    fn compile_statement(&mut self, node: Node, scope: usize, func: usize) -> Result<(), Error> {
        match node.data {
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
                    self.compile_node(expr, scope, func)?;
                    self.push_instr(Instruction::Print(PrintMode::Default), &node.pos, func);
                }
                self.push_instr(Instruction::Exit, &node.pos, func);
            }

            NodeType::Assign { .. } => {
                self.compile_assign(node, false, scope, func)?;
            }

            NodeType::MultipleAssign { targets, value } => {
                todo!("multiple assign {targets:?}, {value:?}")
            }

            NodeType::Print { value, mode } => {
                self.compile_node(*value, scope, func)?;
                self.push_instr(Instruction::Print(mode), &node.pos, func);
                self.push_instr(Instruction::PopOne, &node.pos, func);
            }

            _ => {
                let pos = node.pos.clone();
                self.compile_node(node, scope, func)?;
                self.push_instr(Instruction::PopOne, &pos, func);
            }
        }
        Ok(())
    }

    fn compile_variable(&mut self, var: &str, pos: &Pos, scope: usize, func: usize) -> Result<usize, Error> {
        match self.get_var(var, scope) {
            Some(id) => {
                self.push_instr(Instruction::LoadVar(id), pos, func);
                Ok(id)
            },
            None => Err(
                Error::err("Use of undefined variable")
                    .label(pos.clone(), format!("Couldn't find variable '{var}' in this scope"))
            )
        }
    }
}
