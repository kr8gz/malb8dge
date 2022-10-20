use std::collections::HashMap;

use crate::{parse::ast::*, util::{*, errors::Error, operators::{self, OpType}, Pos}, run::types::*};

use super::instructions::*;

#[derive(Debug)]
pub struct Compiler {
    pub constants: Stack<ValueType>,
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

    fn push_instr(&mut self, instr: Instruction, pos: &Pos, func: usize) -> usize {
        let instrs = &mut self.functions[func].instructions;
        instrs.push(InstrData { data: instr, pos: pos.clone() });
        instrs.len() - 1
    }

    fn edit_instr(&mut self, func: usize, index: usize, num: usize) {
        use Instruction::*;
        match &mut self.functions[func].instructions[index].data {
            Jump(n) |
            PopJumpIfFalse(n) => *n = num,
            _ => ()
        }
    }

    fn is_const_instr(&self, func: usize, index: usize) -> bool {
        matches!(self.functions[func].instructions[index].data, Instruction::LoadConst(_))
    }

    fn pop_const_instr(&mut self, func: usize, index: usize) -> Option<ValueType> {
        let instrs = &mut self.functions[func].instructions;
        match instrs[index].data {
            Instruction::LoadConst(c) => {
                instrs.remove(index);
                Some(self.constants.0.remove(c))
            }
            _ => None
        }
    }

    fn curr_index(&self, func: usize) -> usize {
        self.functions[func].instructions.len()
    }

    fn push_const(&mut self, constant: ValueType, pos: &Pos, func: usize) {
        let id = self.constants.push(constant);
        self.push_instr(Instruction::LoadConst(id), pos, func);
    }

    fn push_op(&mut self, op_type: OpType, op: &str, pos: &Pos, func: usize) {
        let id = operators::op_id(op_type, op);
        match op_type {
            OpType::Before => self.push_instr(Instruction::BeforeOp(id), pos, func),
            OpType::After => self.push_instr(Instruction::AfterOp(id), pos, func),
            OpType::Binary => self.push_instr(Instruction::BinaryOp(id), pos, func),
            _ => panic!()
        };
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

    pub fn compile(&mut self, mut statements: Vec<Node>) -> Result<()> {
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });
        self.functions.push(Function::new(Vec::new()));

        if let Some(last) = statements.pop() {
            for stmt in statements {
                self.compile_node(stmt, false, 0, 0)?;
            }

            let last_pos = last.pos.clone();
            self.compile_node(last, true, 0, 0)?;
            self.push_instr(Instruction::Print(PrintMode::Default), &last_pos, 0);
            self.push_instr(Instruction::PopOne, &last_pos, 0);
        }
        Ok(())
    }

    fn compile_node(&mut self, node: Node, is_expr: bool, scope: usize, func: usize) -> Result<()> {
        let Node { data, ref pos } = node;

        match data {
            NodeType::Statements(mut statements) => {
                if let Some(last) = statements.pop() {
                    for stmt in statements {
                        self.compile_node(stmt, false, scope, func)?;
                    }
                    self.compile_node(last, true, scope, func)?;
                }
            }

            NodeType::Return(expr) => {
                self.compile_or_null(*expr, pos, scope, func)?;
                self.push_instr(Instruction::Return, pos, func);
            }

            NodeType::Break(expr) => {
                self.compile_or_null(*expr, pos, scope, func)?;
                self.push_instr(Instruction::Break, pos, func);
            }

            NodeType::Continue(expr) => {
                self.compile_or_null(*expr, pos, scope, func)?;
                self.push_instr(Instruction::Continue, pos, func);
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
                            self.push_instr(Instruction::DupOne, pos, func);
                            self.push_instr(Instruction::RotFour, pos, func);
                            self.push_instr(Instruction::StoreIndex, pos, func);
                        }
    
                        NodeType::Variable(name) => {
                            let id = self.compile_variable(&name, &target.pos, scope, func)?;
                            self.compile_node(*value, true, scope, func)?;
                            self.push_op(OpType::Binary, &op, pos, func);
                            self.push_instr(Instruction::DupOne, pos, func);
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
                            self.push_instr(Instruction::DupOne, pos, func);
                            self.push_instr(Instruction::RotFour, pos, func);
                            self.push_instr(Instruction::StoreIndex, pos, func);
                        }
    
                        NodeType::Variable(name) => {
                            self.compile_node(*value, true, scope, func)?;
                            self.push_instr(Instruction::DupOne, pos, func);
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
                // TODO if cond is const then dont compile where it wont go to
                self.compile_node(*cond, true, scope, func)?;

                macro_rules! compile_block {
                    ( $block:ident ) => {
                        if let Some(block) = *$block {
                            let inner = self.new_scope(scope);
                            self.compile_node(block, true, inner, func)?;
                        }
                    }
                }

                let jump_false = self.push_instr(Instruction::PopJumpIfFalse(0), pos, func);
                compile_block!(on_true);
                let jump_end = self.push_instr(Instruction::Jump(0), pos, func);
                self.edit_instr(func, jump_false, jump_end + 1);
                compile_block!(on_false);
                self.edit_instr(func, jump_end, self.curr_index(func))
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

            NodeType::BeforeOp { target, op } => {
                // TODO if const args then run op at compile time
                self.compile_node(*target, true, scope, func)?;
                self.push_instr(Instruction::BeforeOp(op), pos, func);
            },

            NodeType::AfterOp { target, op } => {
                // TODO if const args then run op at compile time
                self.compile_node(*target, true, scope, func)?;
                self.push_instr(Instruction::AfterOp(op), pos, func);
            },

            NodeType::BinOp { a, op, b } => {
                self.compile_node(*a, true, scope, func)?;
                self.compile_node(*b, true, scope, func)?;

                // let curr = self.curr_index(func);
                // if self.is_const_instr(func, curr - 1) && self.is_const_instr(func, curr - 2) {
                //     let rhs = self.pop_const_instr(func, curr - 2).unwrap();
                //     let lhs = self.pop_const_instr(func, curr - 1).unwrap();

                //     operators::run_bin_op(memory, pos, lhs, rhs, op);
                // } else {
                    self.push_instr(Instruction::BinaryOp(op), pos, func);
                // }
            },

            NodeType::Compare { first, chain } => {
                todo!("compare {first:?}, {chain:?}")
            }

            NodeType::Increment { target, mode } => {
                let compile_op_part = |s: &mut Compiler| {
                    s.push_const(ValueType::Number(1.0), pos, func);
                    s.push_op(OpType::Binary, if mode.add() { "+" } else { "-" }, pos, func);
                    if mode.bef() {
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
                        if mode.aft() {
                            self.push_instr(Instruction::DupOne, pos, func);
                            self.push_instr(Instruction::RotFour, pos, func);
                        }
                        compile_op_part(self);
                        self.push_instr(Instruction::StoreIndex, pos, func);
                    }
        
                    NodeType::Variable(name) => {
                        let id = self.compile_variable(&name, pos, scope, func)?;
                        if mode.aft() {
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

            NodeType::Group(node) => self.compile_node(*node, true, scope, func)?,

            NodeType::List(list) => {
                let len = list.len();
                for node in list {
                    self.compile_node(node, true, scope, func)?;
                }
                self.push_instr(Instruction::BuildList(len), pos, func);
            }

            NodeType::Variable(name) => {
                self.compile_variable(&name, pos, scope, func)?;
            }

            NodeType::String(frags) => {
                let concatenations = frags.len() - 1;

                for frag in frags {
                    match frag {
                        ParsedFragment::Literal(lit) => self.push_const(ValueType::String(lit), pos, func),
                        ParsedFragment::Expr(node) => self.compile_node(node, true, scope, func)?,
                    }
                }

                for _ in 0..concatenations {
                    self.push_op(OpType::Binary, "+", pos, func);
                }
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

    fn compile_or_null(&mut self, node: Option<Node>, pos: &Pos, scope: usize, func: usize) -> Result<()> {
        match node {
            Some(node) => self.compile_node(node, true, scope, func)?,
            None => self.push_const(ValueType::Null(), pos, func),
        }
        Ok(())
    }

    fn compile_variable(&mut self, name: &str, pos: &Pos, scope: usize, func: usize) -> Result<usize> {
        match self.get_var(name, scope) {
            Some(id) => {
                self.push_instr(Instruction::LoadVar(id), pos, func);
                Ok(id)
            }
            None => Err(
                Error::err("Use of undefined variable")
                    .label(pos.clone(), format!("Couldn't find variable #{name}# in this scope"))
            )
        }
    }
}
