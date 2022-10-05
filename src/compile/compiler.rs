use std::collections::HashMap;

use ariadne::ReportKind;

use crate::{parse::{ast::*, parser::*}, errors::Error, operators::{self, OpType, Pos}};

use super::instructions::*;

#[derive(Debug)]
pub struct Compiler {
    pub file: String,

    pub constants: Stack<Constant>,
    pub functions: Vec<Function>,
    pub scopes: Vec<Scope>,
    pub var_count: usize,

    show_warnings: bool,
}

impl Compiler {
    pub fn new(parser: Parser, warnings: bool) -> Self {
        let mut compiler = Self {
            file: parser.file,

            constants: Stack::new(),
            functions: Vec::new(),
            scopes: Vec::new(),
            var_count: 0,

            show_warnings: warnings,
        };

        compiler.compile_all(parser.statements);
        compiler
    }

    fn warn(&self, msg: &str) -> Error {
        Error::new(self.file.clone(), msg, ReportKind::Warning)
    }

    fn error(&self, msg: &str) -> Error {
        Error::new(self.file.clone(), msg, ReportKind::Error)
    }

    fn push_instr(&mut self, instr: Instruction, func: usize) {
        self.functions[func].instructions.push(instr);
    }

    fn push_const(&mut self, constant: Constant, func: usize) {
        let id = self.constants.add(constant);
        self.push_instr(Instruction::LoadConst(id), func);
    }

    fn push_op(&mut self, op_type: OpType, op: &str, func: usize) {
        let id = operators::op_id(op_type, op);
        match op_type {
            OpType::Before | OpType::After => self.push_instr(Instruction::UnaryOp(id), func),
            OpType::Binary => self.push_instr(Instruction::BinaryOp(id), func),
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

    fn compile_all(&mut self, statements: Vec<Node>) {
        self.scopes.push(Scope {
            vars: HashMap::new(),
            parent: None,
            children: Vec::new(),
        });
        self.functions.push(Function::new());

        for statement in statements {
            self.compile_statement(statement, 0, 0);
        }
    }

    fn compile_or_null<F: Fn(&mut Self, Node)>(&mut self, node: Option<Node>, func: usize, f: F) {
        match node {
            Some(node) => f(self, node),
            None => self.push_const(Constant::Keyword(Keyword::Null), func),
        }
    }

    fn compile_assign(&mut self, target: Node, op: String, value: Node, is_expr: bool, scope: usize, func: usize) {
        if !op.is_empty() {
            match target.data {
                NodeType::Index { target, index } => {
                    self.compile_node(*target, scope, func);
                    self.compile_node(*index, scope, func);
                    self.push_instr(Instruction::DupTwo, func); // duplicate target and index for storing
                    self.push_instr(Instruction::BinaryIndex, func);
                    self.compile_node(value, scope, func);
                    self.push_op(OpType::Binary, &op, func);
                    if is_expr { self.push_instr(Instruction::DupOne, func); }
                    self.push_instr(Instruction::RotThree, func);
                    self.push_instr(Instruction::StoreIndex, func);
                }

                NodeType::Variable(name) => {
                    let id = self.compile_variable(&name, target.pos, scope, func);
                    self.compile_node(value, scope, func);
                    self.push_op(OpType::Binary, &op, func);
                    if is_expr { self.push_instr(Instruction::DupOne, func); }
                    self.push_instr(Instruction::StoreVar(id), func);
                }

                _ => unreachable!()
            }
        } else {
            self.compile_node(value, scope, func);
            if is_expr { self.push_instr(Instruction::DupOne, func); }

            match target.data {
                NodeType::Index { target, index } => {
                    self.compile_node(*target, scope, func);
                    self.compile_node(*index, scope, func);
                    self.push_instr(Instruction::StoreIndex, func);
                }

                NodeType::Variable(name) => {
                    let id = self.get_var(&name, scope).unwrap_or_else(|| self.new_var(&name, scope));
                    self.push_instr(Instruction::StoreVar(id), func);
                }

                _ => unreachable!()
            }
        }
    }

    fn compile_node(&mut self, node: Node, scope: usize, func: usize) {
        match node.data {
            NodeType::Statements(stmts) => {
                for stmt in stmts {
                    self.compile_statement(stmt, scope, func);
                }
            }

            NodeType::Assign { target, op, value } => {
                self.compile_assign(*target, op, *value, true, scope, func);
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
                self.compile_node(*target, scope, func);
                self.push_instr(Instruction::UnaryOp(op), func);
            },

            NodeType::BinOp { a, op, b } => {
                // TODO if const args then run op at compile time
                self.compile_node(*a, scope, func);
                self.compile_node(*b, scope, func);
                self.push_instr(Instruction::BinaryOp(op), func);
            },

            NodeType::Compare { first, chain } => {
                todo!("compare {first:?}, {chain:?}")
            }

            NodeType::FnCall { target, args } => {
                todo!("fn call {target:?}, {args:?}")
            }

            NodeType::Index { target, index } => {
                self.compile_node(*target, scope, func);
                self.compile_node(*index, scope, func);
                self.push_instr(Instruction::BinaryIndex, func);
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

            NodeType::Replace { target, mode, pairs } => {
                todo!("replace {target:?}, {mode:?}, {pairs:?}")
            }

            NodeType::CharReplace { target, mode, pairs } => {
                todo!("char replace {target:?}, {mode:?}, {pairs:?}")
            }

            NodeType::Print { value, mode } => {
                self.compile_or_null(*value, func, |s, node| {
                    s.compile_node(node, scope, func);
                });
                self.push_instr(Instruction::Print(mode as usize), func);
            }

            NodeType::Input { prompt, mode } => {
                if let Some(prompt) = *prompt {
                    self.compile_node(prompt, scope, func);
                    self.push_instr(Instruction::Print(PrintMode::NoNewline as usize), func);
                    self.push_instr(Instruction::PopOne, func);
                }

                self.push_instr(Instruction::Input, func);
                
                match mode {
                    InputMode::String => (),
                    InputMode::Number => self.push_op(OpType::After, "$", func),
                    InputMode::NumberList => self.push_op(OpType::After, "#$", func),
                }
            }

            NodeType::Group(inner) => {
                self.compile_node(*inner, scope, func);
            }

            NodeType::List(list) => {
                let len = list.len();
                for el in list {
                    self.compile_node(el, scope, func);
                }
                self.push_instr(Instruction::BuildList(len), func);
            }

            NodeType::Variable(var) => {
                self.compile_variable(&var, node.pos, scope, func);
            },

            NodeType::String(frags) => {
                todo!("string {frags:?}")
            }

            NodeType::Keyword(kw) => self.push_const(Constant::Keyword(kw), func),
            NodeType::Integer(int) => self.push_const(Constant::Integer(int), func),
            NodeType::Float(float) => self.push_const(Constant::Float(float), func),
            
            _ => unreachable!()
        };
    }

    fn compile_statement(&mut self, node: Node, scope: usize, func: usize) {
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
                    self.compile_node(expr, scope, func);
                    self.push_instr(Instruction::Print(PrintMode::Normal as usize), func);
                }
                self.push_instr(Instruction::Exit, func);
            }

            NodeType::Assign { target, op, value } => {
                self.compile_assign(*target, op, *value, false, scope, func);
            }

            NodeType::MultipleAssign { targets, value } => {
                todo!("multiple assign {targets:?}, {value:?}")
            }

            _ => {
                self.compile_node(node, scope, func);
                self.push_instr(Instruction::PopOne, func);
            }
        }
    }

    fn compile_variable(&mut self, var: &str, pos: Pos, scope: usize, func: usize) -> usize {
        match self.get_var(var, scope) {
            Some(id) => {
                self.push_instr(Instruction::LoadVar(id), func);
                id
            },
            None => {
                self.error("Use of undefined variable")
                    .label(pos, format!("Couldn't find variable '{var}' in this scope"))
                    .eprint();
            }
        }
    }
}
