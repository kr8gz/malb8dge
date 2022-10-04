use crate::parse::{ast::*, parser::*};

use super::instructions::*;

#[derive(Debug)]
pub struct Compiler {
    pub constants: Stack<Constant>,
    pub functions: Vec<Function>,
}

impl Compiler {
    pub fn new(parser: Parser) -> Self {
        let mut compiler = Self {
            constants: Stack::new(),
            functions: Vec::new(),
        };

        compiler.compile_all(parser.statements);
        compiler
    }

    fn push_instr(&mut self, instr: Instruction, func: usize) {
        self.functions[func].instructions.push(instr);
    }

    fn compile_all(&mut self, statements: Vec<Node>) {
        self.functions.push(Function::new());

        for statement in statements {
            self.compile(statement, 0);
        }
    }

    fn compile(&mut self, node: Node, func: usize) {
        match node.data {
            NodeType::Statements(stmts) => {
                todo!("stmts {stmts:?}")
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
                todo!("exit {expr:?}")
            }

            NodeType::Assign { target, op, value } => {
                todo!("assign {target:?}, {op:?}, {value:?}")
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

            NodeType::BefOp { target, op } | NodeType::AftOp { target, op } => {
                self.compile(*target, func);
                self.push_instr(Instruction::Operator(op), func);
            },

            NodeType::BinOp { a, op, b } => {
                self.compile(*a, func);
                self.compile(*b, func);
                self.push_instr(Instruction::Operator(op), func);
            },

            NodeType::Compare { first, chain } => {
                todo!("compare {first:?}, {chain:?}")
            }

            NodeType::FnCall { target, args } => {
                todo!("fn call {target:?}, {args:?}")
            }

            NodeType::Index { target, index } => {
                todo!("index {target:?}, {index:?}")
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
                todo!("print {value:?}, {mode:?}")
            }

            NodeType::Input { prompt, mode } => {
                todo!("input {prompt:?}, {mode:?}")
            }

            NodeType::IncrementBef { target, mode } | NodeType::IncrementAft { target, mode } => {
                todo!("increment {target:?}, {mode:?}")
            }

            NodeType::Group(inner) => {
                todo!("group {inner:?}")
            }

            NodeType::List(list) => {
                let len = list.len();
                for el in list {
                    self.compile(el, func);
                }
                self.push_instr(Instruction::BuildList(len), func);
            },

            NodeType::Variable(var) => {
                todo!("var {var:?}")
            },

            NodeType::Keyword(kw) => {
                let id = self.constants.add(Constant::Keyword(kw));
                self.push_instr(Instruction::LoadConst(id), func);
            },

            NodeType::String(frags) => {
                todo!("string {frags:?}")
            }

            NodeType::Integer(int) => {
                let id = self.constants.add(Constant::Integer(int));
                self.push_instr(Instruction::LoadConst(id), func);
            },

            NodeType::Float(float) => {
                let id = self.constants.add(Constant::Float(float));
                self.push_instr(Instruction::LoadConst(id), func);
            },
        };
    }
}
