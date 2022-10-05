use std::collections::HashMap;

use crate::parse::ast::Keyword;

type Id = usize;
type Len = usize;
type Mode = usize;
type Idx = usize;

#[derive(Debug)]
pub struct Stack<T>(pub Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl<T: PartialEq> Stack<T> {
    pub fn add(&mut self, value: T) -> Id {
        match self.0.iter().position(|v| v == &value) {
            Some(id) => id,
            None => {
                self.0.push(value);
                self.0.len() - 1
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Constant {
    Keyword(Keyword),
    Literal(String),
    Integer(u32),
    Float(f64),
}

#[derive(Debug)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub args: Vec<Id>,
}

impl Function {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            args: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, usize>,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
}

#[derive(Debug)]
pub enum Instruction {
    LoadConst(Id),
    LoadVar(Id),
    StoreVar(Id), // pop and store
    StoreIndex, // s1.s0 = s2 // pop all 3
    
    BuildList(Len),

    UnaryOp(Id),
    BinaryOp(Id), // s1 X s0
    BinaryIndex, // s1.s0

    Print(Mode),
    Input,

    PopOne,
    DupOne,
    DupTwo,
    RotThree, // top goes 2 back

    Exit,
}
