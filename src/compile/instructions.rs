use std::{collections::HashMap, ops::{Index, IndexMut}};

use crate::{parse::ast::PrintMode, util::Pos, run::types::Value};

type Id = usize;
type Len = usize;

#[derive(Debug)]
pub struct Stack(pub Vec<Value>);

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: Value) -> Id {
        match self.0.iter().position(|v| v.data == value.data) {
            Some(id) => id,
            None => {
                self.0.push(value);
                self.0.len() - 1
            }
        }
    }
}

impl Index<usize> for Stack {
    type Output = Value;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl IndexMut<usize> for Stack {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub instructions: Vec<InstrData>,
    pub args: Vec<Id>,
}

impl Function {
    pub fn new(args: Vec<Id>) -> Self {
        Self { instructions: Vec::new(), args }
    }
}

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, usize>,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instruction {
    LoadConst(Id),
    LoadVar(Id),
    StoreVar(Id), // pop and store
    StoreIndex, // s1.s0 = s2 // pop all 3
    
    BuildList(Len),

    UnaryOp(Id),
    BinaryOp(Id), // s1 X s0
    BinaryIndex, // s1.s0

    Print(PrintMode),
    Input,

    PopOne,
    DupOne,
    DupTwo,
    RotThree, // top goes 2 back

    Exit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InstrData {
    pub data: Instruction,
    pub pos: Pos,
}
