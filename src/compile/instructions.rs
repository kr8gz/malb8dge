use std::collections::HashMap;

use crate::{parse::ast::PrintMode, util::Pos};

#[derive(Clone, Debug, PartialEq)]
pub struct Function {
    pub instructions: Vec<InstrData>,
    pub args: Vec<usize>,
}

impl Function {
    pub fn new(args: Vec<usize>) -> Self {
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
    LoadConst(usize),
    LoadVar(usize),
    StoreVar(usize), // pop and store
    StoreIndex, // s2.s1 = s0 // pop all 3
    
    BuildList(usize),

    BeforeOp(usize),
    AfterOp(usize),
    BinaryOp(usize), // s1 X s0
    BinaryIndex, // s1.s0

    Print(PrintMode),
    Input,

    PopOne,
    DupOne,
    DupTwo,
    RotThree, // top goes 2 back
    RotFour,

    Exit,
}

#[derive(Clone, Debug, PartialEq)]
pub struct InstrData {
    pub data: Instruction,
    pub pos: Pos,
}
