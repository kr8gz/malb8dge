use crate::parse::ast::Keyword;

#[derive(Debug, PartialEq)]
pub enum Constant {
    Integer(u32),
    Float(f64),
    Keyword(Keyword),
}

#[derive(Debug)]
pub struct Function {
    pub instructions: Vec<Instruction>,
    pub args: Vec<usize>,
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
pub struct Stack<T>(pub Vec<T>);

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }
}

impl<T: PartialEq> Stack<T> {
    pub fn add(&mut self, value: T) -> usize {
        match self.0.iter().position(|v| v == &value) {
            Some(id) => id,
            None => {
                self.0.push(value);
                self.0.len() - 1
            }
        }
    }
}

#[derive(Debug)]
pub enum Instruction {
    LoadConst(usize), // ID
    
    BuildList(usize), // len

    Operator(String),
}
