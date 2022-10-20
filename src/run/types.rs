use std::ops::{Index, IndexMut};

use itertools::Itertools;

use crate::{compile::instructions::*, util::{Pos, operators::Operand}};

#[derive(Debug)]
pub struct Stack<T: PartialEq>(pub Vec<T>);

impl<T: PartialEq> Stack<T> {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: T) -> usize {
        match self.0.iter().position(|v| *v == value) {
            Some(id) => id,
            None => {
                self.0.push(value);
                self.0.len() - 1
            }
        }
    }
}

impl<T: PartialEq> Index<usize> for Stack<T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl<T: PartialEq> IndexMut<usize> for Stack<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.0[index]
    }
}

pub trait RemoveElement<T: PartialEq> {
    fn remove_element(&mut self, element: &T);
}

impl<T: PartialEq> RemoveElement<T> for Vec<T> {
    fn remove_element(&mut self, element: &T) {
        if let Some(pos) = self.iter().position(|el| el == element) {
            self.remove(pos);
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub data: ValueType,
    pub pos: Pos,
}

macro_rules! types {
    (
        $( $name:ident($( $value:ty )?), )*
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum ValueType {
            $( $name($( $value )?), )*
        }
        
        impl Value {
            pub fn type_name(&self) -> String {
                match self.data {
                    $( ValueType::$name(..) => stringify!($name).to_ascii_lowercase(), )*
                }
            }
        }
    }
}

types! {
    Function(Function),
    List(Vec<usize>),
    String(String),
    Number(f64),
    Boolean(bool),
    Null(),
}
        
impl ValueType {
    pub fn into_value(self, pos: &Pos) -> Value {
        Value { data: self, pos: pos.clone() }
    }
    
    pub fn as_int(&self) -> Option<f64> {
        match self {
            Self::List(list) => Some(list.len() as f64),
            Self::String(s) => s.parse::<f64>().ok().map(|n| n.floor()),
            Self::Number(num) => Some(num.floor()),
            Self::Boolean(true) => Some(1.0),
            Self::Boolean(false) | Self::Null() => Some(0.0),
            _ => None
        }
    }

    pub fn as_num(&self) -> Option<f64> {
        match self {
            Self::Number(num) => Some(*num),
            _ => self.as_int()
        }
    }

    pub fn as_bool(&self) -> bool {
        match self {
            Self::List(list) => !list.is_empty(),
            Self::String(s) => !s.is_empty(),
            Self::Number(num) => *num != 0.0,
            Self::Boolean(_) | Self::Function(_) => true,
            Self::Null() => false,
        }
    }

    pub fn as_string(&self, memory: &Stack<Value>) -> String {
        match self {
            Self::Function(_) => "<function>".into(),
            Self::List(list) => {
                format!("[{}]", list.iter().map(|&v| memory[v].data.as_repr_string(memory)).collect::<Vec<_>>().join(", "))
            },
            Self::Boolean(b) => b.to_string(),
            Self::String(s) => s.clone(),
            Self::Number(num) => num.to_string(),
            Self::Null() => "".into(),
        }
    }

    pub fn as_repr_string(&self, memory: &Stack<Value>) -> String {
        match self {
            Self::String(s) => format!("\"{s}\""),
            Self::Null() => "null".into(),
            _ => self.as_string(memory),
        }
    }
}

pub fn join_list(list: &[usize], sep: &str, memory: &Stack<Value>) -> String {
    list.iter()
        .map(|&v| memory[v].data.as_string(memory))
        .join(sep)
}
