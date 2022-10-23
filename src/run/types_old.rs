use std::ops::{Index, IndexMut};

use itertools::Itertools;

use crate::{compile::instructions::*, util::{Pos, operators::Operand}};

#[derive(Debug)]
pub struct Stack(pub Vec<Value>);

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: Value) -> usize {
        match self.0.iter().position(|v| *v == value) {
            Some(id) => id,
            None => {
                self.0.push(value);
                self.0.len() - 1
            }
        }
    }

    pub fn get_operand(&self, id: usize) -> Operand {
        let cloned = self[id].clone();
        Operand {
            id,
            type_name: cloned.type_name(),
            data: cloned.data,
            pos: cloned.pos,
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
        
impl Value {
    pub fn type_name(&self) -> String {
        self.data.type_name()
    }
}

macro_rules! types {
    (
        $( $name:ident($( $value:ty )?), )*
    ) => {
        #[derive(Clone, Debug, PartialEq)]
        pub enum ValueType {
            $( $name($( $value )?), )*
        }

        impl ValueType {
            pub fn type_name(&self) -> String {
                match self {
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
            Self::Function(_) => true,
            Self::Boolean(b) => *b,
            Self::Null() => false,
        }
    }

    pub fn as_string(&self, memory: &Stack) -> String {
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

    pub fn as_repr_string(&self, memory: &Stack) -> String {
        match self {
            Self::String(s) => format!("\"{s}\""),
            Self::Null() => "null".into(),
            _ => self.as_string(memory),
        }
    }
}

pub fn join_list(list: &[usize], sep: &str, memory: &Stack) -> String {
    list.iter()
        .map(|&v| memory[v].data.as_string(memory))
        .join(sep)
}
