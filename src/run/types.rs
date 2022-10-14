use std::ops::{Index, IndexMut};

use crate::{compile::instructions::*, util::Pos};

#[derive(Debug)]
pub struct Stack(pub Vec<Value>);

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: Value) -> usize {
        match self.0.iter().position(|v| !matches!(v.data, ValueType::List(_)) && v.data == value.data) {
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
        
        impl ValueType {
            pub fn into_value(self, pos: &Pos) -> Value {
                Value { data: self, pos: pos.clone() }
            }
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
