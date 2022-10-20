use std::ops::{Index, IndexMut};

use crate::{compile::instructions::*, util::Pos};

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
