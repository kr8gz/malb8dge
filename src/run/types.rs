use std::{cmp::Ordering, collections::HashMap};

use itertools::Itertools;

use crate::util::Pos;

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, usize>,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
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

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use ValueType::*;

        match (&self.data, &other.data) {
            (List(a), List(b)) => a.partial_cmp(b),
            (String(a), String(b)) => a.partial_cmp(b),

            (String(a), n @ Number(b)) => match a.parse::<f64>() {
                Ok(a) => a.partial_cmp(b),
                _ => a.partial_cmp(&n.as_string())
            }

            (n @ Number(a), String(b)) => match b.parse::<f64>() {
                Ok(b) => a.partial_cmp(&b),
                _ => n.as_string().partial_cmp(b)
            }

            (String(a), Boolean(b)) => match a.parse::<bool>() {
                Ok(a) => a.partial_cmp(b),
                _ => a.partial_cmp(&b.to_string())
            }

            (Boolean(a), String(b)) => match b.parse::<bool>() {
                Ok(b) => a.partial_cmp(&b),
                _ => a.to_string().partial_cmp(b)
            }

            (String(a), Null()) => a.as_str().partial_cmp(""),
            (Null(), String(b)) => "".partial_cmp(b),
            (a, b) => a.as_num()?.partial_cmp(&b.as_num()?)
        }
    }
}

impl Value {
    pub fn type_name(&self) -> String {
        self.data.type_name()
    }

    pub fn as_int(&self) -> Option<f64> {
        self.data.as_int()
    }

    pub fn as_num(&self) -> Option<f64> {
        self.data.as_num()
    }

    pub fn as_bool(&self) -> bool {
        self.data.as_bool()
    }

    pub fn as_string(&self) -> String {
        self.data.as_string()
    }

    pub fn as_repr_string(&self) -> String {
        self.data.as_repr_string()
    }

    pub fn as_joined_list_string(&self, sep: &str) -> String {
        self.data.as_joined_list_string(sep)
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
    Function(usize),
    List(Vec<Value>),
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

    pub fn as_string(&self) -> String {
        match self {
            Self::Function(index) => format!("<function@{index}>"),
            Self::List(list) => format!("[{}]", list.iter().map(|v| v.data.as_repr_string()).collect::<Vec<_>>().join(", ")),
            Self::Boolean(b) => b.to_string(),
            Self::String(s) => s.clone(),
            Self::Number(num) => {
                let s = num.to_string();
                if s == "-0" { "0".into() } else { s }
            }
            Self::Null() => "".into(),
        }
    }

    pub fn as_repr_string(&self) -> String {
        match self {
            Self::String(s) => {
                let mut s = s.clone();
                for ch in ['"', '\\', '{', '}'] {
                    s = s.replace(ch, &format!("\\{ch}"));
                }
                format!("\"{}\"", s.replace('\n', "\\n").replace('\t', "\\t"))
            }
            Self::Null() => "null".into(),
            _ => self.as_string(),
        }
    }

    pub fn as_joined_list_string(&self, sep: &str) -> String {
        match self {
            ValueType::List(list) => {
                list.iter()
                    .map(|v| v.as_string())
                    .join(sep)
            }

            _ => {
                let ret = self.as_string();
                if sep.is_empty() {
                    return ret
                }
                ret.chars()
                    .map(|c| c.to_string())
                    .intersperse(sep.into())
                    .collect::<String>()
            }
        }
    }
}
