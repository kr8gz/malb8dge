use std::{cmp::Ordering, collections::HashMap, ops::{Index, IndexMut}};

use itertools::Itertools;

use crate::util::Pos;

#[derive(Debug)]
pub struct Stack(Vec<Value>);

impl Stack {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    pub fn push(&mut self, value: Value) -> usize {
        self.0.push(value);
        self.0.len() - 1
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

#[derive(Debug)]
pub struct Scope {
    pub vars: HashMap<String, usize>,
    pub parent: Option<usize>,
    pub children: Vec<usize>,
}

fn f64_to_str(num: f64) -> String {
    let s = num.to_string();
    if s == "-0" { "0".into() } else { s }
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

            (String(a), Number(b)) => match a.parse::<f64>() {
                Ok(a) => a.partial_cmp(b),
                _ => a.partial_cmp(&f64_to_str(*b))
            }

            (Number(a), String(b)) => match b.parse::<f64>() {
                Ok(b) => a.partial_cmp(&b),
                _ => f64_to_str(*a).partial_cmp(b)
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

    pub fn as_string(&self, memory: &Stack) -> String {
        self.data.as_string(memory)
    }

    pub fn as_repr_string(&self, memory: &Stack) -> String {
        self.data.as_repr_string(memory)
    }

    pub fn as_joined_list_string(&self, memory: &Stack, sep: &str) -> String {
        self.data.as_joined_list_string(memory, sep)
    }

    pub fn as_list(&self, memory: &Stack, pos: &Pos) -> Option<Vec<Value>> {
        self.data.as_list(memory, pos)
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
            Self::String(s) => s.parse::<f64>().ok().map(|n| n.trunc()),
            Self::Number(num) => Some(num.trunc()),
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
            Self::Function(index) => format!("<function@{index}>"),
            Self::List(list) => format!(
                "[{}]", list.iter().map(|&v| memory[v].data.as_repr_string(memory)).collect::<Vec<_>>().join(", ")
            ),
            Self::Boolean(b) => b.to_string(),
            Self::String(s) => s.clone(),
            Self::Number(num) => f64_to_str(*num),
            Self::Null() => "".into(),
        }
    }

    pub fn as_repr_string(&self, memory: &Stack) -> String {
        match self {
            Self::String(s) => {
                let mut s = s.clone();
                for ch in ['"', '\\', '{', '}'] {
                    s = s.replace(ch, &format!("\\{ch}"));
                }
                format!("\"{}\"", s.replace('\n', "\\n").replace('\t', "\\t"))
            }
            Self::Null() => "null".into(),
            _ => self.as_string(memory),
        }
    }

    pub fn as_joined_list_string(&self, memory: &Stack, sep: &str) -> String {
        match self {
            ValueType::List(list) => {
                list.iter()
                    .map(|&v| memory[v].as_string(memory))
                    .join(sep)
            }

            _ => {
                let ret = self.as_string(memory);
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

    pub fn as_list(&self, memory: &Stack, pos: &Pos) -> Option<Vec<Value>> {
        match self {
            Self::List(list) => Some(list.iter().map(|&v| memory[v].clone()).collect()),
            Self::String(s) => Some(s.chars().map(|c| ValueType::String(c.into()).into_value(pos)).collect()),
            Self::Number(num) => {
                let num = *num as i64;
                Some(
                    if num < 0 { num+1..1 } else { 0..num }
                        .map(|i| ValueType::Number(i as f64).into_value(pos))
                        .collect()
                )
            }
            _ => None
        }
    }
}
