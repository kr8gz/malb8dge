use crate::{compile::instructions::*, util::Pos};

#[derive(Clone, Debug, PartialEq)]
pub struct Value {
    pub data: ValueType,
    pub pos: Pos,
}

impl Value {
    pub fn type_name(&self) -> String {
        self.data.type_name()
    }

    pub fn to_string(&self, memory: &Stack<Value>) -> String {
        self.data.to_string(memory)
    }

    fn repr(&self, memory: &Stack<Value>) -> String {
        self.data.repr(memory)
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
            pub fn into_value(self, pos: &Pos) -> Value {
                Value { data: self, pos: pos.clone() }
            }
        
            pub fn type_name(&self) -> String {
                match self {
                    $( Self::$name(..) => stringify!($name).to_ascii_lowercase(), )*
                }
            }
        
            pub fn to_string(&self, memory: &Stack<Value>) -> String {
                match self {
                    Self::Function(_) => "<function>".into(),
                    Self::List(list) => {
                        format!("[{}]", list.iter().map(|&v| memory[v].repr(memory)).collect::<Vec<_>>().join(", "))
                    },
                    Self::Boolean(b) => b.to_string(),
                    Self::String(s) => s.clone(),
                    Self::Integer(int) => int.to_string(),
                    Self::Float(float) => float.to_string(),
                    Self::Null() => String::new(),
                }
            }
        
            fn repr(&self, memory: &Stack<Value>) -> String {
                match self {
                    Self::String(s) => format!("\"{s}\""),
                    Self::Null() => "null".into(),
                    _ => self.to_string(memory),
                }
            }
        }
    }
}

types! {
    Function(Function),
    List(Vec<usize>),
    Boolean(bool),
    String(String),
    Integer(i64),
    Float(f64),
    Null(),
}
