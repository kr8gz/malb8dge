use crate::{compile::instructions::*, util::Pos};

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
    Integer(i64),
    Float(f64),
    Boolean(bool),
    Null(),
}
