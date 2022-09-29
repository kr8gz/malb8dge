use std::fmt::Display;

use crate::{lexer::*, constants::*};

type BNode = Box<Node>;
type ONode = Box<Option<Node>>;
type VNode = Vec<Node>;

type ReplaceValue = Vec<ParsedFragment>;

#[derive(Debug)]
pub struct Node {
    pub data: NodeType,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum NodeType {
    Statements(VNode),
    Return(ONode),
    Break(ONode),
    Continue(ONode),
    Exit(ONode),
    Assign { target: BNode, op: String, value: BNode },
    MultipleAssign { targets: VNode, value: BNode },
    If { cond: BNode, on_true: ONode, on_false: ONode },
    For { iter: BNode, vars: VNode, mode: IterMode, block: BNode },   // x ~ [vars]  mode    ... // x ~ [vars]   [mode] { ... } //
    While { cond: BNode, mode: IterMode, block: BNode },              // x ~        [mode] ? ... // x ~          [mode] [ ... ] //
    Loop { mode: IterMode, block: BNode },                            //                   ? ... //            ? [mode] { ... } //
    FnDef { index: usize }, // see Function struct
    BefOp { target: BNode, op: String },
    BinOp { a: BNode, op: String, b: BNode },
    AftOp { target: BNode, op: String },
    FnCall { target: BNode, args: VNode },  
    Index { target: BNode, mode: IndexMode, index: BNode },
    Slice { target: BNode, start: ONode, stop: ONode, step: ONode },
    BracketThing { target: BNode, mode: IterMode, value: BNode },
    BraceThing { target: BNode, mode: IterMode },
    Replace { target: BNode, mode: ReplaceMode, pairs: Vec<(ReplaceValue, Option<ReplaceValue>)> },
    CharReplace { target: BNode, mode: ReplaceMode, pairs: Vec<(char, Option<char>)> },
    Print { values: ONode, mode: PrintMode },
    Input { prompt: ONode, mode: InputMode },
    IncrementBef { target: BNode, mode: IncrementMode },
    IncrementAft { target: BNode, mode: IncrementMode },
    Group(BNode), // might not be needed after dealing with precedence and stuff
    List(VNode),
    Variable(String),
    Keyword(Keyword),
    String(Vec<ParsedFragment>),
    Integer(u32),
    Float(f64),
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Statements(_) => "block".into(),
            Self::Return(_) => "return statement".into(),
            Self::Break(_) => "break statement".into(),
            Self::Continue(_) => "continue statement".into(),
            Self::Exit(_) => "exit statement".into(),
            Self::Assign { .. } | Self::MultipleAssign { .. } => "assignment".into(),
            Self::If { .. } => "if expression".into(),
            Self::For { .. } => "for loop".into(),
            Self::While { .. } => "while loop".into(),
            Self::Loop { .. } => "loop".into(),
            Self::FnDef { .. } => "function definition".into(),
            Self::BefOp { .. } | Self::BinOp { .. } | Self::AftOp { .. } => "expression".into(),
            Self::FnCall { .. } => "function call".into(),
            Self::Index { .. } => "index".into(),
            Self::BracketThing { .. } => "bracket thing".into(),
            Self::Slice { .. } => "slice".into(),
            Self::BraceThing { .. } => "brace thing".into(),
            Self::Replace { .. } | Self::CharReplace { .. } => "replace expression".into(),
            Self::Print { .. } => "print".into(),
            Self::Input { .. } => "input".into(),
            Self::IncrementBef { mode, .. } | Self::IncrementAft { mode, .. } => match mode {
                IncrementMode::Add => "incrementation".into(),
                IncrementMode::Sub => "decrementation".into(),
            },
            Self::Group(_) => "group".into(),
            Self::List(_) => "list".into(),
            Self::Variable(var) => match var.as_str() {
                "&" => "global variable".into(),
                "~" => "loop variable".into(),
                _ => format!("variable '{var}'")
            },
            Self::Keyword(kw) => format!("keyword '{}'", kw.to_string()),
            Self::String(_) => "string".into(),
            Self::Integer(_) => "number".into(),
            Self::Float(_) => "float".into(),
        })
    }
}

macro_rules! enum_modes {
    (
        $name:ident
        $( $var:ident: $sym:literal, )*
    ) => {
        #[derive(Debug)]
        pub enum $name {
            $( $var, )*
            Default
        }
        
        impl $name {
            pub fn from_token(t: &Token) -> Self {
                if let TokenType::Symbol(sym) = &t.value {
                    match sym.as_str() {
                        $( $sym => return Self::$var, )*
                        _ => ()
                    }
                }
                Self::Default
            }
        }
    }
}

enum_modes! {
    IterMode

    // returns value
    Sum: "+",
    Product: "*",
    All: "&",
    AllBool: "&&",
    Any: "|",
    AnyBool: "||",
    AllEqual: "=",
    AllUnequal: "^",
    Min: ".",
    Max: "`",
    MostFreq: "#",

    // returns list
    Map: "~",
    Unique: "/",
    Print: ";",
    SortAsc: "<",
    SortDesc: ">",
    Filter: "-",
}

enum_modes! {
    IndexMode

    Count: "$",
    Contains: "?",
    NotContains: "!",
    IndexOf: "@",
}

#[derive(Debug)]
pub enum PrintMode {
    Normal,
    Spaces,
    NoNewline,
}

#[derive(Debug)]
pub enum InputMode {
    String,
    Number,
    NumberList,
}

#[derive(Debug)]
pub enum IncrementMode {
    Add,
    Sub,
}
  
macro_rules! keywords {
    ( $($n:ident),* ) => {
        #[derive(Debug)]
        pub enum Keyword {
            $( $n, )*
        }

        impl Keyword {
            pub fn from_str(s: &str) -> Option<Self> {
                $(
                    if s == stringify!($n).to_ascii_lowercase() {
                        Some(Self::$n)
                    } else
                )* { None }
            }
        }

        impl ToString for Keyword {
            fn to_string(&self) -> String {
                match self {
                    $( Self::$n => stringify!($n).to_ascii_lowercase(), )*
                }
            }
        }
    }
}

keywords! { True, False, Null }

#[derive(Debug)]
pub struct Function {
    pub args: Vec<ArgDef>,
    pub block: Node,
}

#[derive(Debug)]
pub struct ArgDef {
    pub name: String,
    pub default: ONode,
    pub pos: Pos,
}

#[derive(Debug)]
pub enum ParsedFragment {
    Expr(VNode),
    Literal(String),
}