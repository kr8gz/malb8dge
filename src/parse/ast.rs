use std::fmt::Display;

use crate::{lex::tokens::*, util::*};

type BNode = Box<Node>;
type ONode = Box<Option<Node>>;
type VNode = Vec<Node>;

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
    Assign { target: BNode, op: String, value: BNode }, // non-empty op = augmented assignment
    MultipleAssign { targets: VNode, value: BNode },
    If { cond: BNode, on_true: ONode, on_false: ONode },
    For { iter: BNode, vars: VNode, mode: IterMode, block: BNode }, // x ~ [vars]  mode    ... // x ~ [vars]   [mode] { ... } //
    While { cond: BNode, mode: IterMode, block: BNode },            // x ~        [mode] ? ... // x ~          [mode] [ ... ] //
    Loop { mode: IterMode, block: BNode },                          //                   ? ... //            ? [mode] { ... } //
    Function { args: VNode, block: BNode },
    BeforeOp { target: BNode, op: usize }, // op: usize = op id
    AfterOp { target: BNode, op: usize }, // op: usize = op id
    BinOp { a: BNode, op: usize, b: BNode },
    Compare { first: BNode, chain: Vec<(String, BNode)> },
    Increment { target: BNode, mode: IncrMode },
    FnCall { target: BNode, args: VNode },
    Index { target: BNode, index: BNode },
    Slice { target: BNode, start: ONode, stop: ONode, step: ONode },
    BracketIndex { target: BNode, mode: IndexMode, value: BNode },
    BracketIter { target: BNode, mode: IterMode, expr: BNode },
    BraceIter { target: BNode, mode: IterMode },
    Replace { target: BNode, mode: ReplaceMode, left: Vec<Vec<ParsedFragment>>, right: Vec<Vec<ParsedFragment>> },
    CharReplace { target: BNode, mode: ReplaceMode, left: Vec<char>, right: Vec<char> },
    Print { value: BNode, mode: PrintMode },
    Input { prompt: ONode, mode: InputMode },
    Group(BNode),
    List(VNode),
    Variable(String),
    String(Vec<ParsedFragment>),
    Number(f64),
    Boolean(bool),
    Null,
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
            Self::Function { .. } => "function definition".into(),
            Self::BeforeOp { .. } | Self::AfterOp { .. } | Self::BinOp { .. } => "expression".into(),
            Self::Increment { .. } => "incrementation".into(),
            Self::Compare { .. } => "comparison".into(),
            Self::FnCall { .. } => "function call".into(),
            Self::Index { .. } => "index".into(),
            Self::BracketIndex { .. } => "bracket indexing thing".into(),
            Self::BracketIter { .. } => "bracket iterating thing".into(),
            Self::Slice { .. } => "slice".into(),
            Self::BraceIter { .. } => "brace iterating thing".into(),
            Self::Replace { .. } | Self::CharReplace { .. } => "replace expression".into(),
            Self::Print { .. } => "print".into(),
            Self::Input { .. } => "input".into(),
            Self::Group(_) => "group".into(),
            Self::List(_) => "list".into(),
            Self::Variable(var) => match var.as_str() {
                "&" => "global variable".into(),
                "~" => "loop variable".into(),
                _ => format!("variable '{var}'")
            },
            Self::String(_) => "string".into(),
            Self::Number(_) => "number".into(),
            Self::Boolean(b) => format!("keyword '{b}'"),
            Self::Null => "keyword 'null'".into(),
        })
    }
}

#[derive(Debug)]
pub enum IncrMode {
    AddBef,
    AddAft,
    SubBef,
    SubAft,
}

impl IncrMode {
    pub fn add(&self) -> bool { matches!(self, Self::AddBef | Self::AddAft) }
    // pub fn sub(&self) -> bool { matches!(self, Self::SubBef | Self::SubAft) }
    pub fn bef(&self) -> bool { matches!(self, Self::AddBef | Self::SubBef) }
    pub fn aft(&self) -> bool { matches!(self, Self::AddAft | Self::SubAft) }
}

macro_rules! enum_modes {
    (
        $name:ident
        $( $var:ident: $sym:literal, )*
    ) => {
        #[derive(Clone, Debug, PartialEq)]
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

enum_modes! {
    PrintMode

    Spaces: "/",
    NoNewline: "|",
    NoNewlineSpaces: "/|",
    // Default: ";"
}

enum_modes! {
    InputMode

    Number: "$",
    NumberList: "#$",
    // Default: "_"
}

#[derive(Debug)]
pub enum ParsedFragment {
    Expr(VNode),
    Literal(String),
}
