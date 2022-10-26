use std::fmt::Display;

use crate::{lex::tokens::*, util::{*, operators::OpType}, run::types::ValueType};

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
    Assign { target: BNode, value: BNode },
    AugmentedAssign { target: BNode, op: String, value: BNode },
    MultipleAssign { targets: VNode, targets_pos: Pos, value: BNode },
    If { cond: BNode, on_true: ONode, on_false: ONode },
    For { iter: BNode, vars: VNode, mode: IterMode, block: BNode }, // x ~ [vars]  mode    ... // x ~ [vars]   [mode] { ... } //
    While { cond: BNode, mode: IterMode, block: BNode },            // x ~        [mode] ? ... // x ~          [mode] [ ... ] //
    Loop { mode: IterMode, block: BNode },                          //                   ? ... //            ? [mode] { ... } //
    Function { index: usize }, // see Function struct
    UnaryOp { target: BNode, op_type: OpType, op: String },
    BinOp { a: BNode, op: String, b: BNode },
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
    FragmentString(Vec<ParsedFragment>),
    Literal(ValueType),
}

impl Display for NodeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Statements(_) => "block",
            Self::Return(_) => "return statement",
            Self::Break(_) => "break statement",
            Self::Continue(_) => "continue statement",
            Self::Exit(_) => "exit statement",
            Self::Assign { .. } | Self::AugmentedAssign { .. } | Self::MultipleAssign { .. } => "assignment",
            Self::If { .. } => "if expression",
            Self::For { .. } => "for loop",
            Self::While { .. } => "while loop",
            Self::Loop { .. } => "loop",
            Self::Function { .. } => "function definition",
            Self::UnaryOp { .. } | Self::BinOp { .. } => "expression",
            Self::Increment { .. } => "incrementation",
            Self::Compare { .. } => "comparison",
            Self::FnCall { .. } => "function call",
            Self::Index { .. } => "index",
            Self::BracketIndex { .. } => "bracket indexing thing",
            Self::BracketIter { .. } => "bracket iterating thing",
            Self::Slice { .. } => "slice",
            Self::BraceIter { .. } => "brace iterating thing",
            Self::Replace { .. } | Self::CharReplace { .. } => "replace expression",
            Self::Print { .. } => "print",
            Self::Input { .. } => "input",
            Self::Group(_) => "group",
            Self::List(_) => "list",
            Self::Variable(_) => "variable",
            Self::FragmentString(_) => "string",
            Self::Literal(_) => "literal",
        })
    }
}

#[derive(Debug)]
pub struct Function {
    pub args: VNode,
    pub block: Node,
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
    SplitNewline: "/|",
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
    Expr(Node),
    Literal(String),
}
