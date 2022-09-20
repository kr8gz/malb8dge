use std::{fmt::Display, iter};

use crate::{constants::*, lexer::*, errors::Error};

type BNode = Box<Node>;
type ONode = Box<Option<Node>>;
type VNode = Vec<Node>;

type ReplaceValue = Vec<ParsedFragment>;

#[derive(Debug)]
pub struct Node {
    data: NodeType,
    pos: Pos,
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
    If { cond: BNode, on_true: BNode, on_false: BNode },
    For { iter: BNode, vars: VNode, mode: String, block: BNode },   // x ~ [vars]  mode    ... // x ~ [vars]   [mode] { ... } //
    While { cond: BNode, mode: String, block: BNode },              // x ~        [mode] ? ... // x ~          [mode] [ ... ] //
    Loop { mode: String, block: BNode },                            //                   ? ... //            ? [mode] { ... } //
    FnDef { index: usize }, // see Function struct
    BefOp { target: BNode, op: String },
    BinOp { a: BNode, op: String, b: BNode },
    AftOp { target: BNode, op: String },
    FnCall { target: BNode, args: VNode },  
    Index { target: BNode, index: BNode },
    BracketThing { target: BNode, mode: String, value: BNode },
    Slice { target: BNode, start: ONode, stop: ONode, step: ONode },
    BraceThing { target: BNode, mode: String },
    Replace { target: BNode, mode: ReplaceMode, pairs: Vec<(ReplaceValue, Option<ReplaceValue>)> },
    CharReplace { target: BNode, mode: ReplaceMode, pairs: Vec<(char, Option<char>)> },
    Print { values: ONode, mode: PrintMode },
    Input { prompt: ONode, mode: InputMode },
    IncrementBef { target: BNode, mode: IncrementMode },
    IncrementAft { target: BNode, mode: IncrementMode },
    Group(BNode), // might not be needed after dealing with precedence and stuff
    List(VNode),
    GetVar(String),
    Keyword(Keyword),
    GlobalVar,
    LoopVar,
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
            Self::IncrementBef { .. } | Self::IncrementAft { .. } => "incrementation".into(),
            Self::Group(_) => "group".into(),
            Self::List(_) => "list".into(),
            Self::GetVar(var) => format!("variable '{var}'"),
            Self::Keyword(kw) => format!("keyword '{}'", kw.to_string()),
            Self::GlobalVar => "global variable".into(),
            Self::LoopVar => "loop variable".into(),
            Self::String(_) => "string".into(),
            Self::Integer(_) => "number".into(),
            Self::Float(_) => "float".into(),
        })
    }
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
            fn from_str(s: &str) -> Option<Self> {
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
    args: Vec<ArgDef>,
    block: Node,
    pos: Pos,
}

#[derive(Debug)]
pub struct ArgDef {
    name: String,
    default: ONode,
    pos: Pos,
}

#[derive(Debug)]
pub enum ParsedFragment {
    Expr(Vec<Node>),
    Literal(String),
}

#[derive(Debug)]
pub struct Parser {
    pub file: String,
    pub tokens: Vec<Token>,

    pub statements: Vec<Node>,
    pub functions: Vec<Function>,

    token_index: usize,
    eof: usize,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Self {
            eof: lexer.tokens.last().map(|t| t.pos.end).unwrap_or(0),

            file: lexer.file,
            tokens: lexer.tokens,

            statements: Vec::new(),
            functions: Vec::new(),

            token_index: 0,
        };

        parser.parse();
        parser
    }

    fn error(&self, msg: &str) -> Error {
        Error::new(&self.file, msg)
    }

    fn next(&mut self) -> Option<Token> {
        self.token_index += 1;
        self.tokens.get(self.token_index - 1).cloned()
    }

    fn prev(&mut self) {
        if self.token_index > 0 {
            self.token_index -= 1;
        }
    }

    fn parse(&mut self) {
        while self.token_index < self.tokens.len() {
            if let Some(statement) = self.parse_statement() {
                self.statements.push(statement);
            }
        }
    }

    fn parse_statement(&mut self) -> Option<Node> {
        if let Some(Token { value: TokenType::Symbol(symbol), pos: first_pos, .. }) = self.next() {
            match symbol.as_str() {
                "<" | "%" | ">" | "%%" => {
                    let expr = self.parse_expression(true);

                    return Some(Node {
                        pos: first_pos.start..match expr {
                            Some(ref expr) => expr.pos.end,
                            None => first_pos.end,
                        },
                        data: match symbol.as_str() {
                            "<" => NodeType::Return, // TODO no return outside function
                            "%" => NodeType::Break, // TODO no break outside loop
                            ">" => NodeType::Continue, // TODO no continue outside loop
                            "%%" => NodeType::Exit,
                            _ => unreachable!()
                        } (Box::new(expr)),
                    })
                }

                "\n" => return None,
                _ => ()
            }
        }

        self.prev();
        self.parse_expression(false)

        // TODO look for end of statement things (bm.is_closing shit)?
    }
    
    fn parse_expression(&mut self, optional: bool) -> Option<Node> {
        let mut expr = self.parse_value(optional);
        
        expr
    }

    fn parse_value(&mut self, optional: bool) -> Option<Node> {
        macro_rules! expected {
            ( $pos:expr, $found:expr ) => {
                if !optional {
                    self.error("Expected value")
                    .label($pos, &format!("Expected value, found {}", $found))
                    .eprint();
                } else {
                    return None
                }
            }
        }

        let mut parsed_value = match self.next() {
            Some(Token { value, pos, .. }) => {
                let mut value_end = pos.end;
                let data = match value {
                    TokenType::Integer(int) => NodeType::Integer(int), // TODO multiplying thing
                    TokenType::Float(float) => NodeType::Float(float), // TODO for floats too ig

                    TokenType::Identifier(id) => {
                        match Keyword::from_str(&id) {
                            Some(kw) => NodeType::Keyword(kw),
                            None => NodeType::GetVar(id),
                        }
                    },

                    TokenType::String(st) => NodeType::String(
                        st.into_iter().map(|f| self.parse_fragment(f)).collect()
                    ),

                    TokenType::Symbol(ref sym) => match sym.as_str() {
                        "(" => todo!("group"),

                        "[" => todo!("list"),

                        "{" => todo!("block"),

                        "?" => todo!("infinite loop"),

                        ";" | "/" | "|" => NodeType::Print {
                            mode: match sym.as_str() {
                                ";" => PrintMode::Normal,
                                "/" => PrintMode::Spaces,
                                "|" => PrintMode::NoNewline,
                                _ => unreachable!()
                            },

                            // TODO no bracket list thingy or something idk
                            values: {
                                let expr = self.parse_expression(true);
                                value_end = expr.as_ref().map(|e| e.pos.end).unwrap_or(pos.end);
                                Box::new(expr)
                            },
                        },

                        "_" =>  NodeType::Input { prompt: Box::new(None), mode: InputMode::String },
                        "$" =>  NodeType::Input { prompt: Box::new(None), mode: InputMode::Number },
                        "#$" => NodeType::Input { prompt: Box::new(None), mode: InputMode::NumberList },

                        "++" | "--" => NodeType::IncrementBef {
                            mode: match sym.as_str() {
                                "++" => IncrementMode::Add,
                                "--" => IncrementMode::Sub,
                                _ => unreachable!()
                            },

                            target: {
                                let val = self.parse_value(false).unwrap();
                                value_end = val.pos.end;

                                if !matches!(val.data, NodeType::GetVar(_) | NodeType::GlobalVar | NodeType::LoopVar ) {
                                    self.error("Invalid usage of increment operator")
                                    .label(pos, "Can only increment variables")
                                    .label(val.pos, &format!("Expected variable, found {}", val.data))
                                    .eprint();
                                }

                                Box::new(val)
                            },
                        },

                        ":" => todo!("0 arg fn"),

                        "&" => NodeType::GlobalVar,
                        "~" => NodeType::LoopVar,

                        op if BEFORE_OPERATORS.contains(&op) => NodeType::BefOp {
                            op: op.into(),
                            target: {
                                let val = self.parse_value(false).unwrap();
                                value_end = val.pos.end;
                                Box::new(val)
                            },
                        },

                        _ => expected!(pos, value)
                    }

                    _ => expected!(pos, value)
                };

                Node {
                    data,
                    pos: pos.start..value_end,
                }
            },

            None => expected!(self.eof..self.eof, "end of file"),
        };

        while let Some(Token { value, pos, .. }) = self.next() {
             // parsed_value will be moved and re-assigned later but we need the start pos
            let value_start = parsed_value.pos.start;

            let data = match value {
                TokenType::Replace(rd) => NodeType::Replace {
                    target: Box::new(parsed_value),
                    mode: rd.mode,

                    pairs: {
                        // please help me
                        macro_rules! map_frags {
                            ( $v:expr ) => {
                                $v.into_iter().map(
                                    |v| v.into_iter().map(
                                        |f| self.parse_fragment(f)
                                    ).collect()
                                ).collect()
                            }
                        }

                        zip_longer(map_frags!(rd.left), map_frags!(rd.right))
                    }
                },

                TokenType::CharReplace(rd) => NodeType::CharReplace {
                    target: Box::new(parsed_value),
                    mode: rd.mode,
                    pairs: zip_longer(rd.left.chars().collect(), rd.right.chars().collect()),
                },

                TokenType::Symbol(ref sym) => match sym.as_str() {
                    "(" => todo!("fn call"),

                    "[" => todo!("index or weird bracket shit"),

                    "{" => todo!("brace thingyym mmm m"),

                    "." => todo!("simple indexing"),

                    ":" => todo!("fn def with args (maybe do this after a list instead or both here and that idk)"),

                    "++" | "--" => NodeType::IncrementAft {
                        mode: match sym.as_str() {
                            "++" => IncrementMode::Add,
                            "--" => IncrementMode::Sub,
                            _ => unreachable!()
                        },

                        target: {
                            if !matches!(parsed_value.data, NodeType::GetVar(_) | NodeType::GlobalVar | NodeType::LoopVar ) {
                                self.error("Invalid usage of increment operator")
                                .label(pos, "Can only increment variables")
                                .label(parsed_value.pos, &format!("Expected variable, found {}", parsed_value.data))
                                .eprint();
                            }

                            Box::new(parsed_value)
                        },
                    },

                    op if AFTER_OPERATORS.contains(&op) => NodeType::AftOp {
                        op: op.into(),
                        target: Box::new(parsed_value),
                    },

                    _ => todo!("probably stop"),
                }

                _ => todo!("probably stop"),
            };

            parsed_value = Node {
                data,
                pos: value_start..pos.end,
            }
        }

        // TODO
        Some(parsed_value)
    }

    fn parse_fragment(&mut self, frag: Fragment) -> ParsedFragment {
        match frag {
            Fragment::Literal(lit) => ParsedFragment::Literal(lit),
            Fragment::Expr(expr, offset) => ParsedFragment::Expr(
                Self::new(Lexer::from_str(self.file.clone(), expr, offset)).statements
            )
        }
    }
}

fn zip_longer<T>(longer: Vec<T>, shorter: Vec<T>) -> Vec<(T, Option<T>)> {
    longer
    .into_iter()
    .zip(
        shorter
        .into_iter()
        .map(|v| Some(v))
        .chain(iter::repeat_with(|| None))
    )
    .collect()
}
