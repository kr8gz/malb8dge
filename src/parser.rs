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
    For { iter: BNode, vars: VNode, mode: String, block: BNode },
    While { cond: BNode, mode: String, block: BNode },
    Loop { mode: String, block: BNode },
    FnDef { index: usize }, // see Function struct
    BefOp { target: BNode, op: String },
    BinOp { a: BNode, op: String, b: BNode },
    AftOp { target: BNode, op: String },
    FnCall { target: BNode, args: VNode },
    Index { target: BNode, index: BNode },
    BracketThing { target: BNode, mode: String, value: BNode },
    Slice { target: BNode, start: ONode, stop: ONode, step: ONode },
    BraceThing { target: BNode, mode: String },
    Replace { target: BNode, mode: ReplaceMode, pairs: Vec<(ReplaceValue, ReplaceValue)> },
    CharReplace { target: BNode, mode: ReplaceMode, pairs: Vec<(char, char)> },
    Print { values: ONode, mode: PrintMode },
    Input { prompt: ONode, mode: InputMode },
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
    }
}
keywords! { True, False, Null }

#[derive(Debug)]
pub struct Function {
    args: Vec<ArgDef>,
    block: NodeType,
}

#[derive(Debug)]
pub struct ArgDef {
    name: String,
    default: ONode,
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
    }
    
    fn parse_expression(&mut self, optional: bool) -> Option<Node> {
        let mut expr = self.parse_value(optional);
        
        expr
    }

    fn parse_value(&mut self, optional: bool) -> Option<Node> {
        let mut value = match self.next() {
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

                    TokenType::String(st) => {
                        NodeType::String(st.into_iter().map(|f| {
                            match f {
                                Fragment::Literal(lit) => ParsedFragment::Literal(lit),
                                Fragment::Expr(expr, offset) => ParsedFragment::Expr(
                                    Self::new(Lexer::from_str(self.file.clone(), expr, offset)).statements
                                )
                            }
                        }).collect())
                    },

                    TokenType::Symbol(sym) => match sym.as_str() {
                        "(" => todo!("group"),

                        "[" => todo!("list"),

                        "{" => todo!("block"),

                        "?" => todo!("loop"),

                        ";" | "/" | "|" => {
                            let expr = self.parse_expression(true);
                            value_end = expr.as_ref().map(|e| e.pos.end).unwrap_or(pos.end);

                            NodeType::Print {
                                // TODO no bracket list thingy or something idk
                                values: Box::new(expr),
                                mode: match sym.as_str() {
                                    ";" => PrintMode::Normal,
                                    "/" => PrintMode::Spaces,
                                    "|" => PrintMode::NoNewline,
                                    _ => unreachable!()
                                },
                            }
                        },

                        "_" | "$" | "#$" => todo!("input"),

                        "++" | "--" => todo!("incr bef"),

                        ":" => todo!("0 arg fn"),

                        "&" => NodeType::GlobalVar,
                        "~" => NodeType::LoopVar,

                        op if BEFORE_OPERATORS.contains(&op) => todo!("bef op"),

                        _ => todo!("probably error")
                    }

                    _ => {
                        self.error("Expected value")
                        .label(pos, &format!("Expected value, found {value}"))
                        .eprint();
                    }
                };

                Node {
                    data,
                    pos: pos.start..value_end
                }
            },

            None => {
                if !optional {
                    self.error("Expected value")
                    .label(self.eof..self.eof, "Expected value, found end of file")
                    .eprint();
                } else {
                    return None
                }
            },
        };

        // TODO
        Some(value)
    }
}
