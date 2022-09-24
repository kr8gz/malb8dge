use std::iter;

use crate::{ast::*, constants::*, lexer::*, errors::Error};

#[derive(Clone, Debug)]
// any better name?
struct BracketManager {
    is_func: bool,
    is_loop: bool,

    // bracket pairs ("scopes")
    opening_pos: usize,
    closing_sym: String,

    // will only stop current parse function
    separators: Vec<String>,
}

impl BracketManager {
    // BracketManager::init() // initial bm
    // bm.bracket(pos, ")")   // new bracket pair
    // bm.add_sep(".")        // add symbol which will stop next parsing fn
    // bm.add_seps(...)       // add multiple of those
    // bm.clone()             // prevents bm from being moved

    fn init() -> Self {
        Self {
            is_func: false,
            is_loop: false,

            opening_pos: Default::default(),
            closing_sym: Default::default(),

            separators: Vec::new(),
        }
    }

    fn bracket(&self, opening_pos: usize, closing_sym: &str) -> Self {
        Self {
            is_func: self.is_func,
            is_loop: self.is_loop,

            opening_pos,
            closing_sym: closing_sym.into(),

            separators: Vec::new(),
        }
    }

    fn add_sep(&self, sep: &str) -> Self {
        let mut bm = self.clone();
        bm.separators.push(sep.into());
        bm
    }

    fn add_seps(&self, seps: Vec<&str>) -> Self {
        let mut bm = self.clone();
        bm.separators.extend(seps.into_iter().map(|s| s.into()));
        bm
    }

    // TODO how the hell will i get the self.prev() in here
    fn closing(&self, token_value: &TokenType) -> ClosingInstruction {
        use ClosingInstruction::*;

        if let TokenType::Symbol(sym) = token_value {
            if *sym == self.closing_sym {
                if !self.separators.is_empty() {
                    return Prev
                } else {
                    return Close
                }
            }

            else if
                // symbols needed for outer parse functions
                self.separators.contains(sym) ||
                // global level + statement separator
                (self.closing_sym.is_empty() && matches!(sym.as_str(), "\n" | ";"))
            {
                return Prev
            }
        }

        None
    }
}

// TODO fuck this struct
#[derive(Debug)]
enum ClosingInstruction {
    None,
    Close,
    Prev,
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
            if let Some(statement) = self.parse_statement(BracketManager::init()) {
                self.statements.push(statement);
            }
        }
    }

    fn parse_statement(&mut self, bm: BracketManager) -> Option<Node> {
        let mut statement = None;
        if let Some(Token { value: TokenType::Symbol(symbol), pos, .. }) = self.next() {
            match symbol.as_str() {
                "<" | "%" | ">" | "%%" => {
                    let expr = self.parse_expression(bm.clone(), true);
                    statement = Some(Node {
                        pos: pos.start..match expr {
                            Some(ref expr) => expr.pos.end,
                            None => pos.end,
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

        if statement.is_none() {
            self.prev();
            statement = self.parse_expression(bm.clone(), false);
        }

        if let Some(Token { value, pos, .. }) = self.next() {
            if let ClosingInstruction::None = bm.closing(&value) {
                self.error("Expected end of statement")
                .label(pos, &format!("Expected end of statement, found {value}"))
                .eprint();
            }
        }
        self.prev();

        statement
    }
    
    fn parse_expression(&mut self, bm: BracketManager, optional: bool) -> Option<Node> {
        let mut expr = self.parse_value(bm.clone(), optional)?;
        let expr_start = expr.pos.start;

        while let Some(Token { value: ref value @ TokenType::Symbol(ref sym), pos, .. }) = self.next() {
            // TODO bm cloisongg

            let data = match sym.as_str() {
                "," => todo!("funny no parentheses list"),

                "~" => todo!("for or while loop"),

                "?" | "!" => todo!("if expr"),

                "=" => todo!("normal assignment"),

                op if BINARY_OPERATORS.contains(&&op[..op.len() - 1]) && op.ends_with('=') => todo!("augmented assignment"),

                op if BINARY_OPERATORS.contains(&op) => todo!("bin op"),

                op if COMPARE_OPERATORS.contains(&op) => todo!("cmp"),

                _ => {
                    // TODO ???
                    self.error("Invalid expression")
                    .label(pos, &format!("Expected shit, found {value}"))
                    .eprint();
                }
            };

            expr = Node {
                data,
                pos: expr_start..pos.end,
            }
        }
        
        // TODO error stuff // YO WHAT IF you dont have to close brackets at the end of the file
        Some(expr)
    }

    fn parse_value(&mut self, bm: BracketManager, optional: bool) -> Option<Node> {
        macro_rules! expected {
            ( $pos:expr, $found:expr ) => {
                if !optional {
                    self.error("Expected value")
                    .label($pos, &format!("Expected value, found {}", $found))
                    .eprint();
                } else {
                    self.prev();
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
                            None => NodeType::Variable(id),
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
                                let expr = self.parse_expression(bm.clone(), true);
                                value_end = expr.as_ref().map(|e| e.pos.end).unwrap_or(pos.end);
                                Box::new(expr)
                            },
                        },

                        "_" =>  NodeType::Input { prompt: Box::new(None), mode: InputMode::String },
                        "$" =>  NodeType::Input { prompt: Box::new(None), mode: InputMode::Number },
                        "#$" => NodeType::Input { prompt: Box::new(None), mode: InputMode::NumberList },

                        "++" | "--" => {
                            let (mode, verb) = match sym.as_str() {
                                "++" => (IncrementMode::Add, "increment"),
                                "--" => (IncrementMode::Sub, "decrement"),
                                _ => unreachable!()
                            };
    
                            NodeType::IncrementBef {
                                mode,
                                target: {
                                    let val = self.parse_value(bm.clone(), false).unwrap();
                                    value_end = val.pos.end;

                                    if !matches!(val.data, NodeType::Variable(_) ) {
                                        self.error(&format!("Invalid usage of {verb} operator"))
                                        .label(pos, &format!("Can only {verb} variables"))
                                        .label(val.pos, &format!("Expected variable, found {}", val.data))
                                        .eprint();
                                    }
                                    Box::new(val)
                                }
                            }
                        },

                        ":" => todo!("0 arg fn"),

                        "&" | "~" => NodeType::Variable(sym.into()),

                        op if BEFORE_OPERATORS.contains(&op) => NodeType::BefOp {
                            op: op.into(),
                            target: {
                                let val = self.parse_value(bm.clone(), false).unwrap();
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

        let value_start = parsed_value.pos.start;

        while let Some(Token { value, mut pos, .. }) = self.next() {
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

                    "." => NodeType::Index {
                        target: Box::new(parsed_value),
                        index: {
                            let index = self.parse_value(bm.add_sep("."), false).unwrap();
                            pos.end = index.pos.end;
                            Box::new(index)
                        }
                    },

                    ":" => todo!("fn def with args (maybe do this after a list instead or both here and that idk)"),

                    "++" | "--" => {
                        let (mode, verb) = match sym.as_str() {
                            "++" => (IncrementMode::Add, "increment"),
                            "--" => (IncrementMode::Sub, "decrement"),
                            _ => unreachable!()
                        };

                        NodeType::IncrementAft {
                            mode,
                            target: {
                                if !matches!(parsed_value.data, NodeType::Variable(_) ) {
                                    self.error(&format!("Invalid usage of {verb} operator"))
                                    .label(pos, &format!("Can only {verb} variables"))
                                    .label(parsed_value.pos, &format!("Expected variable, found {}", parsed_value.data))
                                    .eprint();
                                }
                                Box::new(parsed_value)
                            }
                        }
                    },

                    op if AFTER_OPERATORS.contains(&op) => NodeType::AftOp {
                        op: op.into(),
                        target: Box::new(parsed_value),
                    },

                    _ => { self.prev(); break },
                }

                _ => { self.prev(); break },
            };

            parsed_value = Node {
                data,
                pos: value_start..pos.end,
            }
        }

        Some(parsed_value)
    }

    fn parse_fragment(&mut self, frag: LexedFragment) -> ParsedFragment {
        match frag {
            LexedFragment::Literal(lit) => ParsedFragment::Literal(lit),
            LexedFragment::Expr(expr) => ParsedFragment::Expr(Self::new(expr).statements)
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
