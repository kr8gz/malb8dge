use std::iter;

use crate::{ast::*, constants::*, lexer::*, errors::Error};

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
        let mut expr = self.parse_value(optional)?;
        let expr_start = expr.pos.start;

        while let Some(Token { value: ref value @ TokenType::Symbol(ref sym), mut pos, .. }) = self.next() {
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
        
        // TODO error stuff
        Some(expr)
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
                                let expr = self.parse_expression(true);
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
                                    let val = self.parse_value(false).unwrap();
                                    value_end = val.pos.end;

                                    if !matches!(val.data, NodeType::Variable(_) ) {
                                        self.error(&format!("Invalid usage of {} operator", verb))
                                        .label(pos, &format!("Can only {} variables", verb))
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
                            // bracketmanager thing to stop parsing if there is a "."
                            let index = self.parse_value(false).unwrap();
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
                                    self.error(&format!("Invalid usage of {} operator", verb))
                                    .label(pos, &format!("Can only {} variables", verb))
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
