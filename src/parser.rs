use std::iter;

use crate::{ast::*, constants::*, lexer::*, errors::Error};

#[derive(Debug)]
pub struct Parser {
    pub file: String,
    pub tokens: Vec<Token>,

    pub statements: Vec<Node>,
    pub functions: Vec<Function>,

    token_index: usize,
    eof: Pos,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let eof = lexer.tokens.last().map(|t| t.pos.end).unwrap_or(0);

        let mut parser = Self {
            eof: eof..eof,

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

    fn expected(&mut self, expected: &str) {
        if let Some(token) = self.next() {
            if !token.is(expected) {
                self.error("Syntax error")
                .label(token.pos, &format!("Expected '{}', found {}", expected, token.value))
                .eprint();
            }
        }
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

    fn pos_end(&self) -> usize {
        self.tokens.get(self.token_index - 1).cloned().map(|t| t.pos.end).unwrap_or(self.eof.end)
    }

    fn parse(&mut self) {
        while self.token_index < self.tokens.len() {
            if let Some(statement) = self.parse_statement() {
                self.statements.push(statement);
            }
        }
    }

    fn parse_atom(&mut self, optional: bool) -> Option<Node> {
        macro_rules! optional_expected {
            ( $pos:expr, $expected:expr, $found:expr ) => {
                if !optional {
                    self.error("Syntax error")
                    .label($pos, &format!("Expected {}, found {}", $expected, $found))
                    .eprint();
                } else {
                    self.prev();
                    return None
                }
            }
        }

        match self.next() {
            Some(Token { value, pos, .. }) => {
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
                        "(" => {
                            let expr = self.parse_expression(true);
                            self.expected(")");
                            match expr {
                                Some(expr) => NodeType::Group(Box::new(expr)),
                                None             => NodeType::List(Vec::new()),
                            }
                        },

                        "[" => {
                            let list = self.parse_list(None);
                            self.expected("]");
                            NodeType::List(list)
                        },

                        "{" => {
                            self.prev();
                            return Some(self.parse_block())
                        },

                        "?" => todo!("infinite loop"),

                        ";" | "/" | "|" => NodeType::Print {
                            values: Box::new(self.parse_expression(true)),
                            mode: match sym.as_str() {
                                ";" => PrintMode::Normal,
                                "/" => PrintMode::Spaces,
                                "|" => PrintMode::NoNewline,
                                _ => unreachable!()
                            },
                        },

                        "_" | "$" | "#$" =>  NodeType::Input {
                            prompt: Box::new(None),
                            mode: match sym.as_str() {
                                "_" => InputMode::String,
                                "$" => InputMode::Number,
                                "#$" => InputMode::NumberList,
                                _ => unreachable!()
                            },
                        },

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

                        ":" => self.parse_fn_def(Vec::new()),

                        "&" | "~" => NodeType::Variable(sym.into()),

                        op if BEFORE_OPERATORS.contains(&op) => NodeType::BefOp {
                            op: op.into(),
                            target: Box::new(self.parse_value(false).unwrap()),
                        },

                        _ => optional_expected!(pos, "value", value)
                    }

                    _ => optional_expected!(pos, "value", value)
                };

                Some(Node {
                    data,
                    pos: pos.start..self.pos_end(),
                })
            },

            None => optional_expected!(self.eof.clone(), "value", "end of file"),
        }
    }

    fn parse_block(&mut self) -> Node {
        // TODO
        Node {
            data: NodeType::Statements(Vec::new()),
            pos: self.pos_end()..self.pos_end(),
        }
    }

    fn parse_expression(&mut self, optional: bool) -> Option<Node> {
        let mut expr = self.parse_value(optional)?;
        let expr_start = expr.pos.start;

        if let Some(Token { value: ref value @ TokenType::Symbol(ref sym), pos, .. }) = self.next() {
            let data = match sym.as_str() {
                "," => NodeType::List(self.parse_list(Some(expr))),

                "~" => todo!("for or while loop"),

                "?" | "!" => self.parse_if(expr, sym == "!"),

                "=" => todo!("normal assignment"),

                op if BINARY_OPERATORS.contains(&&op[..op.len() - 1]) && op.ends_with('=') => todo!("augmented assignment"),

                op if BINARY_OPERATORS.contains(&op) => todo!("bin op"),

                op if COMPARE_OPERATORS.contains(&op) => todo!("cmp"),

                _ => { self.prev(); return Some(expr) }
            };

            expr = Node {
                data,
                pos: expr_start..self.pos_end(),
            }
        }
        
        // TODO error stuff // YO WHAT IF you dont have to close brackets at the end of the file
        Some(expr)
    }

    fn parse_fn_def(&mut self, args: Vec<Node>) -> NodeType {
        let f = Function {
            args: Vec::new(), // TODO map Vec<Node> to Vec<ArgDef>
            block: self.parse_block(),
        };
        self.functions.push(f);

        NodeType::FnDef {
            index: self.functions.len() - 1,
        }
    }
    
    fn parse_fragment(&mut self, frag: LexedFragment) -> ParsedFragment {
        match frag {
            LexedFragment::Literal(lit) => ParsedFragment::Literal(lit),
            LexedFragment::Expr(expr) => ParsedFragment::Expr(Self::new(expr).statements)
        }
    }

    fn parse_if(&mut self, cond: Node, invert: bool) -> NodeType {
        let mut on_true = Some(self.parse_block());
        let mut on_false = None;

        if let Some(token) = self.next() {
            if token.is("!") {
                on_false = Some(self.parse_block());
            } else {
                self.prev();
            }
        }
        
        if invert {
            (on_true, on_false) = (on_false, on_true);
        }

        NodeType::If {
            cond: Box::new(cond),
            on_true: Box::new(on_true),
            on_false: Box::new(on_false),
        }
    }

    fn parse_list(&mut self, first: Option<Node>) -> Vec<Node> {
        let mut list = first.into_iter().collect();
        
        // TODO
        // this list can also act as function arg definitions or multiple assignment targets
        // different unpack behavior

        list
    }

    fn parse_statement(&mut self) -> Option<Node> {
        let mut statement = None;
        if let Some(Token { value: TokenType::Symbol(symbol), pos, .. }) = self.next() {
            match symbol.as_str() {
                "<" | "%" | ">" | "%%" => {
                    let expr = self.parse_expression(true);
                    statement = Some(Node {
                        pos: pos.start..match expr {
                            Some(ref expr) => expr.pos.end,
                            None => pos.end,
                        },
                        data: match symbol.as_str() {
                            "<" => NodeType::Return, // TODO (?) no return outside function
                            "%" => NodeType::Break, // TODO (?) no break outside loop
                            ">" => NodeType::Continue, // TODO (?) no continue outside loop
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
            statement = self.parse_expression(false);
        }

        if let Some(token) = self.next() {
            if !token.is("\n") && !token.is(";") {
                self.error("Syntax error")
                .label(token.pos, &format!("Expected statement separator, found {}", token.value))
                .eprint();
            }
        }

        statement
    }

    fn parse_value(&mut self, optional: bool) -> Option<Node> {
        let mut parsed_value = self.parse_atom(optional)?;
        let value_start = parsed_value.pos.start;

        while let Some(Token { value, pos, .. }) = self.next() {
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
                        index: Box::new(self.parse_atom(false).unwrap()),
                    },

                    ":" => self.parse_fn_def(vec![parsed_value]),

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
                pos: value_start..self.pos_end(),
            }
        }

        Some(parsed_value)
    }
}

fn zip_longer<T>(longer: Vec<T>, shorter: Vec<T>) -> Vec<(T, Option<T>)> {
    longer
    .into_iter()
    .zip(
        shorter
        .into_iter()
        .map(Some)
        .chain(iter::repeat_with(|| None))
    )
    .collect()
}
