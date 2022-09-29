use std::{iter, fmt};

use crate::{ast::*, constants::*, lexer::*, errors::Error};

#[derive(Debug)]
pub struct Parser {
    eof: Token,

    pub file: String,
    pub tokens: Vec<Token>,

    pub statements: Vec<Node>,
    pub functions: Vec<Function>,

    token_index: usize,

    stop: Option<&'static str>,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let eof_pos = lexer.tokens.last().map(|t| t.pos.end).unwrap_or(0);

        let mut parser = Self {
            eof: Token {
                value: TokenType::Eof,
                macro_data: None,
                pos: eof_pos..eof_pos,
            },

            file: lexer.file,
            tokens: lexer.tokens,

            statements: Vec::new(),
            functions: Vec::new(),

            token_index: 0,

            stop: None,
        };

        parser.parse();
        parser
    }

    fn error(&self, msg: &str) -> Error {
        Error::new(&self.file, msg)
    }

    fn expected<T: fmt::Display>(&mut self, pos: Pos, expected: &str, found: T) -> ! {
        self.error("Syntax error")
        .label(pos, &format!("Expected {}, found {}", expected, found))
        .eprint();
    }

    fn expected_sym(&mut self, sym: &str) {
        let next = self.next();
        if !next.value.is(sym) && !next.value.eof() {
            self.expected(next.pos, &format!("'{sym}'"), next.value);
        }
    }

    fn statement_sep(&mut self) {
        let next = self.next();
        if !next.value.is("\n") && !next.value.is(";") && !next.value.eof() {
            self.expected(next.pos, "statement terminator", next.value)
        }
    }

    fn next(&mut self) -> Token {
        self.token_index += 1;
        self.curr()
    }

    fn peek(&mut self) -> Token {
        self.token_index += 1;
        let token = self.curr();
        self.token_index -= 1;
        token
    }

    fn curr(&self) -> Token {
        self.tokens.get(self.token_index - 1).cloned().unwrap_or_else(|| self.eof.clone())
    }

    fn prev(&mut self) {
        if self.token_index > 0 {
            self.token_index -= 1;
        }
    }

    fn pos_end(&self) -> usize {
        self.curr().pos.end
    }

    fn parse(&mut self) {
        while self.token_index < self.tokens.len() {
            if let Some(statement) = self.parse_statement(true) {
                self.statement_sep();
                self.statements.push(statement);
            }
        }
    }

    fn parse_atom(&mut self, optional: bool) -> Option<Node> {
        let Token { value, pos, .. } = self.next();
        
        macro_rules! optional_expected {
            () => {
                if optional {
                    self.prev();
                    return None
                } else {
                    self.expected(pos, "value", value);
                }
            }
        }

        let data = match value {
            TokenType::Integer(int) => {
                let peek = self.peek().value;
                if peek.is("(") || peek.is("[") || peek.is("{") || matches!(peek, TokenType::String(_) | TokenType::Identifier(_)) {
                    NodeType::BinOp {
                        a: Box::new(Node {
                            data: NodeType::Integer(int),
                            pos: pos.clone(),
                        }),
                        op: "*".into(),
                        b: Box::new(self.parse_atom(false).unwrap()),
                    }
                } else {
                    NodeType::Integer(int)
                }
            },

            TokenType::Float(float) => NodeType::Float(float),

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
                    self.expected_sym(")");

                    match expr {
                        Some(expr) => NodeType::Group(Box::new(expr)),
                        None => NodeType::List(Vec::new()),
                    }
                },

                "[" => {
                    let list = self.parse_list(None);
                    self.expected_sym("]");

                    NodeType::List(list)
                },

                "{" => return Some(self.parse_statements(pos.start, "}")),

                "?" => {
                    let mut mode = IterMode::from_token(&self.next());
                    // mode only allowed if there is a { } block
                    if !self.peek().value.is("{") || matches!(mode, IterMode::Default) {
                        self.prev();
                        mode = IterMode::Default;
                    }

                    NodeType::Loop {
                        mode,
                        block: Box::new(self.parse_statement(false).unwrap()),
                    }
                },

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
                                self.error("Syntax error")
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

                _ => optional_expected!()
            }

            _ => optional_expected!()
        };

        Some(Node {
            data,
            pos: pos.start..self.pos_end(),
        })
    }

    fn parse_expression(&mut self, optional: bool) -> Option<Node> {
        let mut expr = self.parse_value(optional)?;
        let expr_start = expr.pos.start;

        if let Token { value: TokenType::Symbol(ref sym), .. } = self.next() {
            macro_rules! is_stop {
                ( $s:literal ) => {
                    matches!(self.stop, Some(s) if sym == s)
                }
            }

            let data = match sym.as_str() {
                "," => NodeType::List(self.parse_list(Some(expr))),

                "~" => self.parse_for_while_loop(expr),

                "?"                   => self.parse_if(expr, false),
                "!" if !is_stop!("!") => self.parse_if(expr, true),

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
        } else {
            self.prev();
        }
        
        Some(expr)
    }

    fn parse_fn_def(&mut self, args: Vec<Node>) -> NodeType {
        let f = Function {
            args: Vec::new(), // TODO map Vec<Node> to Vec<ArgDef>
            block: self.parse_statement(false).unwrap(),
        };
        self.functions.push(f);

        NodeType::FnDef {
            index: self.functions.len() - 1,
        }
    }

    fn parse_for_while_loop(&mut self, expr: Node) -> NodeType {
        let mut mode = IterMode::from_token(&self.next());
        if let IterMode::Default = mode { self.prev(); }
        
        let next = self.next();

        if next.value.is("[") {
            return NodeType::While {
                cond: Box::new(expr),
                mode,
                block: Box::new(self.parse_statements(next.pos.start, "]")),
            }
        } else if next.value.is("?") {
            return NodeType::While {
                cond: Box::new(expr),
                mode,
                block: Box::new(self.parse_statement(false).unwrap()),
            }
        }

        self.prev();
        let mut vars = Vec::new();
        
        if let IterMode::Default = mode {
            if !next.value.is("{") {
                macro_rules! parse_ident {
                    () => {
                        let var = self.parse_atom(false).unwrap();
                        if !matches!(var.data, NodeType::Variable(_)) {
                            self.expected(var.pos, "variable", var.data);
                        }
                        vars.push(var);
                    }
                }

                parse_ident!();

                loop {
                    let next = self.next();
                    mode = IterMode::from_token(&next);
                    if let IterMode::Default = mode {
                        if next.value.is("{") {
                            self.prev();
                            break
                        } else if next.value.is(",") {
                            parse_ident!();
                        } else {
                            self.expected(next.pos, "mode specifier, comma, or block", next.value);
                        }
                    } else {
                        break
                    }
                }
            }
        }

        NodeType::For {
            iter: Box::new(expr),
            vars,
            mode,
            block: Box::new(self.parse_statement(false).unwrap()),
        }
    }
    
    fn parse_fragment(&mut self, frag: LexedFragment) -> ParsedFragment {
        match frag {
            LexedFragment::Literal(lit) => ParsedFragment::Literal(lit),
            LexedFragment::Expr(expr) => ParsedFragment::Expr(Self::new(expr).statements)
        }
    }

    fn parse_if(&mut self, cond: Node, invert: bool) -> NodeType {
        self.stop.replace("!");
        let mut on_true = Some(self.parse_statement(false).unwrap());
        let mut on_false = None;
        self.stop.take();
        
        if self.next().value.is("!") {
            on_false = Some(self.parse_statement(false).unwrap());
        } else {
            self.prev();
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

    fn parse_statement(&mut self, optional: bool) -> Option<Node> {
        let Token { value, pos, .. } = self.next();

        match value {
            TokenType::Symbol(sym) if matches!(sym.as_str(), "<" | "%" | ">" | "%%") => {
                let expr = self.parse_expression(true);
                Some(Node {
                    pos: pos.start..match expr {
                        Some(ref expr) => expr.pos.end,
                        None => pos.end,
                    },
                    data: match sym.as_str() {
                        "<" => NodeType::Return, // TODO (?) no return outside function
                        "%" => NodeType::Break, // TODO (?) no break outside loop
                        ">" => NodeType::Continue, // TODO (?) no continue outside loop
                        "%%" => NodeType::Exit,
                        _ => unreachable!()
                    } (Box::new(expr)),
                })
            },

            _ if value.is("\n") || value.eof() => {
                if optional {
                    None
                } else {
                    self.expected(pos, "statement", value);
                }
            },

            _ => {
                self.prev();
                self.parse_expression(optional)
            }
        }
    }

    fn parse_statements(&mut self, start_pos: usize, end_sym: &str) -> Node {
        let mut statements = Vec::new();

        macro_rules! check_end {
            () => {
                let next = self.next().value;
                if next.is(end_sym) || next.eof() {
                    return Node {
                        data: NodeType::Statements(statements),
                        pos: start_pos..self.pos_end(),
                    }
                } else {
                    self.prev();
                }
            }
        }

        loop {
            check_end!();
            if let Some(statement) = self.parse_statement(true) {
                statements.push(statement);
                check_end!();
                self.statement_sep();
            }
        }
    }

    fn parse_value(&mut self, optional: bool) -> Option<Node> {
        let mut parsed_value = self.parse_atom(optional)?;
        let value_start = parsed_value.pos.start;

        loop {
            let Token { value, pos, .. } = self.next();
            
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

                    "{" => NodeType::BraceThing {
                        target: Box::new(parsed_value),
                        mode: {
                            let next = self.next();
                            let mode = IterMode::from_token(&next);

                            if let IterMode::Default = mode {
                                self.expected(next.pos, "mode specifier", next.value)
                            }

                            mode
                        },
                    },

                    "." => NodeType::Index {
                        target: Box::new(parsed_value),
                        index: Box::new(self.parse_atom(false).unwrap()),
                        mode: IndexMode::Default,
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
                                    self.error("Syntax error")
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
