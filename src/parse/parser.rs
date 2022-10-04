use std::{iter, fmt};

use crate::{parse::ast::*, util::{self, *, OpType::*}, lex::{lexer::*, tokens::*}, errors::*};

#[derive(Debug)]
pub struct Parser {
    eof: Token,

    pub file: String,
    pub tokens: Vec<Token>,

    pub statements: Vec<Node>,

    token_index: usize,

    stop: Vec<&'static str>,
    ignore_nl: bool,

    in_func: bool,
    in_loop: bool,
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

            token_index: 0,

            stop: Vec::new(),
            ignore_nl: false,

            in_func: false,
            in_loop: false,
        };

        parser.parse();
        parser
    }

    // ------------------------------- error helper methods -------------------------------
    fn error(&self, msg: &str) -> Error {
        Error::new(self.file.clone(), msg)
    }

    fn expected<T: fmt::Display>(&self, pos: Pos, expected: &str, found: T) -> Error {
        self.error("Syntax error")
            .label(pos, format!("Expected {}, found {}", expected, found))
    }

    fn expected_bracket(&mut self, open_pos: Pos, bracket: &str, has_value: bool) {
        let next = self.next();
        if !next.value.is(bracket) && !next.value.eof() {
            self.error("Syntax error")
                .label(open_pos, "Opening bracket here")
                .label(next.pos, format!(
                    "Expected {} or '{bracket}', found {}",
                    if has_value { "value separator" } else { "value" },
                    next.value
                ))
                .eprint();
        }
    }

    fn statement_sep(&mut self) {
        let next = self.next();
        if !next.value.is("\n") && !next.value.is(";") && !next.value.eof() {
            self.expected(next.pos, "statement terminator", next.value).eprint()
        }
    }

    fn check_vars(&self, mut node: Node, allow_list: bool, found: &str, expected: &str) -> Node {
        node.data = match node.data {
            NodeType::Group(group) => self.check_vars(*group, allow_list, found, expected).data,
            NodeType::List(list) if allow_list => NodeType::List(list.into_iter().map(|el| self.check_vars(el, false, found, expected)).collect()),
            ind @ NodeType::Index { .. } => ind,
            NodeType::Variable(var) if var != "&" && var != "~" => NodeType::Variable(var),
            _ => {
                self.error("Syntax error")
                    .label(self.curr().pos, format!("Found {found} here"))
                    .label(node.pos, format!("Expected {expected}, found {}", node.data))
                    .eprint();
            }
        };
        node
    }

    fn check_var_list(&self, nodes: Vec<Node>, found: &str, expected: &str) -> Vec<Node> {
        let mut vars = Vec::new();
        for node in nodes {
            let node = self.check_vars(node, true, found, expected);
            match node.data {
                NodeType::List(mut list) => vars.append(&mut list),
                NodeType::Variable(_) => vars.push(node),
                _ => unreachable!()
            }
        }
        vars
    }

    // ------------------------------- token iterator methods -------------------------------
    fn next(&mut self) -> Token {
        self.token_index += 1;
        while self.ignore_nl && self.curr().value.is("\n") {
            self.token_index += 1;
        }
        self.curr()
    }

    fn peek(&mut self) -> Token {
        let prev = self.token_index;
        self.token_index += 1;
        while self.ignore_nl && self.curr().value.is("\n") {
            self.token_index += 1;
        }
        let token = self.curr();
        self.token_index = prev;
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

    // ------------------------------- "bracketmanager" methods -------------------------------
    fn with_stop<T, F: Fn(&mut Self) -> T>(&mut self, stop: &'static str, f: F) -> T {
        self.with_stops(&[stop], f)
    }

    fn with_stops<T, F: Fn(&mut Self) -> T>(&mut self, stops: &[&'static str], f: F) -> T {
        self.stop.extend(stops);
        let ret = f(self);
        self.stop.truncate(self.stop.len() - stops.len());
        ret
    }

    fn reset_stops<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> T {
        let mut prev = Vec::new();
        prev.append(&mut self.stop);
        let ret = f(self);
        self.stop = prev;
        ret
    }

    fn ignoring_nl<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> T {
        let prev = self.ignore_nl;
        self.ignore_nl = true;
        let ret = f(self);
        self.ignore_nl = prev;
        ret
    }

    fn with_func<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> T {
        let prev = (self.in_func, self.in_loop);
        self.in_func = true;
        self.in_loop = false;
        let ret = f(self);
        (self.in_func, self.in_loop) = prev;
        ret
    }

    fn with_loop<T, F: Fn(&mut Self) -> T>(&mut self, f: F) -> T {
        let prev = self.in_loop;
        self.in_loop = true;
        let ret = f(self);
        self.in_loop = prev;
        ret
    }

    // ------------------------------- parsing methods -------------------------------
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
                    self.expected(pos, "value", value).eprint();
                }
            }
        }

        let data = match value {
            TokenType::Integer(int) => {
                let peek = self.peek().value;
                if
                    peek.is("(") || peek.is("[") || peek.is("{") ||
                    peek.is("_") || peek.is("$") ||
                    matches!(peek, TokenType::String(_) | TokenType::Identifier(_))
                {
                    NodeType::Group(Box::new(Node {
                        data: NodeType::BinOp {
                            a: Box::new(Node {
                                data: NodeType::Integer(int),
                                pos: pos.clone(),
                            }),
                            op: "*".into(),
                            b: Box::new(self.parse_atom(false).unwrap()),
                        },
                        pos: pos.start..self.pos_end(),
                    }))
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
                    let expr = self.reset_stops(|s| {
                        s.ignoring_nl(|s| {
                            s.parse_expression(true)
                        })
                    });
                    self.expected_bracket(pos.clone(), ")", expr.is_some());
                    match expr {
                        Some(expr) => NodeType::Group(Box::new(expr)),
                        None => NodeType::List(Vec::new()),
                    }
                },

                "[" => {
                    let list = self.reset_stops(|s| {
                        s.ignoring_nl(|s| {
                            s.parse_list(None)
                        })
                    });
                    self.expected_bracket(pos.clone(), "]", !list.is_empty());
                    NodeType::List(list)
                },

                "{" => return Some(self.reset_stops(|s| s.parse_statements(pos.start, "}"))),

                "?" => {
                    let mut mode = IterMode::from_token(&self.next());
                    // mode only allowed if there is a { } block
                    if !self.peek().value.is("{") || matches!(mode, IterMode::Default) {
                        self.prev();
                        mode = IterMode::Default;
                    }

                    NodeType::Loop {
                        mode,
                        block: Box::new(self.with_loop(|s| s.parse_statement(false).unwrap())),
                    }
                },

                ";" | "/" | "|" => NodeType::Print {
                    value: Box::new({
                        let start = self.peek().pos.start;
                        let mut list = self.parse_list(None);

                        if list.is_empty() {
                            None
                        } else {
                            Some(Node {
                                data: if list.len() > 1 || matches!(list[0].data, NodeType::List(_)) {
                                    NodeType::List(list)
                                } else {
                                    list.remove(0).data
                                },
                                pos: start..self.pos_end(),
                            })
                        }
                    }),
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
                            if !matches!(val.data, NodeType::Variable(_) | NodeType::Index { .. } ) {
                                self.error("Syntax error")
                                    .label(pos, format!("Found {verb} operator here"))
                                    .label(val.pos, format!("Expected variable, found {}", val.data))
                                    .note(format!("Only variables can be {verb}ed"))
                                    .eprint()
                            }
                            Box::new(val)
                        }
                    }
                },

                ":" => self.parse_fn_def(Vec::new()),

                "&" | "~" => NodeType::Variable(sym.into()),

                op if util::is_op(Before, op) => NodeType::BefOp {
                    op: op.into(),
                    target: Box::new(self.parse_operation(util::op_prec(Before, op), false).unwrap()),
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
        let mut expr = self.parse_operation(1, optional)?;
        let expr_start = expr.pos.start;

        if let Token { value: TokenType::Symbol(ref sym), .. } = self.next() {
            let data = match sym.as_str() {
                "," if !self.stop.contains(&",") => self.parse_maybe_list(Some(expr)),

                "~" => self.parse_for_while_loop(expr),

                "?" => self.parse_if(expr, false),

                "!" if !self.stop.contains(&"!") => self.parse_if(expr, true),

                "=" if !self.stop.contains(&"=") => NodeType::Assign {
                    target: Box::new(self.check_vars(expr, true, "assignment", "target variable(s)")),
                    op: String::new(),
                    value: Box::new(self.parse_expression(false).unwrap()),
                },

                op if util::is_bin_op(&op[..op.len() - 1]) && op.ends_with('=') => NodeType::Assign {
                    target: Box::new(self.check_vars(expr, false, "augmented assignment", "target variable")),
                    op: String::new(),
                    value: Box::new(self.parse_expression(false).unwrap()),
                },

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
        NodeType::Function {
            args: self.check_var_list(args, "function definition", "argument name"),
            block: Box::new(self.with_func(|s| s.parse_statement(false).unwrap())),
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
                block: Box::new(self.with_loop(|s| {
                    s.reset_stops(|s| {
                        s.parse_statements(next.pos.start, "]")
                    })
                })),
            }
        } else if next.value.is("?") {
            return NodeType::While {
                cond: Box::new(expr),
                mode,
                block: Box::new(self.with_loop(|s| s.parse_statement(false).unwrap())),
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
                            self.expected(var.pos, "variable", var.data).eprint();
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
                            self.expected(next.pos, "mode specifier, comma, or block", next.value).eprint();
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
            block: Box::new(self.with_loop(|s| s.parse_statement(false).unwrap())),
        }
    }
    
    fn parse_fragment(&mut self, frag: LexedFragment) -> ParsedFragment {
        match frag {
            LexedFragment::Literal(lit) => ParsedFragment::Literal(lit),
            LexedFragment::Expr(expr) => ParsedFragment::Expr(Self::new(expr).statements)
        }
    }

    fn parse_if(&mut self, cond: Node, invert: bool) -> NodeType {
        let mut on_true = Some(self.with_stop("!", |s| s.parse_statement(false).unwrap()));
        let mut on_false = None;
        
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
        const STOPS: &[&str] = &[","];

        let mut list: Vec<_> = first.into_iter().collect();

        match self.with_stops(STOPS, |s| s.parse_expression(true)) {
            Some(expr) => list.push(expr),
            None => return list
        }

        loop {
            if self.next().value.is(",") {
                list.push(self.with_stops(STOPS, |s| s.parse_expression(false).unwrap()));
            } else {
                self.prev();
                break
            }
        }

        list
    }

    fn parse_maybe_list(&mut self, first: Option<Node>) -> NodeType {
        const STOPS: &[&str] = &[",", "=", ":"];

        let mut list: Vec<_> = first.into_iter().collect();

        match self.with_stops(STOPS, |s| s.parse_expression(true)) {
            Some(expr) => list.push(expr),
            None => return NodeType::List(list)
        }

        loop {
            let next = self.next().value;

            if next.is(",") {
                list.push(self.with_stops(STOPS, |s| s.parse_expression(false).unwrap()));
            }
            
            else if next.is("=") {
                return NodeType::MultipleAssign {
                    targets: self.check_var_list(list, "assignment", "target variable"),
                    value: Box::new(self.parse_expression(false).unwrap()),
                }
            }
            
            else if next.is(":") {
                return self.parse_fn_def(list)
            }
            
            else {
                self.prev();
                break
            }
        }

        NodeType::List(list)
    }

    fn parse_operation(&mut self, prec: usize, optional: bool) -> Option<Node> {
        let mut next_prec = util::MAX_PREC.min(prec + 1);
        while next_prec != util::MAX_PREC && !util::is_bin_type(util::prec_type(next_prec)) {
            next_prec += 1;
        }

        macro_rules! parse_operand {
            () => {
                if prec == util::MAX_PREC {
                    self.parse_value(optional)?
                } else {
                    self.parse_operation(next_prec, optional)?
                }
            }
        }

        let mut a = parse_operand!();
        let op_type = util::prec_type(prec);

        if op_type == Compare {
            let mut chain = Vec::new();

            while let TokenType::Symbol(op) = self.next().value {
                if util::op_prec(op_type, &op) != prec { break }
                chain.push((op, Box::new(parse_operand!())));
            }

            if !chain.is_empty() {
                a = Node {
                    pos: a.pos.start..self.pos_end(),
                    data: NodeType::Compare {
                        first: Box::new(a),
                        chain
                    },
                };
            }
            self.prev();
        }

        else if util::is_bin_type(op_type) {
            while let TokenType::Symbol(op) = self.next().value {
                if util::op_prec(op_type, &op) != prec { break }
                
                let b = if op_type == RightAssoc {
                    self.parse_operation(prec, optional)?
                } else {
                    parse_operand!()
                };

                a = Node {
                    pos: a.pos.start..self.pos_end(),
                    data: NodeType::BinOp {
                        a: Box::new(a),
                        op,
                        b: Box::new(b),
                    },
                };
            }
            self.prev();
        }

        Some(a)
    }

    fn parse_statement(&mut self, optional: bool) -> Option<Node> {
        let Token { value, pos, .. } = self.next();

        match value {
            TokenType::Symbol(sym) if matches!(sym.as_str(), "<" | "%" | ">" | "%%") => {
                let expr = self.parse_expression(true);
                let pos = pos.start..match expr {
                    Some(ref expr) => expr.pos.end,
                    None => pos.end,
                };

                macro_rules! check_scope {
                    ( $stmt:ident, $cond:expr, $scope:literal ) => {
                        if $cond {
                            NodeType::$stmt(Box::new(expr))
                        } else {
                            self.error("Syntax error")
                                .label(pos, format!("{} statement not in {}", stringify!($stmt), $scope))
                                .eprint()
                        }
                    }
                }

                Some(Node {
                    data: match sym.as_str() {
                        "<" => check_scope!(Return, self.in_func, "function"),
                        "%" => check_scope!(Break, self.in_loop, "loop"),
                        ">" => check_scope!(Continue, self.in_loop, "loop"),
                        "%%" => NodeType::Exit(Box::new(expr)),
                        _ => unreachable!()
                    },
                    pos,
                })
            },

            _ if value.is("\n") || value.eof() => {
                if optional {
                    None
                } else {
                    self.expected(pos, "statement", value).eprint();
                }
            },

            _ => {
                self.prev();
                self.parse_expression(false)
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
                    pairs: zip_longer(rd.left, rd.right),
                },

                TokenType::Symbol(ref sym) => match sym.as_str() {
                    "(" => if let NodeType::Input { mut prompt, mode } = parsed_value.data {
                        if prompt.is_some() {
                            self.error("Syntax error")
                                .label(parsed_value.pos, "Found input with prompt here")
                                .label(pos, "Tried to specify another prompt")
                                .help("Remove the extra prompt")
                                .eprint();
                        }

                        let mut expr = self.reset_stops(|s| {
                            s.ignoring_nl(|s| {
                                s.parse_expression(false).unwrap()
                            })
                        });
                        self.expected_bracket(pos.clone(), ")", true);

                        expr.pos = pos.start..self.pos_end();
                        parsed_value.pos.end = expr.pos.end;
                        prompt = Box::new(Some(expr));
                        NodeType::Input { prompt, mode }
                    } else {
                        NodeType::FnCall {
                            target: Box::new(parsed_value),
                            args: {
                                let args = self.reset_stops(|s| {
                                    s.ignoring_nl(|s| {
                                        s.parse_list(None)
                                    })
                                });
                                self.expected_bracket(pos.clone(), ")", !args.is_empty());
                                args
                            },
                        }
                    },

                    "[" => {
                        let next = self.next();

                        let iter_mode = IterMode::from_token(&next);
                        if let IterMode::Default = iter_mode {
                            let index_mode = IndexMode::from_token(&next);
                            if let IndexMode::Default = index_mode {
                                self.prev();

                                let mut values = [None, None, None];
                                let mut value_pos = 0;
                                let mut is_slice = false;
                                let mut can_add_expr = true;

                                loop {
                                    let token = self.next();

                                    if token.value.is("]") {
                                        break if is_slice {
                                            NodeType::Slice {
                                                target: Box::new(parsed_value),
                                                start: Box::new(values[0].take()),
                                                stop: Box::new(values[1].take()),
                                                step: Box::new(values[2].take()),
                                            }
                                        } else {
                                            NodeType::Index {
                                                target: Box::new(parsed_value),
                                                index: Box::new(values[0].take().unwrap_or_else(
                                                    || self.expected(token.pos, "value", token.value).eprint()
                                                )),
                                            }
                                        }
                                    }

                                    else if token.value.is(",") {
                                        is_slice = true;
                                        value_pos += 1;
                                    }

                                    else if can_add_expr {
                                        self.prev();
                                        values[value_pos] = self.reset_stops(|s| {
                                            s.ignoring_nl(|s| {
                                                s.with_stop(",", |s| {
                                                    s.parse_expression(true)
                                                })
                                            })
                                        });
                                    }

                                    else {
                                        self.prev();
                                        self.expected_bracket(pos.clone(), "]", true);
                                    }

                                    if value_pos > 2 {
                                        self.expected(token.pos, "]", token.value)
                                            .note("A maximum of 3 values may be specified in a slice (start, stop, step)")
                                            .eprint();
                                    }

                                    can_add_expr = token.value.is(",");
                                }
                            } else {
                                NodeType::BracketIndex {
                                    target: Box::new(parsed_value),
                                    mode: index_mode,
                                    value: Box::new(self.reset_stops(|s| {
                                        s.ignoring_nl(|s| {
                                            let expr = s.parse_expression(false).unwrap();
                                            s.expected_bracket(pos.clone(), "]", true);
                                            expr
                                        })
                                    })),
                                }
                            }
                        } else {
                            NodeType::BracketIter {
                                target: Box::new(parsed_value),
                                mode: iter_mode,
                                expr: {
                                    let expr = Box::new(self.reset_stops(|s| s.parse_expression(false).unwrap()));
                                    self.expected_bracket(pos.clone(), "]", true);
                                    expr
                                },
                            }
                        }
                    },

                    "{" => NodeType::BraceIter {
                        target: Box::new(parsed_value),
                        mode: {
                            let next = self.next();
                            let mode = IterMode::from_token(&next);

                            if let IterMode::Default = mode {
                                self.expected(next.pos, "mode specifier", next.value).eprint()
                            }

                            mode
                        },
                    },

                    "." => NodeType::Index {
                        target: Box::new(parsed_value),
                        index: Box::new(self.parse_atom(false).unwrap()),
                    },

                    ":" if !self.stop.contains(&":") => self.parse_fn_def(vec![parsed_value]),

                    "++" | "--" => {
                        let (mode, verb) = match sym.as_str() {
                            "++" => (IncrementMode::Add, "increment"),
                            "--" => (IncrementMode::Sub, "decrement"),
                            _ => unreachable!()
                        };

                        NodeType::IncrementAft {
                            mode,
                            target: {
                                if !matches!(parsed_value.data, NodeType::Variable(_) | NodeType::Index { .. } ) {
                                    self.error("Syntax error")
                                        .label(pos, format!("Found {verb} operator here"))
                                        .label(parsed_value.pos, format!("Expected variable, found {}", parsed_value.data))
                                        .note(format!("Only variables can be {verb}ed"))
                                        .eprint()
                                }
                                Box::new(parsed_value)
                            }
                        }
                    },

                    op if util::is_op(After, op) => NodeType::AftOp {
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

fn zip_longer<T>(longer: Vec<T>, shorter: Vec<T>) -> ZipLonger<T> {
    longer
        .into_iter()
        .zip(shorter.into_iter().map(Some).chain(iter::repeat_with(|| None)))
        .collect()
}
