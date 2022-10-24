use std::fmt;

use ariadne::{Fmt, Color::*};

use crate::{parse::ast::*, util::{*, errors::*, operators::{self, *, OpType::*}}, lex::{lexer::*, tokens::*}, run::types::ValueType};

#[derive(Debug)]
pub struct Parser {
    pub eof: usize,
    pub tokens: Vec<Token>,

    pub statements: Vec<Node>,
    pub functions: Vec<Function>,

    token_index: usize,

    stop: Vec<&'static str>,
    ignore_nl: bool,

    in_func: bool,
    in_loop: bool,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            eof: lexer.tokens.last().map(|t| t.pos.end).unwrap_or(0),
            tokens: lexer.tokens,

            statements: Vec::new(),
            functions: Vec::new(),

            token_index: 0,

            stop: Vec::new(),
            ignore_nl: false,

            in_func: false,
            in_loop: false,
        }
    }

    // ------------------------------- error helper methods -------------------------------
    fn expected(&self, pos: Pos, expected: impl fmt::Display, found: impl fmt::Display) -> Error {
        Error::err("Syntax error")
            .label(pos, format!("Expected {}, found {}", expected.fg(Green), found.fg(Red)))
    }

    fn expected_bracket(&mut self, open_pos: Pos, bracket: &str, has_value: bool) -> Result<()> {
        let next = self.next();
        if !next.value.is(bracket) && !next.value.eof() {
            return Err(
                Error::err("Syntax error")
                    .label(open_pos, "Opening bracket here")
                    .label(next.pos, format!(
                        "Expected {} or '{}', found {}",
                        if has_value { "value separator" } else { "value" }.fg(Green),
                        bracket.fg(Green),
                        next.value.fg(Red),
                    ))
            )
        }
        Ok(())
    }

    fn statement_sep(&mut self) -> Result<()> {
        let next = self.next();
        if !next.value.is("\n") && !next.value.is(";") && !next.value.eof() {
            return Err(
                self.expected(next.pos, "statement terminator".fg(Green), next.value.fg(Red))
            )
        }
        Ok(())
    }

    fn check_vars(&self, mut node: Node, allow_list: bool, allow_index: bool, found: &str, expected: &str) -> Result<Node> {
        node.data = match node.data {
            NodeType::Group(group) => self.check_vars(*group, allow_list, allow_index, found, expected)?.data,

            NodeType::List(list) if allow_list => NodeType::List(
                list.into_iter()
                    .map(|el| self.check_vars(el, false, allow_index, found, expected))
                    .collect::<Result<_>>()?
            ),

            var @ NodeType::Variable(_) => var,
            idx @ NodeType::Index { .. } if allow_index => idx,
            
            _ => return Err(
                Error::err("Syntax error")
                    .label(self.curr().pos, format!("Found #{found}# here"))
                    .label(node.pos, format!("Expected {}, found {}", expected.fg(Green), node.data.fg(Red)))
            )
        };
        Ok(node)
    }

    fn check_var_list(&self, nodes: Vec<Node>, allow_index: bool, found: &str, expected: &str) -> Result<Vec<Node>> {
        let mut vars = Vec::new();
        for node in nodes {
            let node = self.check_vars(node, true, allow_index, found, expected)?;
            match node.data {
                NodeType::List(mut list) => vars.append(&mut list),
                NodeType::Variable(_) | NodeType::Index { .. } => vars.push(node),
                _ => unreachable!()
            }
        }
        Ok(vars)
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
        self.tokens.get(self.token_index - 1).cloned().unwrap_or(Token {
            value: TokenType::Eof,
            macro_data: None,
            pos: self.eof..self.eof,
        })
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
    pub fn parse(&mut self) -> Result<()> {
        while self.token_index < self.tokens.len() {
            if let Some(statement) = self.parse_statement(true)? {
                self.statement_sep()?;
                self.statements.push(statement);
            }
        }
        Ok(())
    }

    fn parse_atom(&mut self, optional: bool) -> Result<Option<Node>> {
        let Token { value, pos, .. } = self.next();
        
        macro_rules! optional_expected {
            () => {
                if optional {
                    self.prev();
                    return Ok(None)
                } else {
                    return Err(self.expected(pos, "value", value))
                }
            }
        }

        let data = match value {
            TokenType::Number(num) => {
                let peek = self.peek().value;
                if
                    peek.is("(") || peek.is("[") ||
                    peek.is("_") || peek.is("$") ||
                    matches!(peek, TokenType::FragmentString(_) | TokenType::Identifier(_))
                {
                    NodeType::Group(Box::new(Node {
                        data: NodeType::BinOp {
                            a: Box::new(Node {
                                data: NodeType::Literal(ValueType::Number(num)),
                                pos: pos.clone(),
                            }),
                            op: "*".into(),
                            b: Box::new(self.parse_atom(false)?.unwrap()),
                        },
                        pos: pos.start..self.pos_end(),
                    }))
                } else {
                    NodeType::Literal(ValueType::Number(num))
                }
            },

            TokenType::Identifier(id) => {
                match id.as_str() {
                    "true" | "false" => NodeType::Literal(ValueType::Boolean(id.parse().unwrap())),
                    "null" => NodeType::Literal(ValueType::Null()),
                    _ => NodeType::Variable(id),
                }
            },

            TokenType::FragmentString(st) => NodeType::FragmentString(
                st.into_iter()
                    .map(|f| self.parse_fragment(f))
                    .collect::<Result<_>>()?
            ),

            TokenType::String(st) => NodeType::Literal(ValueType::String(st)),

            TokenType::Symbol(ref sym) => match sym.as_str() {
                "(" => {
                    let expr = self.reset_stops(|s| {
                        s.ignoring_nl(|s| {
                            s.parse_expression(true)
                        })
                    })?;
                    self.expected_bracket(pos.clone(), ")", expr.is_some())?;
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
                    })?;
                    self.expected_bracket(pos.clone(), "]", !list.is_empty())?;
                    NodeType::List(list)
                },

                "{" => return Ok(Some(
                    self.reset_stops(|s| s.parse_statements(pos.start, "}"))?
                )),

                "?" => {
                    let mut mode = IterMode::from_token(&self.next());
                    // mode only allowed if there is a { } block
                    if !self.peek().value.is("{") || matches!(mode, IterMode::Default) {
                        self.prev();
                        mode = IterMode::Default;
                    }

                    NodeType::Loop {
                        mode,
                        block: Box::new(self.with_loop(|s| s.parse_statement(false))?.unwrap()),
                    }
                },

                ";" | "/" | "|" | "/|" => NodeType::Print {
                    mode: PrintMode::from_token(&self.curr()),
                    value: Box::new({
                        let start = self.peek().pos.start;
                        let mut list = self.parse_list(None)?;

                        Node {
                            pos: start..self.pos_end(),
                            data: {
                                if list.is_empty() {
                                    NodeType::Literal(ValueType::Null())
                                } else if list.len() > 1 || matches!(list[0].data, NodeType::List(_)) {
                                    NodeType::List(list)
                                } else {
                                    list.swap_remove(0).data
                                }
                            },
                        }
                    }),
                },

                "_" | "$" | "#$" =>  NodeType::Input {
                    mode: InputMode::from_token(&self.curr()),
                    prompt: Box::new(None),
                },

                "++" | "--" => {
                    let (verb, mode) = match sym.as_str() {
                        "++" => ("increment", IncrMode::AddBef),
                        "--" => ("decrement", IncrMode::SubBef),
                        _ => unreachable!()
                    };

                    NodeType::Increment {
                        mode,
                        target: {
                            let val = self.parse_value(false)?.unwrap();
                            if !matches!(val.data, NodeType::Variable(_) | NodeType::Index { .. } ) {
                                return Err(
                                    Error::err("Syntax error")
                                        .label(pos, format!("Found #{verb} operator# here"))
                                        .label(val.pos, format!("Expected {}, found {}", "variable".fg(Green), val.data.fg(Red)))
                                        .note(format!("Only variables can be {verb}ed"))
                                )
                            }
                            Box::new(val)
                        }
                    }
                },

                ":" => self.parse_fn_def(Vec::new())?,

                "&" | "~" => NodeType::Variable(sym.into()),

                op if operators::is_op(Before, op) => NodeType::UnaryOp {
                    op_type: Before,
                    op: op.into(),
                    target: Box::new(self.parse_operation(operators::op_prec(Before, op), false)?.unwrap()),
                },

                _ => optional_expected!()
            }

            _ => optional_expected!()
        };

        Ok(Some(Node {
            data,
            pos: pos.start..self.pos_end(),
        }))
    }

    fn parse_expression(&mut self, optional: bool) -> Result<Option<Node>> {
        let mut expr = match self.parse_operation(1, optional)? {
            Some(node) => node,
            None => return Ok(None)
        };
        let expr_start = expr.pos.start;

        if let Token { value: TokenType::Symbol(ref sym), .. } = self.next() {
            let data = match sym.as_str() {
                "," if !self.stop.contains(&",") => self.parse_maybe_list(Some(expr))?,

                "~" => self.parse_for_while_loop(expr)?,

                "?" => self.parse_if(expr, false)?,

                "!" if !self.stop.contains(&"!") => self.parse_if(expr, true)?,

                "=" if !self.stop.contains(&"=") => {
                    let target = self.check_vars(expr, true, true, "assignment", "target variable(s)")?;

                    if let NodeType::List(targets) = target.data {
                        NodeType::MultipleAssign {
                            targets,
                            value: Box::new(self.parse_expression(false)?.unwrap()),
                        }
                    } else {
                        NodeType::Assign {
                            target: Box::new(target),
                            op: String::new(),
                            value: Box::new(self.parse_expression(false)?.unwrap()),
                        }
                    }
                },

                op if operators::is_op(Binary, &op[..op.len() - 1]) && op.ends_with('=') => NodeType::Assign {
                    target: Box::new(self.check_vars(expr, false, true, "augmented assignment", "target variable")?),
                    op: op[..op.len() - 1].into(),
                    value: Box::new(self.parse_expression(false)?.unwrap()),
                },

                _ => { self.prev(); return Ok(Some(expr)) }
            };

            expr = Node {
                data,
                pos: expr_start..self.pos_end(),
            }
        } else {
            self.prev();
        }
        
        Ok(Some(expr))
    }

    fn parse_fn_def(&mut self, args: Vec<Node>) -> Result<NodeType> {
        let args = self.check_var_list(args, false, "function definition", "argument name")?;
        let block = self.with_func(|s| s.parse_statement(false))?.unwrap();
        self.functions.push(Function { args, block });
        Ok(NodeType::Function {
            index: self.functions.len() - 1,
        })
    }

    fn parse_for_while_loop(&mut self, expr: Node) -> Result<NodeType> {
        let mut mode = IterMode::from_token(&self.next());
        if let IterMode::Default = mode { self.prev(); }
        
        let next = self.next();

        if next.value.is("[") {
            return Ok(NodeType::While {
                cond: Box::new(expr),
                mode,
                block: Box::new(self.with_loop(|s| {
                    s.reset_stops(|s| {
                        s.parse_statements(next.pos.start, "]")
                    })
                })?),
            })
        } else if next.value.is("?") {
            return Ok(NodeType::While {
                cond: Box::new(expr),
                mode,
                block: Box::new(self.with_loop(|s| s.parse_statement(false))?.unwrap()),
            })
        }

        self.prev();
        let mut vars = Vec::new();
        
        if let IterMode::Default = mode {
            if !next.value.is("{") {
                macro_rules! parse_ident {
                    () => {
                        let var = self.parse_atom(false)?.unwrap();
                        if !matches!(var.data, NodeType::Variable(_)) {
                            return Err(self.expected(var.pos, "variable", var.data));
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
                            return Err(self.expected(next.pos, "mode specifier, comma, or block", next.value));
                        }
                    } else {
                        break
                    }
                }
            }
        }

        Ok(NodeType::For {
            iter: Box::new(expr),
            vars,
            mode,
            block: Box::new(self.with_loop(|s| s.parse_statement(false))?.unwrap()),
        })
    }
    
    fn parse_fragment(&mut self, frag: LexedFragment) -> Result<ParsedFragment> {
        Ok(match frag {
            LexedFragment::Literal(lit) => ParsedFragment::Literal(lit),
            LexedFragment::Expr(lexer) => {
                let pos = lexer.actual_pos - lexer.char_index..lexer.actual_pos;
                let mut parser = Self::new(lexer);
                parser.parse()?;
                ParsedFragment::Expr(Node {
                    data: NodeType::Statements(parser.statements),
                    pos
                })
            }
        })
    }

    fn parse_if(&mut self, cond: Node, invert: bool) -> Result<NodeType> {
        let mut on_true = Some(self.with_stop("!", |s| s.parse_statement(false))?.unwrap());
        let mut on_false = None;
        
        if self.next().value.is("!") {
            on_false = Some(self.parse_statement(false)?.unwrap());
        } else {
            self.prev();
        }
        
        if invert {
            (on_true, on_false) = (on_false, on_true);
        }

        Ok(NodeType::If {
            cond: Box::new(cond),
            on_true: Box::new(on_true),
            on_false: Box::new(on_false),
        })
    }

    fn parse_list(&mut self, first: Option<Node>) -> Result<Vec<Node>> {
        const STOPS: &[&str] = &[","];

        let mut list: Vec<_> = first.into_iter().collect();

        match self.with_stops(STOPS, |s| s.parse_expression(true))? {
            Some(expr) => list.push(expr),
            None => return Ok(list)
        }

        loop {
            if self.next().value.is(",") {
                list.push(self.with_stops(STOPS, |s| s.parse_expression(false))?.unwrap());
            } else {
                self.prev();
                break
            }
        }
        Ok(list)
    }

    fn parse_maybe_list(&mut self, first: Option<Node>) -> Result<NodeType> {
        const STOPS: &[&str] = &[",", "=", ":"];

        let mut list: Vec<_> = first.into_iter().collect();

        match self.with_stops(STOPS, |s| s.parse_expression(true))? {
            Some(expr) => list.push(expr),
            None => return Ok(NodeType::List(list))
        }

        loop {
            let next = self.next().value;

            if next.is(",") {
                list.push(self.with_stops(STOPS, |s| s.parse_expression(false))?.unwrap());
            }
            
            else if next.is("=") {
                return Ok(NodeType::MultipleAssign {
                    targets: self.check_var_list(list, true, "assignment", "target variable")?,
                    value: Box::new(self.parse_expression(false)?.unwrap()),
                })
            }
            
            else if next.is(":") {
                return self.parse_fn_def(list)
            }
            
            else {
                self.prev();
                break
            }
        }
        Ok(NodeType::List(list))
    }

    fn parse_operation(&mut self, prec: usize, optional: bool) -> Result<Option<Node>> {
        let mut next_prec = operators::MAX_PREC.min(prec + 1);
        while next_prec != operators::MAX_PREC && operators::prec_type(next_prec) == PrecType::Before {
            next_prec += 1;
        }

        macro_rules! parse_operand {
            () => {
                if prec == operators::MAX_PREC {
                    self.parse_value(optional)?
                } else {
                    self.parse_operation(next_prec, optional)?
                }
            }
        }

        let mut a = match parse_operand!() {
            Some(node) => node,
            None => return Ok(None)
        }; 
        let prec_type = operators::prec_type(prec);
        let op_type = operators::op_type(prec_type);

        if prec_type == PrecType::Compare {
            let mut chain = Vec::new();
            
            while let TokenType::Symbol(op) = self.next().value {
                if operators::op_prec(op_type, &op) != prec { break }
                
                let b = match parse_operand!() {
                    Some(node) => node,
                    None => break
                };
                
                chain.push((op, Box::new(b)));
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
        } else if prec_type != PrecType::Before {
            while let TokenType::Symbol(op) = self.next().value {
                if operators::op_prec(op_type, &op) != prec { break }
                
                let b = if prec_type == PrecType::RightAssoc {
                    self.parse_operation(prec, optional)?
                } else {
                    parse_operand!()
                };

                let b = match b {
                    Some(node) => node,
                    None => break
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
        Ok(Some(a))
    }

    fn parse_statement(&mut self, optional: bool) -> Result<Option<Node>> {
        let Token { value, pos, .. } = self.next();

        match value {
            TokenType::Symbol(sym) if matches!(sym.as_str(), "<" | "%" | ">" | "%%") => {
                let expr = self.parse_expression(true)?;
                let pos = pos.start..match expr {
                    Some(ref expr) => expr.pos.end,
                    None => pos.end,
                };

                macro_rules! check_scope {
                    ( $stmt:ident, $cond:expr, $scope:literal ) => {
                        if $cond {
                            NodeType::$stmt(Box::new(expr))
                        } else {
                            return Err(
                                Error::err("Syntax error")
                                    .label(pos, format!("{} statement not in {}", stringify!($stmt), $scope))
                            )
                        }
                    }
                }

                Ok(Some(Node {
                    data: match sym.as_str() {
                        "<" => check_scope!(Return, self.in_func, "function"),
                        "%" => check_scope!(Break, self.in_loop, "loop"),
                        ">" => check_scope!(Continue, self.in_loop, "loop"),
                        "%%" => NodeType::Exit(Box::new(expr)),
                        _ => unreachable!()
                    },
                    pos,
                }))
            },

            _ if value.is("\n") || value.eof() => {
                if optional {
                    Ok(None)
                } else {
                    Err(self.expected(pos, "statement", value))
                }
            },

            _ => {
                self.prev();
                self.parse_expression(false)
            }
        }
    }

    fn parse_statements(&mut self, start_pos: usize, end_sym: &str) -> Result<Node> {
        let mut statements = Vec::new();

        macro_rules! check_end {
            () => {
                let next = self.next().value;
                if next.is(end_sym) || next.eof() {
                    return Ok(Node {
                        data: NodeType::Statements(statements),
                        pos: start_pos..self.pos_end(),
                    })
                } else {
                    self.prev();
                }
            }
        }

        loop {
            check_end!();
            if let Some(statement) = self.parse_statement(true)? {
                statements.push(statement);
                check_end!();
                self.statement_sep()?;
            }
        }
    }

    fn parse_value(&mut self, optional: bool) -> Result<Option<Node>> {
        let mut parsed_value = match self.parse_atom(optional)? {
            Some(node) => node,
            None => return Ok(None)
        };
        let value_start = parsed_value.pos.start;

        loop {
            let Token { value, pos, .. } = self.next();
            
            let data = match value {
                TokenType::Replace(rd) => {
                    // please help me
                    macro_rules! map_frags {
                        ( $v:expr ) => {
                            $v.into_iter().map(
                                |v| v.into_iter().map(
                                    |f| self.parse_fragment(f)
                                ).collect::<Result<_>>()
                            ).collect::<Result<_>>()?
                        }
                    }

                    NodeType::Replace {
                        target: Box::new(parsed_value),
                        mode: rd.mode,
                        left: map_frags!(rd.left),
                        right: map_frags!(rd.right),
                    }
                },

                TokenType::CharReplace(rd) => NodeType::CharReplace {
                    target: Box::new(parsed_value),
                    mode: rd.mode,
                    left: rd.left,
                    right: rd.right,
                },

                TokenType::Symbol(ref sym) => match sym.as_str() {
                    "(" => if let NodeType::Input { mut prompt, mode } = parsed_value.data {
                        if prompt.is_some() {
                            return Err(
                                Error::err("Syntax error")
                                    .label(parsed_value.pos, "Found input with prompt here")
                                    .label(pos, "Tried to specify another prompt")
                                    .help("Remove the extra prompt")
                            )
                        }

                        let mut expr = self.reset_stops(|s| {
                            s.ignoring_nl(|s| {
                                s.parse_expression(false)
                            })
                        })?.unwrap();
                        self.expected_bracket(pos.clone(), ")", true)?;

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
                                })?;
                                self.expected_bracket(pos.clone(), ")", !args.is_empty())?;
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
                                                index: Box::new(values[0].take().ok_or_else(
                                                    || self.expected(token.pos, "value", token.value)
                                                )?),
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
                                        })?;
                                    }

                                    else {
                                        self.prev();
                                        self.expected_bracket(pos.clone(), "]", true)?;
                                    }

                                    if value_pos > 2 {
                                        return Err(
                                            self.expected(token.pos, "]", token.value)
                                                .note("A maximum of 3 values may be specified in a slice (start, stop, step)")
                                        )
                                    }

                                    can_add_expr = token.value.is(",");
                                }
                            } else {
                                NodeType::BracketIndex {
                                    target: Box::new(parsed_value),
                                    mode: index_mode,
                                    value: Box::new({
                                        let expr = self.reset_stops(|s| {
                                            s.ignoring_nl(|s| {
                                                s.parse_expression(false)
                                            })
                                        })?.unwrap();
                                        self.expected_bracket(pos.clone(), "]", true)?;
                                        expr
                                    }),
                                }
                            }
                        } else {
                            NodeType::BracketIter {
                                target: Box::new(parsed_value),
                                mode: iter_mode,
                                expr: {
                                    let expr = Box::new(self.reset_stops(|s| s.parse_expression(false))?.unwrap());
                                    self.expected_bracket(pos.clone(), "]", true)?;
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
                                return Err(self.expected(next.pos, "mode specifier", next.value))
                            }

                            mode
                        },
                    },

                    "." => NodeType::Index {
                        target: Box::new(parsed_value),
                        index: Box::new(self.parse_atom(false)?.unwrap()),
                    },

                    ":" if !self.stop.contains(&":") => self.parse_fn_def(vec![parsed_value])?,

                    "++" | "--" => {
                        let (verb, mode) = match sym.as_str() {
                            "++" => ("increment", IncrMode::AddAft),
                            "--" => ("decrement", IncrMode::SubAft),
                            _ => unreachable!()
                        };
    
                        NodeType::Increment {
                            mode,
                            target: {
                                if !matches!(parsed_value.data, NodeType::Variable(_) | NodeType::Index { .. } ) {
                                    return Err(
                                        Error::err("Syntax error")
                                            .label(pos, format!("Found #{verb} operator# here"))
                                            .label(parsed_value.pos, format!("Expected {}, found {}", "variable".fg(Green), parsed_value.data.fg(Red)))
                                            .note(format!("Only variables can be {verb}ed"))
                                    )
                                }
                                Box::new(parsed_value)
                            }
                        }
                    },

                    op if operators::is_op(After, op) => NodeType::UnaryOp {
                        op_type: After,
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
        Ok(Some(parsed_value))
    }
}
