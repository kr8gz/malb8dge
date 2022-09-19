use std::{fs, process, fmt::Display};

use ariadne::{Fmt, Color};

use crate::{constants::{self, *}, errors::*};

/// Example:
/// ```
/// fmt_plural!("Remove the extra value{}", 1) // Remove the extra value
/// fmt_plural!("Remove the extra value{}", 5) // Remove the extra values
/// ```
macro_rules! fmt_plural {
    ( $s:literal, $n:expr ) => {
        &format!($s, if $n == 1 { "" } else { "s" })
    }
}

/// Example:
/// ```
/// fmt_num_plural!("Found {} value{}", 1) // Found 1 value
/// fmt_num_plural!("Found {} value{}", 5) // Found 5 values
/// ```
macro_rules! fmt_num_plural {
    ( $s:literal, $n:expr ) => {
        &format!($s, $n, if $n == 1 { "" } else { "s" })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub value: TokenType,
    pub macro_data: Option<MacroData>,
    pub pos: Pos,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Replace(ReplaceData),
    CharReplace(CharReplaceData),
    String(Vec<Fragment>),
    Integer(u32),
    Float(f64),
    Symbol(String),
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Identifier(_) => "identifier".into(),
            Self::Replace(_) | Self::CharReplace(_) => "replace expression".into(),
            Self::String(_) => "string".into(),
            Self::Integer(n) => format!("'{n}'"),
            Self::Float(n) => format!("'{n}'"),
            Self::Symbol(sym) => format!("'{sym}'"),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroData {

}

#[derive(Clone, Debug, PartialEq)]
pub struct ReplaceData {
    pub left: Vec<Vec<Fragment>>,
    pub right: Vec<Vec<Fragment>>,
    pub mode: ReplaceMode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CharReplaceData {
    pub left: String,
    pub right: String,
    pub mode: ReplaceMode,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReplaceMode {
    Normal,
    Swap,
    First,
    Last,
}

impl ReplaceMode {
    fn from_char(c: char) -> Self {
        match c {
            '|' => Self::Swap,
            '!' => Self::First,
            '@' => Self::Last,
            _ => Self::Normal,
        }
    }
}

#[derive(PartialEq)]
enum ReplaceLexStep {
    CharMode,
    LeftPattern,
    RightPattern,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Fragment {
    // value, pos offset
    Expr(String, usize),
    Literal(String),
}

#[derive(Debug)]
pub struct Lexer {
    pub file: String,
    pub chars: Vec<char>,

    pub tokens: Vec<Token>,

    actual_pos: usize,
    token_start: usize,
    char_index: usize,

    // true = parse what would be floating point numbers as <int>, ".", <int>
    floating_point_override: bool,
}

impl Lexer {
    pub fn from_file(file: String) -> Self {
        let code = fs::read_to_string(&file).unwrap_or_else(|err| {
            eprintln!("{} {err}", "Error:".fg(Color::Red));
            process::exit(1);
        });

        Self::from_str(file, code, 0)
    }

    pub fn from_str(file: String, code: String, offset: usize) -> Self {
        let mut lexer = Self {
            file,
            chars: code.chars().collect(),

            char_index: 0,
            token_start: 0,
            actual_pos: offset,

            floating_point_override: false,

            tokens: Vec::new(),
        };

        lexer.lex();
        lexer
    }

    fn error(&self, msg: &str) -> Error {
        Error::new(&self.file, msg)
    }
    
    fn next(&mut self) -> Option<char> {
        self.char_index += 1;
        self.actual_pos += 1;
        self.chars.get(self.char_index - 1).copied()
    }

    fn prev(&mut self) {
        if self.char_index > 0 {
            self.char_index -= 1;
            self.actual_pos -= 1;
        }
    }
    
    fn lex(&mut self) {
        while let Some(next) = self.next() {
            self.token_start = self.actual_pos - 1;

            if next.is_alphabetic() {
                self.lex_identifier(next);
            }

            else if next == '\\' {
                self.lex_replace();
            }

            else if next == '"' {
                self.lex_string();
            }

            else if next.is_ascii_digit() {
                self.lex_number(next);
            }

            else if !" \r\t".contains(next) {
                self.lex_symbol(next);
            }
        }
    }

    fn push(&mut self, value: TokenType) {
        self.tokens.push(Token {
            value,
            macro_data: None,
            pos: self.token_start..self.actual_pos,
        });
    }

    fn lex_identifier(&mut self, first: char) {
        let mut identifier = String::from(first);

        while let Some(next) = self.next() {
            if next.is_alphabetic() || (!identifier.is_empty() && next.is_ascii_digit()) {
                identifier.push(next);
            } else {
                break
            }
        }
    
        self.prev();
        self.push(TokenType::Identifier(identifier));
    }
    
    fn lex_replace(&mut self) {
        let mut frag = String::new();
        let mut fragments = Vec::new();
        let mut pattern = Vec::new();
        let mut left_pattern = Vec::new();
        let mut replace_mode = ReplaceMode::Normal;

        let mut frag_start = self.token_start;

        let mut step = ReplaceLexStep::CharMode;
        let mut is_escape = false;
        let mut brace_depth = 0;

        let mut separator_pos = 0;

        while let Some(next) = self.next() {
            if step == ReplaceLexStep::CharMode {
                step = ReplaceLexStep::LeftPattern;
                if next == '\\' {
                    self.lex_char_replace();
                    return;
                }
            }

            if is_escape {
                // invalid escape
                if !"`\\|!@{},".contains(next) { frag.push('`'); }
                frag.push(next);
                is_escape = false;
            }

            else if next == '`' && brace_depth == 0 {
                is_escape = true;
            }

            else if step == ReplaceLexStep::RightPattern && next == '\\' && brace_depth == 0 {
                if !frag.is_empty() {
                    fragments.push(Fragment::Literal(frag));
                }
                
                if !fragments.is_empty() {
                    pattern.push(fragments);
                }

                if replace_mode == ReplaceMode::Swap && pattern.len() != left_pattern.len() {
                    self.error("Different number of swap pattern values")
                    .label(
                        self.token_start + 1..separator_pos - 1, 
                        fmt_num_plural!("Left side has {} value{}", left_pattern.len())
                    )
                    .label(
                        separator_pos..self.actual_pos - 1,
                        fmt_num_plural!("Right side has {} value{}", pattern.len())
                    )
                    .help("Add or remove some values to balance both sides")
                    .eprint();
                }
                
                else if pattern.len() > left_pattern.len() {
                    self.error("More replace values than find values")
                    .label(
                        self.token_start + 1..separator_pos - 1,
                        fmt_num_plural!("Found {} find value{}", left_pattern.len())
                    )
                    .label(
                        separator_pos..self.actual_pos - 1,
                        fmt_num_plural!("Found {} replace value{}", pattern.len())
                    )
                    .help("Add more find values or remove some replace values")
                    .eprint();
                }

                self.push(TokenType::Replace(ReplaceData {
                    left: left_pattern,
                    right: pattern,
                    mode: replace_mode,
                }));
                return;
            }

            else if next == ',' && brace_depth == 0 {
                fragments.push(Fragment::Literal(frag));
                pattern.push(fragments);

                frag = String::new();
                fragments = Vec::new();
            }

            else if step == ReplaceLexStep::LeftPattern && "\\|!@".contains(next) && brace_depth == 0 {
                step = ReplaceLexStep::RightPattern;
                
                if !frag.is_empty() {
                    fragments.push(Fragment::Literal(frag));
                    frag = String::new();
                }
                if !fragments.is_empty() {
                    pattern.push(fragments);
                    fragments = Vec::new();
                }

                if pattern.is_empty() {
                    self.error("Find pattern cannot be empty")
                    .label(self.token_start..self.actual_pos, "No find pattern provided")
                    .help("Add a find pattern")
                    .eprint();
                }

                left_pattern.append(&mut pattern);

                replace_mode = ReplaceMode::from_char(next);
                separator_pos = self.actual_pos;
            }

            else if next == '{' && !is_escape {
                if brace_depth == 0 {
                    if !frag.is_empty() {
                        fragments.push(Fragment::Literal(frag));
                        frag = String::new();
                    }
                    frag_start = self.actual_pos;
                } else {
                    frag.push('{');
                }

                brace_depth += 1;
            }

            else if next == '}' && !is_escape && brace_depth > 0 {
                brace_depth -= 1;

                if brace_depth == 0 {
                    fragments.push(Fragment::Expr(frag, frag_start));
                    frag = String::new();
                } else {
                    frag.push('}');
                }
            }

            else {
                frag.push(next);
            }
        }

        self.prev();

        let mut err = self.error("Unclosed replace expression")
        .label(self.token_start..self.actual_pos, "This replace expression hasn't been closed")
        .label(self.actual_pos..self.actual_pos, "Expected another '\\' to finish it");

        if self.tokens.iter().any(|t| matches!(t.value, TokenType::Replace(_) | TokenType::CharReplace(_))) {
            err = err.note("Check if you forgot to close an earlier one");
        }

        err.eprint();
    }

    // TODO would be nice to merge this with the lex_replace function
    fn lex_char_replace(&mut self) {
        let mut pattern = String::new();
        let mut left_pattern = String::new();
        let mut replace_mode = ReplaceMode::Normal;
        
        let mut step = ReplaceLexStep::LeftPattern;
        let mut is_escape = false;

        let mut separator_pos = 0; 

        while let Some(next) = self.next() {
            if is_escape {
                // invalid escape
                if !"`\\|!@".contains(next) {
                    pattern.push('`');
                }
                pattern.push(next);
                is_escape = false;
            }

            else if next == '`' {
                is_escape = true;
            }

            else if step == ReplaceLexStep::RightPattern && next == '\\' {
                if replace_mode == ReplaceMode::Swap && pattern.len() != left_pattern.len() {
                    self.error("Different number of swap pattern values")
                    .label(
                        self.token_start + 1..separator_pos - 1, 
                        fmt_num_plural!("Left side has {} value{}", left_pattern.len())
                    )
                    .label(
                        separator_pos..self.actual_pos - 1,
                        fmt_num_plural!("Right side has {} value{}", pattern.len())
                    )
                    .help("Add or remove some values to balance both sides")
                    .eprint();
                }
                
                else if pattern.len() > left_pattern.len() {
                    self.error("More replace values than find values")
                    .label(
                        self.token_start + 1..separator_pos - 1,
                        fmt_num_plural!("Found {} find value{}", left_pattern.len())
                    )
                    .label(
                        separator_pos..self.actual_pos - 1,
                        fmt_num_plural!("Found {} replace value{}", pattern.len())
                    )
                    .help("Add more find values or remove some replace values")
                    .eprint();
                }

                self.push(TokenType::CharReplace(CharReplaceData {
                    left: left_pattern,
                    right: pattern,
                    mode: replace_mode,
                }));
                return;
            }

            else if step == ReplaceLexStep::LeftPattern && "\\|!@".contains(next) {
                step = ReplaceLexStep::RightPattern;

                if pattern.is_empty() {
                    self.error("Find pattern cannot be empty")
                    .label(self.token_start..self.actual_pos, "No find pattern provided")
                    .help("Add a find pattern")
                    .eprint();
                }
                
                left_pattern.push_str(&pattern);
                pattern.clear();

                replace_mode = ReplaceMode::from_char(next);
                separator_pos = self.actual_pos;
            }

            else {
                pattern.push(next);
            }
        }

        self.prev();

        let mut err = self.error("Unclosed replace expression")
        .label(self.token_start..self.actual_pos, "This replace expression hasn't been closed")
        .label(self.actual_pos..self.actual_pos, "Expected another '\\' to finish it");

        if self.tokens.iter().any(|t| matches!(t.value, TokenType::Replace(_) | TokenType::CharReplace(_))) {
            err = err.note("Check if you forgot to close an earlier one");
        }

        err.eprint();
    }
    
    fn lex_string(&mut self) {
        let mut frag = String::new();
        let mut fragments = Vec::new();

        let mut frag_start = self.token_start;
        
        let mut is_escape = false;
        let mut brace_depth = 0;

        while let Some(next) = self.next() {
            if is_escape {
                // lmao
                match next {
                                                'n' =>   frag.push('\n'),
                                                't' =>   frag.push('\t'),
                    _ if "\"\\{}".contains(next) =>   frag.push(next),
                                                _ => { frag.push('\\'); frag.push(next); }
                };
                is_escape = false;
            }

            else if next == '\\' && brace_depth == 0 {
                is_escape = true;
            }

            // end string
            else if next == '"' && brace_depth == 0 {
                fragments.push(Fragment::Literal(frag));
                self.push(TokenType::String(fragments));
                return;
            }

            else if next == '{' && !is_escape {
                if brace_depth == 0 {
                    fragments.push(Fragment::Literal(frag));
                    frag = String::new();
                    frag_start = self.actual_pos;
                } else {
                    frag.push('{');
                }

                brace_depth += 1;
            }

            else if next == '}' && !is_escape && brace_depth > 0 {
                brace_depth -= 1;
                
                if brace_depth == 0 {
                    fragments.push(Fragment::Expr(frag, frag_start));
                    frag = String::new();
                } else {
                    frag.push('}');
                }
            }

            else {
                frag.push(next);
            }
        }

        self.prev();

        let mut err = self.error("Unclosed string")
        .label(self.token_start..self.actual_pos, "This string hasn't been closed")
        .label(self.actual_pos..self.actual_pos, "Expected another '\"' to finish it");

        if self.tokens.iter().any(|t| matches!(t.value, TokenType::String(_))) {
            err = err.note("Check if you forgot to close an earlier string");
        }

        err.eprint();
   }
    
    fn lex_number(&mut self, first: char) {
        let mut number = String::from(first);
        let mut float = false;

        while let Some(next) = self.next() {
            if next == '.' && !self.floating_point_override {
                float = true;
            }

            // end number
            else if !next.is_ascii_digit() { break }

            number.push(next)
        }

        self.prev();

        if number.ends_with('.') {
            self.prev();
            float = false;
            number.pop();
        }

        let points = number.matches('.').count();
        if points > 1 {
            self.error("More than one decimal point in number")
            .label(self.token_start..self.actual_pos, "Found multiple decimal points")
            .help(fmt_plural!("Remove the extra decimal point{}", points - 1))
            .eprint();
        }

        if float {
            self.push(TokenType::Float(number.parse().unwrap()));
        } else {
            self.push(TokenType::Integer(number.parse().unwrap()));
        }
    }
    
    fn lex_symbol(&mut self, first: char) {
        let mut symbol = String::from(first);

        while let Some(next) = self.next() {
            // make sure it's not a space because that splits symbols
            if next != ' ' && (
                // continue the symbol if it's either...
                // - `symbol + next` creates one of the combined symbols
                constants::ALL_SYMBOLS.contains(&format!("{symbol}{next}").as_str())
                // - or `symbol` is a binary operator and `next` is '=' (augmented assignment)
                || constants::BINARY_OPERATORS.contains(&symbol.as_str()) && next == '='
            ) {
                symbol.push(next);
            }

            // comment
            else if format!("{symbol}{next}") == "###" {
                while let Some(next) = self.next() {
                    if next == '\n' { break }
                }
                return;
            }

            // end symbol
            else { break }
        }

        self.floating_point_override = symbol == ".";

        self.prev();
        self.push(TokenType::Symbol(symbol));
    }
}
