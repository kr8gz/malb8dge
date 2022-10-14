use std::fmt::Display;

use crate::util::Pos;

use super::lexer::*;

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub value: TokenType,
    pub macro_data: Option<MacroData>,
    pub pos: Pos,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Identifier(String),
    Replace(ReplaceData<Vec<LexedFragment>>),
    CharReplace(ReplaceData<char>),
    String(Vec<LexedFragment>),
    Number(f64),
    Symbol(String),
    Eof,
}

impl TokenType {
    pub fn is(&self, sym: &str) -> bool {
        matches!(self, TokenType::Symbol(ref s) if s == sym)
    }

    pub fn eof(&self) -> bool {
        matches!(self, TokenType::Eof)
    }
}

impl Display for TokenType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Self::Identifier(_) => "identifier".into(),
            Self::Replace(_) | Self::CharReplace(_) => "replace expression".into(),
            Self::String(_) => "string".into(),
            Self::Number(_) => "number".into(),
            Self::Symbol(sym) => match sym.as_str() {
                "\n" => "newline".into(),
                _ => format!("'{sym}'")
            },
            Self::Eof => "end of file".into(),
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MacroData {

}

#[derive(Clone, Debug, PartialEq)]
pub struct ReplaceData<T> {
    pub left: Vec<T>,
    pub right: Vec<T>,
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
    pub fn from_char(c: char) -> Self {
        match c {
            '|' => Self::Swap,
            '!' => Self::First,
            '@' => Self::Last,
            _ => Self::Normal,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexedFragment {
    Expr(Lexer),
    Literal(String),
}
