use std::fs;

use crate::fmt_plural;
use crate::util::{*, errors::{self, Error}, operators::{self, OpType::*}};

use super::tokens::*;

#[derive(PartialEq)]
enum ReplaceLexStep {
    LeftPattern,
    RightPattern,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Lexer {
    pub chars: Vec<char>,

    pub tokens: Vec<Token>,

    pub actual_pos: usize,
    pub char_index: usize,
    token_start: usize,

    // true = parse what would be floating point numbers as <int>, ".", <int>
    floating_point_override: bool,
}

impl Lexer {
    pub fn from_file(file: &str) -> Self {
        let code = fs::read_to_string(file).unwrap_or_else(|err| {
            errors::simple(err.to_string())
        });

        Self::from_str(code, 0)
    }

    pub fn from_str(code: String, offset: usize) -> Self {
        Self {
            chars: code.replace("\r\n", "\n").chars().collect(),

            actual_pos: offset,
            char_index: 0,
            token_start: 0,

            floating_point_override: false,

            tokens: Vec::new(),
        }
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
    
    pub fn lex(&mut self) -> Result<()> {
        while let Some(next) = self.next() {
            self.token_start = self.actual_pos - 1;

            if next.is_alphabetic() {
                self.lex_identifier(next);
            } else if next == '\\' {
                self.lex_replace()?;
            } else if next == '"' {
                self.lex_string()?;
            } else if next.is_ascii_digit() {
                self.lex_number(next)?;
            } else if !" \t".contains(next) {
                self.lex_symbol(next);
            }
        }
        Ok(())
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
            if next.is_alphabetic() || !identifier.is_empty() && next.is_ascii_digit() {
                identifier.push(next);
            } else {
                break
            }
        }
    
        self.prev();
        self.push(TokenType::Identifier(identifier));
    }

    fn lex_replace(&mut self) -> Result<()> {
        let mut replace_mode = ReplaceMode::Normal;
        let mut step = ReplaceLexStep::LeftPattern;
        let char_mode = self.next() == Some('\\');
        if !char_mode { self.prev(); }

        let mut push_target = String::new();

        let mut frag_start = self.token_start;
        let mut fragments = Vec::new();

        let mut pattern = Vec::new();
        let mut left_pattern = Vec::new();
        let mut left_char_pattern = String::new();
        
        let mut is_escape = false;
        let mut brace_depth = 0;

        let left_start = self.actual_pos;
        let mut right_start = 0;

        while let Some(next) = self.next() {
            if is_escape {
                // invalid escape
                if !("`\\|!@".contains(next) || !char_mode && "{},".contains(next)) {
                    push_target.push('`');
                }
                push_target.push(next);
                is_escape = false;
            }

            else if next == '`' && brace_depth == 0 {
                is_escape = true;
            }

            else if step == ReplaceLexStep::RightPattern && next == '\\' && brace_depth == 0 {
                break
            }

            else if !char_mode && next == ',' && brace_depth == 0 {
                fragments.push(LexedFragment::Literal(push_target));
                pattern.push(fragments);

                push_target = String::new();
                fragments = Vec::new();
            }

            else if step == ReplaceLexStep::LeftPattern && "\\|!@".contains(next) && brace_depth == 0 {
                step = ReplaceLexStep::RightPattern;
                
                if !char_mode {
                    if !push_target.is_empty() {
                        fragments.push(LexedFragment::Literal(push_target));
                        push_target = String::new();
                    }
                    if !fragments.is_empty() {
                        pattern.push(fragments);
                        fragments = Vec::new();
                    }
                }

                let is_empty = if char_mode { push_target.is_empty() } else { pattern.is_empty() };
                if is_empty {
                    return Err(
                        Error::err("Find pattern cannot be empty")
                            .label(self.token_start..self.actual_pos, "No find pattern provided")
                            .help("Add a find pattern")
                    )
                }

                if char_mode {
                    (left_char_pattern, push_target) = (push_target, left_char_pattern);
                } else {
                    (left_pattern, pattern) = (pattern, left_pattern);
                }

                replace_mode = ReplaceMode::from_char(next);
                right_start = self.actual_pos;
            }

            else if !char_mode && next == '{' && !is_escape {
                if brace_depth == 0 {
                    if !push_target.is_empty() {
                        fragments.push(LexedFragment::Literal(push_target));
                        push_target = String::new();
                    }
                    frag_start = self.actual_pos;
                } else {
                    push_target.push('{');
                }

                brace_depth += 1;
            }

            else if !char_mode && next == '}' && !is_escape && brace_depth > 0 {
                brace_depth -= 1;

                if brace_depth == 0 {
                    fragments.push(self.lex_expr_frag(push_target, frag_start)?);
                    push_target = String::new();
                } else {
                    push_target.push('}');
                }
            }

            else {
                push_target.push(next);
            }
        }

        if step == ReplaceLexStep::LeftPattern {
            self.prev();

            let mut err = Error::err("Unfinished replace expression")
                .label(self.token_start..self.actual_pos, "This replace expression doesn't have a replace mode specified")
                .label(self.actual_pos..self.actual_pos, "Unexpected end of file");
    
            if self.tokens.iter().any(|t| matches!(t.value, TokenType::Replace(_) | TokenType::CharReplace(_))) {
                err = err.note("Check if you forgot to close an earlier one");
            } else {
                err = err.help("Add a replace mode specifier and optionally a replace pattern");
            }
    
            return Err(err)
        }

        let is_swap = replace_mode == ReplaceMode::Swap;

        let (len_l, len_r);
        let token_value;

        if char_mode {
            (len_l, len_r) = (left_char_pattern.len(), push_target.len());
            token_value = TokenType::CharReplace(ReplaceData {
                left: left_char_pattern.chars().collect(),
                right: push_target.chars().collect(),
                mode: replace_mode,
            });
        }

        else {
            if !push_target.is_empty() {
                fragments.push(if brace_depth > 0 {
                    self.lex_expr_frag(push_target, frag_start)?
                } else {
                    LexedFragment::Literal(push_target)
                });
            }
            if !fragments.is_empty() {
                pattern.push(fragments);
            }

            (len_l, len_r) = (left_pattern.len(), pattern.len());
            token_value = TokenType::Replace(ReplaceData {
                left: left_pattern,
                right: pattern,
                mode: replace_mode,
            });
        }

        if is_swap && len_l != len_r {
            return Err(
                Error::err("Different number of swap pattern values")
                    .label(
                        left_start..right_start - 1, 
                        fmt_plural!("Left side has #{}# value{}", len_l)
                    )
                    .label(
                        right_start..self.actual_pos - 1,
                        fmt_plural!("Right side has #{}# value{}", len_r)
                    )
                    .help("Add or remove some values to balance both sides")
            )
        }
        
        else if len_l < len_r {
            return Err(
                Error::err("More replace values than find values")
                    .label(
                        left_start..right_start - 1,
                        fmt_plural!("Found #{}# find value{}", len_l)
                    )
                    .label(
                        right_start..self.actual_pos - 1,
                        fmt_plural!("Found #{}# replace value{}", len_r)
                    )
                    .help("Add more find values or remove some replace values")
            )
        }

        self.push(token_value);
        Ok(())
    }
    
    fn lex_string(&mut self) -> Result<()> {
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
                break
            }

            else if next == '{' && !is_escape {
                if brace_depth == 0 {
                    fragments.push(LexedFragment::Literal(frag));
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
                    fragments.push(self.lex_expr_frag(frag, frag_start)?);
                    frag = String::new();
                } else {
                    frag.push('}');
                }
            }

            else {
                frag.push(next);
            }
        }

        fragments.push(if brace_depth > 0 {
            self.lex_expr_frag(frag, frag_start)?
        } else {
            LexedFragment::Literal(frag)
        });
        self.push(TokenType::String(fragments));
        Ok(())
   }
    
    fn lex_number(&mut self, first: char) -> Result<()> {
        let mut number = String::from(first);
        let mut float = false;
        let mut scientific = false;
        let mut scientific_op = false;

        while let Some(next) = self.next() {
            if !float && next == '.' && !self.floating_point_override {
                float = true;
            }
            
            else if !scientific && next == 'e' {
                float = true;
                scientific = true;
            }

            else if scientific && !scientific_op && "+-".contains(next) {
                scientific_op = true;
            }
            
            else if !next.is_ascii_digit() { break }

            number.push(next)
        }
        
        self.prev();

        while !number.chars().last().unwrap().is_ascii_digit() {
            self.prev();
            number.pop();
        }

        self.push(TokenType::Number(number.parse().unwrap()));
        Ok(())
    }
    
    fn lex_symbol(&mut self, first: char) {
        let mut symbol = String::from(first);

        while let Some(next) = self.next() {
            // make sure it's not a space because that splits symbols
            if next != ' ' && (
                // continue the symbol if it's either...
                // - `symbol + next` creates one of the combined symbols
                operators::COMBINED_SYMBOLS.contains(&format!("{symbol}{next}").as_str())
                // - or `symbol` is a binary operator and `next` is '=' (augmented assignment)
                || operators::is_op(Binary, &symbol) && next == '='
            ) {
                symbol.push(next);
            }

            // comment
            else if format!("{symbol}{next}") == "###" {
                while let Some(next) = self.next() {
                    if next == '\n' { break }
                }
                return
            }

            // end symbol
            else { break }
        }

        self.floating_point_override = symbol == ".";

        self.prev();
        self.push(TokenType::Symbol(symbol));
    }

    fn lex_expr_frag(&self, expr: String, offset: usize) -> Result<LexedFragment> {
        let mut lexer = Lexer::from_str(expr, offset);
        lexer.lex()?;
        Ok(LexedFragment::Expr(lexer))
    }
}
