use std::ops::Range;

use self::errors::Error;

pub mod errors;
pub mod operators;

pub type Pos = Range<usize>;
pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! fmt_plural {
    ( $s:literal, $n:expr ) => {
        format!($s, $n, if $n == 1 { "" } else { "s" })
    }
}

pub trait Reverse {
    fn reverse(&self) -> Self;
}

impl Reverse for String {
    fn reverse(&self) -> String {
        self.chars().rev().collect()
    }
}

pub fn trim_nl(s: &mut String) {
    while s.ends_with(['\n', '\r']) { s.pop(); }
}
