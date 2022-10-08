use std::ops::Range;

use self::errors::Error;

pub mod errors;
pub mod operators;

pub type Pos = Range<usize>;
pub type Result<T> = std::result::Result<T, Error>;
