use std::ops::Range;

pub type ZipLonger<T> = Vec<(T, Option<T>)>;
pub type Pos = Range<usize>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PrecType {
    Before,
    LeftAssoc,
    RightAssoc,
    Compare,
    After,
    Other,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpType {
    Before,
    Binary,
    After,
    Other,
}

macro_rules! operators {
    (
        $( $type:ident: $( $op:literal ),+, )*
        [no precedence]
        $( $type_n:ident: $( $op_n:literal ),+, )*
    ) => {
        const OP_TABLE: &[(OpType, &str)] = &[
            $( $( (op_type(PrecType::$type), $op), )+ )*
            $( $( (op_type(PrecType::$type_n), $op_n), )+ )*
        ];
        const PREC_LIST: &[PrecType] = &[ $( PrecType::$type, )* ];
        const COMBINED_SYMBOLS: &[&str] = &[ $( $( $op, )+ )* $( $( $op_n, )+ )* ];
                                                 // - 2 because After and Other aren't counted
        pub const MAX_PREC: usize = PREC_LIST.len() - 2;

        pub fn op_prec(op_type: OpType, op: &str) -> usize {
            let mut prec = 0;
            $(
                prec += 1;
                if self::op_type(PrecType::$type) == op_type {
                    match op {
                        $( $op => return prec, )+
                        _ => ()
                    }
                }
            )*
            usize::MAX
        }
    }
}

pub fn is_combined_sym(sym: &str) -> bool {
    COMBINED_SYMBOLS.contains(&sym)
}

pub fn is_op(op_type: OpType, op: &str) -> bool {
    OP_TABLE.contains(&(op_type, op))
}

pub fn op_id(op_type: OpType, op: &str) -> usize {
    OP_TABLE.iter().position(|o| o == &(op_type, op)).unwrap()
}

pub fn prec_type(prec: usize) -> PrecType {
    PREC_LIST[prec - 1]
}

pub const fn op_type(prec_type: PrecType) -> OpType {
    match prec_type {
        PrecType::Before => OpType::Before,
        PrecType::LeftAssoc | PrecType::RightAssoc | PrecType::Compare => OpType::Binary,
        PrecType::After => OpType::After,
        PrecType::Other => OpType::Other,
    }
}

pub fn is_bin_type(prec: usize) -> bool {
    op_type(prec_type(prec)) == OpType::Binary
}

// precedence goes from top to bottom, same row = same precedence
// before operator / binary operator + backslash combos are an option for new operators
operators! {
    RightAssoc: "||",
    RightAssoc: "&&",
    RightAssoc: "|",
    RightAssoc: "&",
    Before:     "!",

    Compare:    "<", ">", "<=", ">=", "!=", "==",
    LeftAssoc:  "-?",

    LeftAssoc:  "..", "#",
    Before:     "^",

    LeftAssoc:  "??",
    Before:     "??",

    LeftAssoc:  "%",
    LeftAssoc:  "+", "-",
    LeftAssoc:  "*", "/", "/.",
    LeftAssoc:  "+-", "/%",

    Before:     "-", ".", "`", "``",

    RightAssoc: "^",

    Before:     "*",

    LeftAssoc:  ".*", "^*",
    LeftAssoc:  "@",

    Before:     "@", "^^", "#", "'",

    [no precedence]
    Before:     "++", "--",
    After:      "++", "--", "^^", "##", "#\\", "#$", "_", "``", "$$", "$", "'", "`",
    Other:      "%%",
}
