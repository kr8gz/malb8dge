use std::ops::Range;

pub type Pos = Range<usize>;
pub type ZipLonger<T> = Vec<(T, Option<T>)>;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum OpType {
    Before,
    LeftAssoc,
    RightAssoc,
    Compare,
    After,
    Other,
}

macro_rules! operators {
    (
        $( $type:ident: $( $op:literal ),+, )*
    ) => {
        use OpType::*;

        const OP_TABLE: &[(OpType, &str)] = &[ $( $( ($type, $op), )+ )* ];
        const PREC_LIST: &[OpType] = &[ $( $type, )* ];
                                                 // - 2 because After and Other aren't counted
        pub const MAX_PREC: usize = PREC_LIST.len() - 2;

        pub fn is_sym(sym: &str) -> bool {
            [ $( $( $op, )+ )* ].contains(&sym)
        }

        pub fn op_prec(op_type: OpType, op: &str) -> usize {
            let mut prec = 0;
            $(
                prec += 1;
                if $type == op_type {
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

pub fn is_op(op_type: OpType, op: &str) -> bool {
    OP_TABLE.contains(&(op_type, op))
}

pub fn is_bin_op(op: &str) -> bool {
    is_op(LeftAssoc, op) || is_op(RightAssoc, op)
}

pub fn prec_type(prec: usize) -> OpType {
    PREC_LIST[prec - 1]
}

pub fn is_bin_type(op_type: OpType) -> bool {
    matches!(op_type, LeftAssoc | RightAssoc | Compare)
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

    // no precedence
    After:      "^^", "^\\", "##", "#$", "_", "``", "$$", "$", "'", "`",
    Other:      "++", "--", "%%",
}
