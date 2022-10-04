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
                                                        // - 2 because After and Other aren't counted
        pub const MAX_PREC: usize = [ $( $type, )* ].len() - 2;
        const OP_TABLE: &[(OpType, &str)] = &[ $( $( ($type, $op), )+ )* ];

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

        pub fn prec_type(prec: usize) -> OpType {
            assert!(0 < prec && prec <= MAX_PREC);
            [ $( $type, )* ][prec - 1]
        }
    }
}

pub fn is_op(op_type: OpType, op: &str) -> bool {
    OP_TABLE.contains(&(op_type, op))
}

pub fn is_bin_op(op: &str) -> bool {
    OP_TABLE.contains(&(LeftAssoc, op)) || OP_TABLE.contains(&(RightAssoc, op))
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

    LeftAssoc:  "#",
    LeftAssoc:  "..",
    Before:     "^",

    LeftAssoc:  "??",
    Before:     "??",

    LeftAssoc:  "%",
    LeftAssoc:  "+", "-",

    Before:     ".", "`", "``",

    LeftAssoc:  "*", "/", "/.",
    LeftAssoc:  "+-", "/%",

    Before:     "-",

    RightAssoc: "^",

    Before:     "*",

    LeftAssoc:  ".*", "^*",
    LeftAssoc:  "@",

    Before:     "@", "^^", "#", "'",

    // no precedence
    After:      "^^", "^\\", "##", "#$", "_", "``", "$$", "$", "'", "`", "..",
    Other:      "++", "--", "%%",
}
