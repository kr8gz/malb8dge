use std::ops::Range;

pub type Pos = Range<usize>;

macro_rules! symbols {
    (
        Unary  { $( $u:literal, )* }
        Binary { $( $b:literal, )* }
        After  { $( $a:literal, )* }
        Other  { $( $o:literal, )* }
    ) => {
        pub const UNARY_OPERATORS: &[&str] = &[
            $( $u, )*
        ];

        pub const BINARY_OPERATORS: &[&str] = &[
            $( $b, )*
        ];

        pub const AFTER_OPERATORS: &[&str] = &[
            $( $a, )*
        ];

        pub const OTHER_COMBINED_SYMBOLS: &[&str] = &[
            $( $o, )*
        ];

        pub const ALL_SYMBOLS: &[&str] = &[
            $( $u, )*
            $( $b, )*
            $( $a, )*
            $( $o, )*
        ];
    }
}

symbols! {
    Unary {
        // string stuff
        ".", "..", "`", "``",
    
        // misc stuff
        "-", "@", "^", "^^", "#", "!", "'", "*", "??",
    }

    Binary {
        // string stuff
        "#", "^",

        // misc stuff
        "??", "@",

        // math stuff
        "^*", ".*", "**", "/%", "+-", "/.", "/", "*", "-", "+", "%",

        // logic stuff
        ">", "<", ">>", "<<", "==", "!=", "&", "|", "&&", "||",
    }

    // can't have any of these in binary_operators
    After {
        // string stuff
        "`", "^^", "##", "#$", "_", "``", "$$", "@@", "..",

        // number stuff
        "$", "'", "-?",
    }

    Other {
        "++", "--", "%%",
    }
}
