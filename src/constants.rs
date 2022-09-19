use std::ops::Range;

pub type Pos = Range<usize>;

macro_rules! symbols {
    (
        Before  { $( $before:literal, )* }
        Binary { $( $binary:literal, )* }
        After  { $( $after:literal, )* }
        Compare { $( $compare:literal, )* }
        Other  { $( $other:literal, )* }
    ) => {
        pub const BEFORE_OPERATORS: &[&str] = &[
            $( $before, )*
        ];

        pub const BINARY_OPERATORS: &[&str] = &[
            $( $binary, )*
        ];

        pub const AFTER_OPERATORS: &[&str] = &[
            $( $after, )*
        ];

        pub const COMPARE_OPERATORS: &[&str] = &[
            $( $compare, )*
        ];

        pub const OTHER_COMBINED_SYMBOLS: &[&str] = &[
            $( $other, )*
        ];

        pub const ALL_SYMBOLS: &[&str] = &[
            $( $before, )*
            $( $binary, )*
            $( $after, )*
            $( $compare, )*
            $( $other, )*
        ];
    }
}

symbols! {
    Before {
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
        "&", "|", "&&", "||",
    }

    // can't have any of these in binary_operators
    After {
        // string stuff
        "`", "^^", "##", "#$", "_", "``", "$$", "@@", "..",

        // number stuff
        "$", "'", "-?",
    }

    Compare {
        ">", "<", ">=", "<=", "==", "!=",
    }

    Other {
        "++", "--", "%%",
    }
}
