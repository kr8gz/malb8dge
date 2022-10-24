use super::{lexer::*, tokens::*};

macro_rules! token_test {
    {
        $name:ident
        $code:literal
        $( $value:expr, $pos:expr )+
    } => {
        #[test]
        fn $name() {
            let lexer = lex!($code, 0);
            let tokens = dbg!(lexer.tokens); // only prints on fail

            let mut i = 0;
            $(
                assert!(tokens.get(i) == Some(&Token {
                    value: $value,
                    macro_data: None,
                    pos: $pos,
                }));
                i += 1;
            )+
            assert!(tokens.len() == i);
        }
    }
}

macro_rules! lex {
    ( $code:expr, $offset:expr ) => {
        {
            let mut lexer = Lexer::new($code.into(), $offset);
            lexer.lex().unwrap_or_else(|e| e.eprint("<test>.mlb8", $code));
            lexer
        }
    }
}

token_test! {
    identifier
    
    "identifier"

    TokenType::Identifier("identifier".into()), 0..10
}

token_test! {
    integer
    
    "01234567890"

    TokenType::Number(1234567890.0), 0..11
}

token_test! {
    float

    "012345.67890"

    TokenType::Number(12345.6789), 0..12
}

token_test! {
    identifier_and_comment
    
    "ident###ifier"

    TokenType::Identifier("ident".into()), 0..5
}

token_test! {
    identifiers_and_newlines

    "hello\nworld"

    TokenType::Identifier("hello".into()), 0..5
    TokenType::Symbol("\n".into()),        5..6
    TokenType::Identifier("world".into()), 6..11
}

token_test! {
    operators

    "+ * //"

    TokenType::Symbol("+".into()),  0..1
    TokenType::Symbol("*".into()),  2..3
    TokenType::Symbol("//".into()), 4..6
}

token_test! {
    augmented_assignment

    "a += 3"

    TokenType::Identifier("a".into()),  0..1
    TokenType::Symbol("+=".into()),     2..4
    TokenType::Number(3.0),             5..6
}

token_test! {
    many_symbols

    "+++==$$$ $$$```"

    TokenType::Symbol("++".into()),     0..2
    TokenType::Symbol("+=".into()),     2..4
    TokenType::Symbol("=".into()),      4..5
    TokenType::Symbol("$$".into()),     5..7
    TokenType::Symbol("$".into()),      7..8

    TokenType::Symbol("$$".into()),     9..11
    TokenType::Symbol("$".into()),      11..12
    TokenType::Symbol("``".into()),     12..14
    TokenType::Symbol("`".into()),      14..15
}

token_test! {
    float_override

    "list.0.0 + 0.0"

    TokenType::Identifier("list".into()),   0..4
    TokenType::Symbol(".".into()),          4..5
    TokenType::Number(0.0),                 5..6
    TokenType::Symbol(".".into()),          6..7
    TokenType::Number(0.0),                 7..8

    TokenType::Symbol("+".into()),          9..10

    TokenType::Number(0.0),                 11..14
}

token_test! {
    simple_string
    
    "\"Hello World!\""

    TokenType::String("Hello World!".into()), 0..14
}

token_test! {
    interpolated_string

    "\"Hello {world}!\""

    TokenType::FragmentString(vec![
        LexedFragment::Literal("Hello ".into()),
        LexedFragment::Expr(lex!("world", 8)),
        LexedFragment::Literal("!".into()),
    ]), 0..16
}

token_test! {
    nested_string

    "\"There {apples == 1 ? \"is 1 apple\" ! \"are {apples} apples\"}!\"  ### nesting"

    TokenType::FragmentString(vec![
        LexedFragment::Literal("There ".into()),
        LexedFragment::Expr(lex!("apples == 1 ? \"is 1 apple\" ! \"are {apples} apples\"", 8)),
        LexedFragment::Literal("!".into()),
    ]), 0..61
}

token_test! {
    string_escapes

    "\"[\\n, \\t, \\\", \\\\, \\{, \\}, \\invalid]\""

    TokenType::String("[\n, \t, \", \\, {, }, \\invalid]".into()), 0..36
}

token_test! {
    simple_replace

    "s\\ab,cd\\ef,gh\\"

    TokenType::Identifier("s".into()),              0..1

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("ab".into())],
            vec![LexedFragment::Literal("cd".into())],
        ],
        right: vec![
            vec![LexedFragment::Literal("ef".into())],
            vec![LexedFragment::Literal("gh".into())],
        ],
        mode: ReplaceMode::Normal,
    }),                                             1..14
}

token_test! {
    char_replace

    "s\\\\ab,cd\\ef,gh\\"

    TokenType::Identifier("s".into()),              0..1

    TokenType::CharReplace(ReplaceData {
        left: "ab,cd".chars().collect(),
        right: "ef,gh".chars().collect(),
        mode: ReplaceMode::Normal,
    }),                                             1..15
}

token_test! {
    replace_escapes

    "s\\``,`\\,`|,`!,`@,`{,`},`,\\\\"

    TokenType::Identifier("s".into()),              0..1

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("`".into())],
            vec![LexedFragment::Literal("\\".into())],
            vec![LexedFragment::Literal("|".into())],
            vec![LexedFragment::Literal("!".into())],
            vec![LexedFragment::Literal("@".into())],
            vec![LexedFragment::Literal("{".into())],
            vec![LexedFragment::Literal("}".into())],
            vec![LexedFragment::Literal(",".into())],
        ],
        right: vec![],
        mode: ReplaceMode::Normal,
    }),                                             1..27
}

token_test! {
    replace_modes

    "s\\a\\b\\\\c|d\\\\e!f\\\\g@h\\"

    TokenType::Identifier("s".into()),              0..1

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("a".into())],
        ],
        right: vec![
            vec![LexedFragment::Literal("b".into())],
        ],
        mode: ReplaceMode::Normal,
    }),                                             1..6

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("c".into())],
        ],
        right: vec![
            vec![LexedFragment::Literal("d".into())],
        ],
        mode: ReplaceMode::Swap,
    }),                                             6..11

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("e".into())],
        ],
        right: vec![
            vec![LexedFragment::Literal("f".into())],
        ],
        mode: ReplaceMode::First,
    }),                                             11..16

    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("g".into())],
        ],
        right: vec![
            vec![LexedFragment::Literal("h".into())],
        ],
        mode: ReplaceMode::Last,
    }),                                             16..21
}

token_test! {
    char_replace_escapes

    "s\\\\``,`\\,`|,`!,`@,`{,`},`,\\\\"

    TokenType::Identifier("s".into()),              0..1
    TokenType::CharReplace(ReplaceData {
        left: "`,\\,|,!,@,`{,`},`,".chars().collect(),
        right: "".chars().collect(),
        mode: ReplaceMode::Normal,
    }),                                             1..28
}

token_test! {
    interpolated_replace

    "s\\`{_}\\{_}\\"

    TokenType::Identifier("s".into()),              0..1
    TokenType::Replace(ReplaceData {
        left: vec![
            vec![LexedFragment::Literal("{_}".into())]
        ],
        right: vec![
            vec![LexedFragment::Expr(lex!("_", 8))]
        ],
        mode: ReplaceMode::Normal,
    }),                                             1..11
}
