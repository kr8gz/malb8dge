#[cfg(test)]
mod tests {
    use crate::lexer::*;

    macro_rules! token_test {
        {
            $name:ident
            $c:literal
            $( $a:expr, $b:expr )+
        } => {
            #[test]
            fn $name() {
                let lexer = Lexer::from_str("<test>.mlb8".into(), $c.into(), 0);
                let tokens = dbg!(lexer.tokens); // only prints on fail

                let mut i = 0;
                $(
                    assert!(tokens.get(i) == Some(&Token {
                        value: $a,
                        macro_data: None,
                        pos: $b,
                    }));
                    i += 1;
                )+
                assert!(tokens.len() == i);
            }
        }
    }

    token_test! {
        lex_identifier
        
        "identifier"

        TokenType::Identifier("identifier".into()), 0..10
    }

    token_test! {
        lex_integer
        
        "01234567890"

        TokenType::Integer(1234567890), 0..11
    }

    token_test! {
        lex_float

        "12345.67890"

        TokenType::Float(12345.6789), 0..11
    }

    token_test! {
        lex_identifier_and_comment
        
        "ident###ifier"

        TokenType::Identifier("ident".into()), 0..5
    }

    token_test! {
        lex_identifiers_and_newlines

        "hello\nworld"

        TokenType::Identifier("hello".into()), 0..5
        TokenType::Symbol("\n".into()),        5..6
        TokenType::Identifier("world".into()), 6..11
    }

    token_test! {
        lex_operators

        "+ * /."

        TokenType::Symbol("+".into()),  0..1
        TokenType::Symbol("*".into()),  2..3
        TokenType::Symbol("/.".into()), 4..6
    }

    token_test! {
        lex_augmented_assignment

        "a += 3"

        TokenType::Identifier("a".into()),  0..1
        TokenType::Symbol("+=".into()),     2..4
        TokenType::Integer(3),              5..6
    }

    token_test! {
        lex_many_symbols

        "+++=== ===```"

        TokenType::Symbol("++".into()),     0..2
        TokenType::Symbol("+=".into()),     2..4
        TokenType::Symbol("==".into()),     4..6

        TokenType::Symbol("===".into()),    7..10
        TokenType::Symbol("``".into()),     10..12
        TokenType::Symbol("`".into()),      12..13
    }

    token_test! {
        lex_float_override

        "list.0.0 + 0.0"

        TokenType::Identifier("list".into()),   0..4
        TokenType::Symbol(".".into()),          4..5
        TokenType::Integer(0),                  5..6
        TokenType::Symbol(".".into()),          6..7
        TokenType::Integer(0),                  7..8

        TokenType::Symbol("+".into()),          9..10

        TokenType::Float(0.0),                  11..14
    }

    token_test! {
        lex_simple_string
        
        "\"Hello World!\""

        TokenType::String(vec![
            Fragment::Literal("Hello World!".into())
        ]), 0..14
    }

    token_test! {
        lex_interpolated_string

        "\"Hello {world}!\""

        TokenType::String(vec![
            Fragment::Literal("Hello ".into()),
            Fragment::Expr("world".into(), 8),
            Fragment::Literal("!".into()),
        ]), 0..16
    }

    token_test! {
        lex_nested_string

        "\"There {apples == 1 ? \"is 1 apple\" ! \"are {apples} apples\"}!\"  ### nesting"

        TokenType::String(vec![
            Fragment::Literal("There ".into()),
            Fragment::Expr("apples == 1 ? \"is 1 apple\" ! \"are {apples} apples\"".into(), 8),
            Fragment::Literal("!".into()),
        ]), 0..61
    }

    token_test! {
        lex_string_escapes

        "\"[\\n, \\t, \\\", \\\\, \\{, \\}, \\invalid]\""

        TokenType::String(vec![
            Fragment::Literal("[\n, \t, \", \\, {, }, \\invalid]".into())
        ]), 0..36
    }

    token_test! {
        lex_simple_replace

        "s\\ab,cd\\ef,gh\\"

        TokenType::Identifier("s".into()),              0..1

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("ab".into())],
                vec![Fragment::Literal("cd".into())],
            ],
            right: vec![
                vec![Fragment::Literal("ef".into())],
                vec![Fragment::Literal("gh".into())],
            ],
            mode: ReplaceMode::Normal,
        }),                                         1..14
    }

    token_test! {
        lex_char_replace

        "s\\\\ab,cd\\ef,gh\\"

        TokenType::Identifier("s".into()),              0..1

        TokenType::CharReplace(CharReplaceData {
            left: "ab,cd".into(),
            right: "ef,gh".into(),
            mode: ReplaceMode::Normal,
        }),                                         1..15
    }

    token_test! {
        lex_replace_escapes

        "s\\``,`\\,`|,`!,`@,`{,`},`,\\\\"

        TokenType::Identifier("s".into()),              0..1

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("`".into())],
                vec![Fragment::Literal("\\".into())],
                vec![Fragment::Literal("|".into())],
                vec![Fragment::Literal("!".into())],
                vec![Fragment::Literal("@".into())],
                vec![Fragment::Literal("{".into())],
                vec![Fragment::Literal("}".into())],
                vec![Fragment::Literal(",".into())],
            ],
            right: vec![],
            mode: ReplaceMode::Normal,
        }),                                         1..27
    }

    token_test! {
        lex_replace_modes

        "s\\a\\b\\\\c|d\\\\e!f\\\\g@h\\"

        TokenType::Identifier("s".into()),              0..1

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("a".into())],
            ],
            right: vec![
                vec![Fragment::Literal("b".into())],
            ],
            mode: ReplaceMode::Normal,
        }),                                         1..6

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("c".into())],
            ],
            right: vec![
                vec![Fragment::Literal("d".into())],
            ],
            mode: ReplaceMode::Swap,
        }),                                         6..11

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("e".into())],
            ],
            right: vec![
                vec![Fragment::Literal("f".into())],
            ],
            mode: ReplaceMode::First,
        }),                                         11..16

        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("g".into())],
            ],
            right: vec![
                vec![Fragment::Literal("h".into())],
            ],
            mode: ReplaceMode::Last,
        }),                                         16..21
    }

    token_test! {
        lex_char_replace_escapes

        "s\\\\``,`\\,`|,`!,`@,`{,`},`,\\\\"

        TokenType::Identifier("s".into()),              0..1
        TokenType::CharReplace(CharReplaceData {
            left: "`,\\,|,!,@,`{,`},`,".into(),
            right: "".into(),
            mode: ReplaceMode::Normal,
        }),                                         1..28
    }

    token_test! {
        lex_interpolated_replace

        "s\\`{_}\\{_}\\"

        TokenType::Identifier("s".into()),              0..1
        TokenType::Replace(ReplaceData {
            left: vec![
                vec![Fragment::Literal("{_}".into())]
            ],
            right: vec![
                vec![Fragment::Expr("_".into(), 8)]
            ],
            mode: ReplaceMode::Normal,
        }),                                         1..11
    }
}
