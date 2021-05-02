#[cfg(test)]
mod lexing_tests {
    use panfix::lexing::{Lexer, LexerBuilder, Token};

    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
    enum JsonToken {
        True,
        False,
        Null,
        JString,
        Number,
        OpenBrace,
        CloseBrace,
        OpenBracket,
        CloseBracket,
        Colon,
        Comma,
        _LexError,
    }

    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
    enum TokenThatHatesYou {
        Colon,
        DoubleColon,
        TripleColon,
        OpenParen,
        CloseParen,
        SadFace,
        True,
        Truer,
        Truest,
        Word,
        AngryWord,
        ShortWord,
        _LexError,
    }

    impl Token for JsonToken {
        const LEX_ERROR: JsonToken = JsonToken::_LexError;
    }

    impl Token for TokenThatHatesYou {
        const LEX_ERROR: TokenThatHatesYou = TokenThatHatesYou::_LexError;
    }

    fn json_lexer() -> Lexer<JsonToken> {
        use JsonToken::*;

        let string_regex = "\"([^\"\\\\]|\\\\.)*\"";
        let number_regex = "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?";

        LexerBuilder::new()
            .unicode_whitespace()
            .regex(string_regex, JString)
            .regex(number_regex, Number)
            .string("true", True)
            .string("false", False)
            .string("null", Null)
            .string(":", Colon)
            .string(",", Comma)
            .string("[", OpenBracket)
            .string("]", CloseBracket)
            .string("{", OpenBrace)
            .string("}", CloseBrace)
            .build()
            .unwrap()
    }

    fn hate_lexer() -> Lexer<TokenThatHatesYou> {
        use TokenThatHatesYou::*;

        let word_regex = "[a-yA-Y]+";
        let angry_word_regex = "[A-Y]+";
        let short_word_regex = "[a-zA-Z]";
        let whitespace_regex = "[ \\t\\n\\re]";

        LexerBuilder::new()
            .whitespace(whitespace_regex)
            .regex(angry_word_regex, AngryWord)
            .regex(word_regex, Word)
            .regex(short_word_regex, ShortWord)
            .string(":", Colon)
            .string(":::", TripleColon)
            .string("::", DoubleColon)
            .string("(", OpenParen)
            .string(")", CloseParen)
            .string(":(", SadFace)
            .string("true", True)
            .string("truer", Truer)
            .string("truerest", Truest)
            .build()
            .unwrap()
    }

    #[test]
    fn test_lexing_json() {
        let lexer = json_lexer();
        let lex = |source| {
            lexer
                .lex(source)
                .map(|l| &source[l.span.0..l.span.1])
                .collect::<Vec<_>>()
        };
        assert_eq!(lex("3.1e5"), vec!["3.1e5"]);
        assert_eq!(
            lex("{false]true  [5\"5\\\"\""),
            vec!["{", "false", "]", "true", "[", "5", "\"5\\\"\""]
        );

        /*
         SPEED TEST:
        use std::fs::File;
        use std::io::{BufRead, BufReader};
        let file = File::open("large-file.json").unwrap();
        let reader = BufReader::new(file);
        let mut lexeme_count = 0;
        for line in reader.lines() {
            let line = &line.unwrap();
            for _ in lexer.lex(line) {
                lexeme_count += 1;
            }
        }
        // This could use some external verification
        assert_eq!(lexeme_count, 2507032);
        */
    }

    #[test]
    fn test_lexing_hate() {
        let lexer = hate_lexer();
        let lex = |source| {
            lexer
                .lex(source)
                .map(|l| &source[l.span.0..l.span.1])
                .collect::<Vec<_>>()
        };
        assert_eq!(lex("HELLO"), vec!["HELLO"]);
        assert_eq!(lex("Hello"), vec!["Hello"]);
        assert_eq!(lex("hello"), vec!["hello"]);
        assert_eq!(lex(":()"), vec![":(", ")"]);
        assert_eq!(lex(":::()"), vec![":::", "(", ")"]);
        assert_eq!(lex("::::()"), vec![":::", ":(", ")"]);
        assert_eq!(lex(":::::()"), vec![":::", "::", "(", ")"]);
        assert_eq!(lex("truertruetruerest"), vec!["truertruetruerest"]);
        assert_eq!(
            lex("truer true truerest"),
            vec!["truer", "true", "truerest"]
        );
        assert_eq!(
            lex("truery truey truerestytrue"),
            vec!["truery", "truey", "truerestytrue"]
        );
        assert_eq!(lex(" eprom "), vec!["prom"]);
        assert_eq!(lex("true! true"), vec!["true", "!", "true"]);
        assert_eq!(lex("tr%ue %%t rue"), vec!["tr", "%u", "%%t", "rue"]);
        assert_eq!(lex("tr\nue"), vec!["tr", "ue"]);
    }
}
