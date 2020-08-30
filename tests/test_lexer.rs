use panfix::lexer::{Lexer, LexerBuilder, Token};

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
    DoubleColon, // for testing
    Comma,
    _LexError,
    _Missing,
    _Juxtapose,
}

impl Token for JsonToken {
    const LEX_ERROR: JsonToken = JsonToken::_LexError;
    const MISSING: JsonToken = JsonToken::_Missing;
    const JUXTAPOSE: JsonToken = JsonToken::_Juxtapose;

    fn as_usize(self) -> usize {
        self as usize
    }
}

fn json_lexer() -> Lexer<JsonToken> {
    use JsonToken::*;

    let string_regex = "\"([^\"\\\\]|\\\\.)*\"";
    let number_regex = "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?";
    let whitespace_regex = "[ \\t\\n\\r\\v]*";
    LexerBuilder::new(whitespace_regex)
        .regex(string_regex, JString)
        .regex(number_regex, Number)
        .constant("true", True)
        .constant("false", False)
        .constant("null", Null)
        .constant(":", Colon)
        .constant("::", DoubleColon)
        .constant(",", Comma)
        .constant("[", OpenBracket)
        .constant("]", CloseBracket)
        .constant("{", OpenBrace)
        .constant("}", CloseBrace)
        .build()
        .unwrap()
}

#[test]
fn test_lexer() {
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
    assert_eq!(lex(":::::"), vec!["::", "::", ":"]);

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
