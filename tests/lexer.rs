use panfix::implementation::lexer::LexerBuilder;
use panfix::{Lexeme, Span, Token};

fn get_span(src: &str, span: Span) -> &str {
    // Assuming that no lexeme spans multiple lines!
    let line = &src.lines().nth(span.start.line as usize).unwrap();
    &line[span.start.col as usize..span.end.col as usize]
}

#[track_caller]
fn assert_lexeme(
    src: &str,
    stream: &mut impl Iterator<Item = Lexeme>,
    expected: &str,
    token: Token,
) {
    let lex = stream
        .next()
        .expect("Token stream in test case ended early");
    assert_eq!(lex.token, token);
    let start = lex.span.start;
    let end = lex.span.end;
    let lexeme = get_span(src, lex.span);
    let actual = format!(
        "{}:{}({})-{}:{}({}) {}",
        start.line, start.col, start.utf8_col, end.line, end.col, end.utf8_col, lexeme
    );
    assert_eq!(actual, expected);
}

#[test]
fn test_lexing_json() {
    let string_regex = "\"([^\"\\\\]|\\\\.)*\"";
    let number_regex = "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?";
    let whitespace_regex = "[ \\t\\n\\r\\v]*";

    let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
    let tok_str = builder.regex(string_regex).unwrap();
    let tok_num = builder.regex(number_regex).unwrap();
    let tok_true = builder.string("true").unwrap();
    let tok_false = builder.string("false").unwrap();
    let tok_null = builder.string("null").unwrap();
    let tok_colon = builder.string(":").unwrap();
    let tok_comma = builder.string(",").unwrap();
    let tok_lbracket = builder.string("[").unwrap();
    let tok_rbracket = builder.string("]").unwrap();
    let tok_lbrace = builder.string("{").unwrap();
    let tok_rbrace = builder.string("}").unwrap();
    let lexer = builder.finish().unwrap();

    let src = " 3.1e5";
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "0:1(1)-0:6(6) 3.1e5", tok_num);
    assert!(lexemes.next().is_none());

    let src = r#"{false]true  [5"5\"" "#;
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "0:0(0)-0:1(1) {", tok_lbrace);
    assert_lexeme(src, lexemes, "0:1(1)-0:6(6) false", tok_false);
    assert_lexeme(src, lexemes, "0:6(6)-0:7(7) ]", tok_rbracket);
    assert_lexeme(src, lexemes, "0:7(7)-0:11(11) true", tok_true);
    assert_lexeme(src, lexemes, "0:13(13)-0:14(14) [", tok_lbracket);
    assert_lexeme(src, lexemes, "0:14(14)-0:15(15) 5", tok_num);
    assert_lexeme(src, lexemes, "0:15(15)-0:20(20) \"5\\\"\"", tok_str);
    assert!(lexemes.next().is_none());

    let src = " \r\n\"Καλημέρα\" :\"Καλησπέρα\",\n  \"Καληνύχτα\"null}";
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "1:0(0)-1:18(10) \"Καλημέρα\"", tok_str);
    assert_lexeme(src, lexemes, "1:19(11)-1:20(12) :", tok_colon);
    assert_lexeme(src, lexemes, "1:20(12)-1:40(23) \"Καλησπέρα\"", tok_str);
    assert_lexeme(src, lexemes, "1:40(23)-1:41(24) ,", tok_comma);
    assert_lexeme(src, lexemes, "2:2(2)-2:22(13) \"Καληνύχτα\"", tok_str);
    assert_lexeme(src, lexemes, "2:22(13)-2:26(17) null", tok_null);
    assert_lexeme(src, lexemes, "2:26(17)-2:27(18) }", tok_rbrace);
    assert!(lexemes.next().is_none());
}

#[test]
fn test_lexing_horrible_things() {
    let word_regex = "[a-yA-Y]+";
    let angry_word_regex = "[A-Y]+";
    let short_word_regex = "[a-zA-Z]";
    let whitespace_regex = "[ \\t\\n\\re]*";

    let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
    builder.regex(angry_word_regex).unwrap();
    builder.regex(word_regex).unwrap();
    builder.regex(short_word_regex).unwrap();
    builder.string(":").unwrap();
    builder.string("::").unwrap();
    builder.string(":::").unwrap();
    builder.string("(").unwrap();
    builder.string(")").unwrap();
    builder.string(":(").unwrap();
    builder.string("true").unwrap();
    builder.string("truer").unwrap();
    builder.string("truest").unwrap();
    let lexer = builder.finish().unwrap();

    let lex = |source| {
        lexer
            .lex(source)
            .map(|lex| get_span(source, lex.span))
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
    assert_eq!(lex("tr\nue"), vec!["tr", "ue"]);
    assert_eq!(lex("tr%ue%% %%true"), vec!["tr", "%ue%%", "%%true"]);
}
