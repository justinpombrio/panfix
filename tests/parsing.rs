use panfix::{pattern, Grammar, GrammarError, Parser, Source};

#[track_caller]
fn assert_parse(parser: &Parser<&'static str>, src: &str, expected: &str) {
    let source = Source::new("testcase", src.to_owned());
    let tree = parser.parse(&source).unwrap();
    assert_eq!(format!("{}", tree.visitor()), expected);
}

#[track_caller]
fn assert_error(parser: &Parser<&'static str>, src: &str, expected: &str) {
    let source = Source::new("testcase", src.to_owned());
    let err = parser.parse(&source).unwrap_err();
    assert_eq!(
        format!("{}", err.display_with_color_override(false)),
        expected
    );
}

#[test]
fn test_lexing_error() {
    fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.left_assoc();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_error(
        &parser,
        "%!",
        r#"Parse Error: Unrecognized token.
 --> testcase:1:1
  |
1 |%!
  |^^ unrecognized token
"#,
    );

    assert_error(
        &parser,
        "5\n + %! + 8",
        r#"Parse Error: Unrecognized token.
 --> testcase:2:4
  |
2 | + %! + 8
  |   ^^ unrecognized token
"#,
    );
}

#[test]
fn test_parsing_assoc() {
    fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.right_assoc();
        grammar.op("exp", pattern!(_ "^" _))?;
        grammar.left_assoc();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "1^2^3", "(exp 1 (exp 2 3))");
    assert_parse(&parser, "1 + 2 + 3", "(plus (plus 1 2) 3)");
}

#[test]
fn test_parsing_minus() {
    fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.left_assoc();
        grammar.op("neg", pattern!("-" _))?;
        grammar.left_assoc();
        grammar.op("minus", pattern!(_ "-" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "- 2", "(neg 2)");
    assert_parse(&parser, "--2", "(neg (neg 2))");
    assert_parse(&parser, "1 - 2 - 3", "(minus (minus 1 2) 3)");
    assert_parse(&parser, "1 - -2 - 3", "(minus (minus 1 (neg 2)) 3)");
}

#[test]
fn test_parsing_blank() {
    fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.op("parens", pattern!("(" ")"))?;
        grammar.left_assoc();
        grammar.op("neg", pattern!("-" _))?;
        grammar.left_assoc();
        grammar.op("minus", pattern!(_ "-" _))?;
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "+2", "(plus _ 2)");
    assert_parse(&parser, "+", "(plus _ _)");
    assert_parse(&parser, "3++", "(plus (plus 3 _) _)");
    assert_parse(&parser, "-", "(neg _)");
    assert_parse(&parser, "+--+", "(plus (plus _ (neg (neg _))) _)");
    assert_parse(&parser, "+--+", "(plus (plus _ (neg (neg _))) _)");
    assert_parse(&parser, "()", "(parens _)");
}

#[test]
fn test_parsing_juxtapose() {
    fn make_parser_1() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.left_assoc();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser_1().unwrap();

    assert_parse(&parser, "1 2", "(_ 1 2)");
    assert_parse(&parser, "1 2 3", "(_ (_ 1 2) 3)");
    assert_parse(&parser, "1 2 + 3 4", "(plus (_ 1 2) (_ 3 4))");

    fn make_parser_2() -> Result<Parser<&'static str>, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.regex("num", "[0-9]+")?;
        grammar.left_assoc();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.right_assoc();
        grammar.juxtapose()?;
        grammar.finish()
    }
    let parser = make_parser_2().unwrap();

    assert_parse(&parser, "1 2", "(_ 1 2)");
    assert_parse(&parser, "1 2 3", "(_ 1 (_ 2 3))");
    assert_parse(&parser, "1 2 + 3 4", "(_ 1 (_ (plus 2 3) 4))");
}
