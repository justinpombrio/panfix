use panfix::{pattern, Grammar, GrammarError, Parser, Source};

#[track_caller]
fn assert_parse(parser: &Parser, sort: &str, src: &str, expected: &str) {
    let source = Source::new("testcase", src.to_owned());
    let tree = parser.parse(&source, sort).unwrap();
    assert_eq!(format!("{}", tree.visitor()), expected);
}

#[test]
fn test_parsing_assoc() {
    fn make_parser() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;
        grammar.rgroup();
        grammar.op("exp", pattern!(_ "^" _))?;
        grammar.lgroup();
        grammar.op("plus", pattern!(_ "+" _))?;
        println!("{}", grammar.show_table());
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "Expr", "1^2^3", "(exp 1 (exp 2 3))");
    assert_parse(&parser, "Expr", "1 + 2 + 3", "(plus (plus 1 2) 3)");
}

#[test]
fn test_parsing_minus() {
    fn make_parser() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;
        grammar.lgroup();
        grammar.op("neg", pattern!("-" _))?;
        grammar.lgroup();
        grammar.op("minus", pattern!(_ "-" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "Expr", "- 2", "(neg 2)");
    assert_parse(&parser, "Expr", "--2", "(neg (neg 2))");
    assert_parse(&parser, "Expr", "1 - 2 - 3", "(minus (minus 1 2) 3)");
    assert_parse(&parser, "Expr", "1 - -2 - 3", "(minus (minus 1 (neg 2)) 3)");
}

#[test]
fn test_parsing_blank() {
    fn make_parser() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;
        grammar.lgroup();
        grammar.op("neg", pattern!("-" _))?;
        grammar.lgroup();
        grammar.op("minus", pattern!(_ "-" _))?;
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser().unwrap();

    assert_parse(&parser, "Expr", "+2", "(plus _ 2)");
    assert_parse(&parser, "Expr", "+", "(plus _ _)");
    assert_parse(&parser, "Expr", "3++", "(plus (plus 3 _) _)");
    assert_parse(&parser, "Expr", "-", "(neg _)");
    assert_parse(&parser, "Expr", "+--+", "(plus (plus _ (neg (neg _))) _)");
    assert_parse(&parser, "Expr", "+--+", "(plus (plus _ (neg (neg _))) _)");
}

#[test]
fn test_parsing_juxtapose() {
    fn make_parser_1() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;
        grammar.lgroup();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.finish()
    }
    let parser = make_parser_1().unwrap();

    assert_parse(&parser, "Expr", "1 2", "(_ 1 2)");
    assert_parse(&parser, "Expr", "1 2 3", "(_ (_ 1 2) 3)");
    assert_parse(&parser, "Expr", "1 2 + 3 4", "(plus (_ 1 2) (_ 3 4))");

    fn make_parser_2() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;
        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;
        grammar.lgroup();
        grammar.op("plus", pattern!(_ "+" _))?;
        grammar.rgroup();
        grammar.juxtapose()?;
        grammar.finish()
    }
    let parser = make_parser_2().unwrap();

    assert_parse(&parser, "Expr", "1 2", "(_ 1 2)");
    assert_parse(&parser, "Expr", "1 2 3", "(_ 1 (_ 2 3))");
    assert_parse(&parser, "Expr", "1 2 + 3 4", "(_ 1 (_ (plus 2 3) 4))");
}

/*
#[test]
fn test_haskell_list_comprehensions() {
    fn make_parser_1() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;

        grammar.sort("ListC");
        grammar.lgroup();
        grammar.op("listC", pattern!("[" Expr "|" Quals "]"))?;

        grammar.sort("Expr");
        grammar.regex("num", "[0-9]+")?;


        grammar.sort("Quals");
        grammar.lgroup();
        grammar.op("let", pattern!("let" Decls "," _))
        grammar.op("letFinal", pattern!("let" Decls))
        // TODO: This should be an expression, not just ids!
        grammar.regex("id", "[a-zA-Z]*")?;
        grammar.rgroup();
        grammar.op("comma", pattern!(_ "," _))?;

        grammar.sort("Expr");
        grammar.regex("id", "[a-zA-Z]*")?;
        grammar.finish()
    }
    let parser = make_parser_1().unwrap();
}

[  e | True ]	=	[e]
[  e | q ]	=	[  e | q, True ]
[  e | b,  Q  ]	=	if b then [  e | Q ] else []
[  e | p <- l,  Q ]	=	let ok p = [  e | Q ]
    ok _ = []
in concatMap ok  l
[  e | let decls,  Q ]	=	let decls in [  e | Q ]
*/
