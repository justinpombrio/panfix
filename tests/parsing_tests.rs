use panfix::{pattern, Grammar, GrammarError, Parser, Visitor};

#[track_caller]
fn assert_parse(grammar: &Parser, sort: &str, src: &str, expected: &str) {
    let tree = grammar.parse(sort, src).unwrap();
    let sexpr = to_sexpr(tree.visitor());
    assert_eq!(sexpr, expected);
}

fn to_sexpr(visitor: Visitor) -> String {
    if visitor.num_children() == 0 {
        visitor.source().to_string()
    } else {
        let mut sexpr = "(".to_string();
        sexpr.push_str(visitor.op());
        for child in visitor.children() {
            sexpr.push_str(" ");
            sexpr.push_str(&to_sexpr(child));
        }
        sexpr.push_str(")");
        sexpr
    }
}

#[test]
fn test_parsing_minus() {
    fn make_parser() -> Result<Parser, GrammarError> {
        let mut grammar = Grammar::new_with_unicode_whitespace()?;

        grammar.sort("Expr");
        grammar.regex_atom("Number", "[0-9]+")?;
        grammar.lgroup();
        grammar.op("neg", pattern!("-" _))?;
        grammar.lgroup();
        grammar.op("minus", pattern!(_ "-" _))?;
        grammar.finish()
    }

    let parser = make_parser().unwrap();

    assert_parse(&parser, "Expr", "- 2", "(neg 2)");
    assert_parse(&parser, "Expr", "--2", "(neg (neg 2))");
    //assert_parse(&parser, "Expr", "1 - 2 - 3", "(minus (minus 1 2) 3)");
}
