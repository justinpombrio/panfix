use panfix::{pattern, Grammar, Parser, Visitor};

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
    let mut grammar = Grammar::new_with_unicode_whitespace().unwrap();

    grammar.sort("Expr");
    grammar.regex_atom("Number", "[0-9]+").unwrap();
    grammar.group();
    grammar.op("neg", pattern!("-" _)).unwrap();
    grammar.group();
    grammar.op("minus", pattern!(_ "-" _)).unwrap();

    let parser = grammar.finish().unwrap();
    assert_parse(&parser, "Expr", "- 2", "(neg 2)");
    assert_parse(&parser, "Expr", "--2", "(neg (neg 2))");
    //assert_parse(&parser, "Expr", "1 - 2 - 3", "(minus (minus 1 2) 3)");
}
