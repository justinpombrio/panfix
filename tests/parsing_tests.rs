use panfix::{pattern, Grammar, GrammarBuilder, Visitor};

#[track_caller]
fn assert_parse(grammar: &Grammar, sort: &str, src: &str, expected: &str) {
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
    let mut builder = GrammarBuilder::new_with_unicode_whitespace().unwrap();

    builder.atom_regex("Expr", "Number", "[0-9]+").unwrap();
    builder.op("Expr", "minus", 40, pattern!(_ "-" _)).unwrap();
    builder.op("Expr", "neg", 50, pattern!("-" _)).unwrap();

    let grammar = builder.finish().unwrap();
    assert_parse(&grammar, "Expr", "- 2", "(neg 2)");
    assert_parse(&grammar, "Expr", "--2", "(neg (neg 2))");
    //assert_parse(&grammar, "Expr", "1 - 2 - 3", "(minus (minus 1 2) 3)");
}
