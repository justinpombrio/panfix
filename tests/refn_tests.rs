mod refn {
    use panfix::refn_impl::{parse, Grammar, GrammarBuilder, ParseTree};
    use panfix::rule;

    fn write_sexpr(out: &mut String, tree: &ParseTree) {
        if tree.arity() == 0 {
            if tree.name() == "$MissingAtom" {
                out.push('?');
            } else {
                out.push_str(tree.text())
            }
        } else {
            out.push('(');
            if tree.name() == "$Juxtapose" {
                out.push('J');
            } else {
                out.push_str(tree.name());
            }
            for child in tree.children() {
                out.push(' ');
                write_sexpr(out, child);
            }
            out.push(')');
        }
    }

    fn run_parser(g: &Grammar, source: &str) -> String {
        match parse(g, source) {
            Err(err) => format!("{}", err),
            Ok(tree) => {
                let mut string = String::new();
                write_sexpr(&mut string, &tree);
                string
            }
        }
    }

    #[test]
    fn test_basics() {
        let g = GrammarBuilder::new()
            .subgrammar("main", |builder| {
                builder
                    .regex("Var", "[a-zA-Z]+")
                    .ops_r(vec![rule!(Dot: _ "." _)])
                    .ops_l(vec![rule!(Plus: _ "+" _), rule!(Minus: _ "-" _)])
            })
            .build();

        assert_eq!(run_parser(&g, "xyz"), "xyz");
        assert_eq!(run_parser(&g, "x+ y"), "(Plus x y)");
        assert_eq!(run_parser(&g, "x y z"), "(J x (J y z))");
        assert_eq!(run_parser(&g, "x.y.z"), "(Dot x (Dot y z))");
        assert_eq!(run_parser(&g, "x-y+y-z"), "(Minus (Plus (Minus x y) y) z)");
        assert_eq!(run_parser(&g, "p.x+p.y"), "(Plus (Dot p x) (Dot p y))");
    }
}
