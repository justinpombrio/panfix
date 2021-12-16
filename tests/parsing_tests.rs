mod parsing {
    use panfix::op;
    use panfix::parsing::{OpName, ParseTree, Parser, ParserBuilder, ParserBuilderError};

    fn run_parser<N: OpName>(parser: &Parser<N>, input: &str) -> String {
        match parser.parse(input) {
            Err(err) => format!("{}", err),
            Ok(parsed) => {
                let mut expr = String::new();
                let num_trees = parsed.trees().len();
                for (i, visitor) in parsed.trees().enumerate() {
                    parenthesize(&mut expr, visitor);
                    if i + 1 != num_trees {
                        expr.push(' ');
                    }
                }
                expr
            }
        }
    }

    fn parenthesize<N: OpName>(out: &mut String, tree: ParseTree<N>) {
        out.push('(');
        let num_children = tree.children().len();
        let mut at_start = true;
        let mut print_space = |out: &mut String| {
            if !at_start {
                out.push(' ');
            }
            at_start = false;
        };
        for (i, child) in tree.children().enumerate() {
            if let Some(token) = tree.token_before_child(i) {
                print_space(out);
                out.push_str(token);
            }
            if child.name() != N::MISSING_ATOM {
                print_space(out);
                parenthesize(out, child);
            }
            if i + 1 == num_children {
                if let Some(token) = tree.token_after_child(i) {
                    print_space(out);
                    out.push_str(token);
                }
            }
        }
        out.push(')');
    }

    #[test]
    fn test_left_associativity() {
        fn make_parser() -> Result<Parser<&'static str>, ParserBuilderError<&'static str>> {
            ParserBuilder::new("TestGrammar")
                .regex_literal("Var", "[a-zA-Z]+")?
                .subgrammar("Expr")?
                .assoc_l()?
                .op(op!(Plus: _ "+" _))?
                .op(op!(Minus: _ "-" _))?
                .op(op!(ListComprehension: _ "for" Expr "in" _))?
                .op(op!(Not: "!" _))?
                .op(op!(Lambda: "λ" Expr "." _))?
                .op(op!(Div100: _ "%"))?
                .op(op!(Subs: "[" Expr "]" _))?
                .op(op!(ForthDefn: _ ":" Expr ";"))?
                .finish()
        }
        let parser = make_parser().unwrap();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x"), "x");
        assert_eq!(parse("x+y+z"), "((x + y) + z)");
        assert_eq!(parse("a-b+c-d"), "(((a - b) + c) - d)");
        assert_eq!(parse("!x%"), "((! x) %)");
        assert_eq!(parse("!x+y%"), "(((! x) + y) %)");
        assert_eq!(parse("x%+!y"), "((x %) + (! y))");
        assert_eq!(parse("x for x in L"), "(x for x in L)");
        assert_eq!(
            parse("x for x in y for y in L"),
            "((x for x in y) for y in L)"
        );
        assert_eq!(parse("λx.y:z;"), "((λ x . y) : z ;)");
    }

    /*
    #[test]
    fn test_right_associativity() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder.ops_r(vec![
                    op!(And: _ "&&" _),
                    op!(Or: _ "||" _),
                    op!(Ternary: _ "?" Expr ":" _),
                    op!(Not: "!" _),
                    op!(Lambda: "λ" Expr "." _),
                    op!(Squared: _ "²"),
                    op!(ColonThingy: _ "::" Expr ";"),
                ])
            })
            .build("Expr");
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x&&y"), "(x && y)");
        assert_eq!(parse("x&&y||z"), "(x && (y || z))"); // look it's what the grammar says
        assert_eq!(parse("!x²"), "(! (x ²))");
        assert_eq!(parse("x²&&!y||!z²"), "((x ²) && (! (y || (! (z ²)))))");
        assert_eq!(parse("a?b:c"), "(a ? b : c)");
        assert_eq!(parse("a?b:c?d:e"), "(a ? b : (c ? d : e))");
        assert_eq!(parse("λx.x"), "(λ x . x)");
        assert_eq!(parse("λx.λy.x"), "(λ x . (λ y . x))");
        assert_eq!(parse("a::b::c;;"), "(a :: (b :: c ;) ;)");
        assert_eq!(parse("λx.y::z;"), "(λ x . (y :: z ;))");
        assert_eq!(parse("λx.a?b:c::z;"), "(λ x . (a ? b : (c :: z ;)))");
        assert_eq!(parse("λx.a?b::c;:z"), "(λ x . (a ? (b :: c ;) : z))");
    }

    #[test]
    fn test_infix_iter() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder.op_l(op!(Plus: _ "+" _)).op_r(op!(Comma: _ "," _))
            })
            .build("Expr");

        let source = "a + b, c + d + e, f";

        let parsed = parser.parse(source).unwrap();
        let visitor = parsed.groups().next().unwrap();
        let elems = visitor.iter_right("Comma").collect::<Vec<_>>();
        assert_eq!(elems.len(), 3);
        let first = elems[0].iter_left_vec("Plus");
        assert_eq!(first.len(), 2);
        assert_eq!(first[0].text(), "a");
        assert_eq!(first[1].text(), "b");
        let second = elems[1].iter_left_vec("Plus");
        assert_eq!(second.len(), 3);
        assert_eq!(second[0].text(), "c");
        assert_eq!(second[1].text(), "d");
        assert_eq!(second[2].text(), "e");
        let third = elems[2].iter_left_vec("Plus");
        assert_eq!(third.len(), 1);
        assert_eq!(third[0].text(), "f");
    }

    #[test]
    fn test_circumfix() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder
                    .op(op!(Parens: "(" Expr ")"))
                    .op_l(op!(Mul: _ "*" _))
                    .op_l(op!(Add: _ "+" _))
            })
            .build("Expr");
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a*b+c*d"), "((a * b) + (c * d))");
        assert_eq!(parse("a*(b+c)*d"), "((a * (( (b + c) ))) * d)");
    }

    #[test]
    fn test_variable_precedence() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder
                    .op(op!(Neg: "-" _))
                    .ops_l(vec![op!(Mul: _ "*" _), op!(Div: _ "/" _)])
                    .ops_l(vec![op!(Add: _ "+" _), op!(Sub: _ "-" _)])
            })
            .build("Expr");
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a*a-b*b"), "((a * a) - (b * b))");
        assert_eq!(parse("-a*a"), "((- a) * a)");
    }

    #[test]
    fn test_juxt_prec() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder
                    .op_l(op!(Mul: _ "*" _))
                    .op_l(juxtapose!())
                    .op_l(op!(Add: _ "+" _))
            })
            .build("Expr");
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a+b*c d*e+f"), "((a + ((b * c) ? (d * e))) + f)");
    }

    #[test]
    fn test_mixed_nonterminals() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Braces", |builder| {
                builder
                    .op(op!(Brace: "{" Brackets "}"))
                    .op_r(op!(Comma: _ "," _))
            })
            .subgrammar("Brackets", |builder| {
                builder
                    .op(op!(Bracket: "[" Braces "]"))
                    .op_r(op!(Semi: _ ";" _))
            })
            .build("Braces");
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("{a;b}"), "({ (a ; b) })");
        assert_eq!(parse("{[a]}"), "({ ([ a ]) })");
        assert_eq!(parse("{[a,{b}];c}"), "({ (([ (a , ({ b })) ]) ; c) })");
    }

    fn parse_c(input: &str) -> String {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .regex("Number", "0|[1-9][0-9]*")
            .constant("True", "true")
            .constant("False", "false")
            .subgrammar("Expr", |builder| {
                builder.ops_l(vec![
                    op!(Inc: _ "++" _),
                    op!(Subs: _ "[" Expr "]"),
                    op!(Dot: _ "." _),
                ])
            })
            .build("Expr");
        run_parser(&parser, input)
    }

    #[test]
    fn test_errors() {
        assert_eq!(parse_c("x$y"), "LexErr $ 0:2");
        assert_eq!(parse_c("x.y]33"), "ExtraSep ] 0:4");
        assert_eq!(parse_c("x.y[x.y"), "MissingSep Subs ] 0:4");
    }

    #[test]
    fn test_c_basics() {
        assert_eq!(parse_c(" x "), "x");
        assert_eq!(parse_c("1"), "1");
        assert_eq!(parse_c("44"), "44");
        assert_eq!(parse_c("x++"), "(x ++)");
        assert_eq!(parse_c("++x"), "(++ x)");
        assert_eq!(parse_c("x.y"), "(x . y)");
        assert_eq!(parse_c("x[1]"), "(x [ 1 ])");
        assert_eq!(parse_c("x.y.z"), "((x . y) . z)");
        assert_eq!(parse_c("x[y][z]"), "((x [ y ]) [ z ])");
        assert_eq!(parse_c("x[y.z]"), "(x [ (y . z) ])");
        assert_eq!(parse_c("x.y[1]"), "((x . y) [ 1 ])");
    }

    #[test]
    #[should_panic(expected = "The first token of operator Dot")]
    fn test_bug_1_err() {
        Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Expr", |builder| {
                builder
                    .op_l(op!(Dot: _ "." _))
                    .op_r(op!(Comma: _ "," _))
                    .op(op!(Lambda: "/" Expr "." _))
            })
            .build("Expr");
    }

    #[test]
    #[should_panic(expected = "No such subgrammar: Nonsense")]
    fn test_no_such_nonterminal() {
        Grammar::new("TestGrammar")
            .subgrammar("Expr", |builder| builder.op(op!(Parens: "(" Nonsense ")")))
            .build("Expr");
    }

    #[test]
    fn test_bug_1_ok() {
        let parser = Grammar::new("TestGrammar")
            .regex("Var", "[a-zA-Z]+")
            .subgrammar("Args", |builder| builder.op_r(op!(Comma: _ "," _)))
            .subgrammar("Expr", |builder| {
                builder
                    .op_l(op!(Dot: _ "." _))
                    .op(op!(Lambda: "/" Args "." _))
            })
            .build("Expr");
        let parse = |input: &str| run_parser(&parser, input);
        assert_eq!(parse("x.y"), "(x . y)");
        assert_eq!(parse("/x.y"), "(/ x . y)");
        assert_eq!(parse("/x,y.x"), "(/ (x , y) . x)");
    }
    */
}
