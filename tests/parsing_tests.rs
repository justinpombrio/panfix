mod common;

#[cfg(test)]
mod parsing_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, WHITESPACE_REGEX};
    use panfix::{circumfix, infix, juxtapose, prefix, suffix};

    #[test]
    fn test_left_associativity() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .ops_l(vec![
                infix!("Plus", "+"),
                infix!("Minus", "-"),
                infix!("ListComprehension", "for", "in"),
                prefix!("Not", "!"),
                prefix!("Lambda", "λ", "."),
                suffix!("Div100", "%"),
                suffix!("Subs", "[", "]"),
                suffix!("ForthDefn", ":", ";"),
            ])
            .build();
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

    #[test]
    fn test_right_associativity() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .ops_r(vec![
                infix!("And", "&&"),
                infix!("Or", "||"),
                infix!("Ternary", "?", ":"),
                prefix!("Not", "!"),
                prefix!("Lambda", "λ", "."),
                suffix!("Squared", "²"),
                suffix!("ColonThingy", ":", ";"),
            ])
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x&&y"), "(x && y)");
        assert_eq!(parse("x&&y||z"), "(x && (y || z))"); // look it's what the grammar says
        assert_eq!(parse("!x²"), "(! (x ²))");
        assert_eq!(parse("x²&&!y||!z²"), "((x ²) && (! (y || (! (z ²)))))");
        assert_eq!(parse("a?b:c"), "(a ? b : c)");
        assert_eq!(parse("a?b:c?d:e"), "(a ? b : (c ? d : e))");
        assert_eq!(parse("λx.x"), "(λ x . x)");
        assert_eq!(parse("λx.λy.x"), "(λ x . (λ y . x))");
        assert_eq!(parse("a:b:c;;"), "(a : (b : c ;) ;)");
        assert_eq!(parse("λx.y:z;"), "(λ x . (y : z ;))");
        assert_eq!(parse("λx.a?b:c:z;"), "(λ x . (a ? b : (c : z ;)))");
    }

    #[test]
    fn test_circumfix() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .op(circumfix!("Parens", "(", ")"))
            .op_l(infix!("Mul", "*"))
            .op_l(infix!("Add", "+"))
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a*b+c*d"), "((a * b) + (c * d))");
        assert_eq!(parse("a*(b+c)*d"), "((a * (( (b + c) ))) * d)");
    }

    #[test]
    fn test_variable_precedence() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .op_l(prefix!("Neg", "-"))
            .ops_l(vec![infix!("Mul", "*"), infix!("Div", "/")])
            .ops_l(vec![infix!("Add", "+"), infix!("Sub", "-")])
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a*a-b*b"), "((a * a) - (b * b))");
        assert_eq!(parse("-a*a"), "((- a) * a)");
    }

    #[test]
    fn test_juxt_prec() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .op_l(infix!("Mul", "*"))
            .op_l(juxtapose!())
            .op_l(infix!("Add", "+"))
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("a+b*c d*e+f"), "((a + ((b * c) ? (d * e))) + f)");
    }

    fn parse_c(input: &str) -> String {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .regex("Number", "0|[1-9][0-9]*")
            .constant("True", "true")
            .constant("False", "false")
            .ops_l(vec![
                infix!("Inc", "++"),
                suffix!("Subs", "[", "]"),
                infix!("Dot", "."),
            ])
            .build();
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
}
