mod common;

#[cfg(test)]
mod parsing_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, WHITESPACE_REGEX};
    use panfix::{infix, prefix, suffix};

    #[test]
    fn test_left_associativity() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .rules_l(vec![
                infix!("Plus", "+"),
                infix!("Minus", "-"),
                prefix!("Not", "!"),
                suffix!("Div100", "%"),
                suffix!("Subs", "[", "]"),
            ])
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x"), "x");
        assert_eq!(parse("x+y+z"), "((x + y) + z)");
        assert_eq!(parse("a-b+c-d"), "(((a - b) + c) - d)");
        assert_eq!(parse("!x%"), "((! x) %)");
        assert_eq!(parse("!x+y%"), "(((! x) + y) %)");
        assert_eq!(parse("x%+!y"), "((x %) + (! y))");
    }

    #[test]
    fn test_right_associativity() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .rules_r(vec![
                infix!("And", "&&"),
                infix!("Or", "||"),
                infix!("Ternary", "?", ":"),
                prefix!("Not", "!"),
                prefix!("Lambda", "λ", "->"),
                suffix!("Squared", "²"),
                suffix!("ForthDefn", ":", ";"),
            ])
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x&&y"), "(x && y)");
        assert_eq!(parse("x&&y||z"), "(x && (y || z))"); // look it's what the grammar says
        assert_eq!(parse("!x²"), "(! (x ²))");
        assert_eq!(parse("x²&&!y||!z²"), "((x ²) && (! (y || (! (z ²)))))");
        assert_eq!(parse("a?b:c"), "(a ? b : c)");
        assert_eq!(parse("a?b:c?d:e"), "(a ? b : (c ? d : e))");
        assert_eq!(parse("λx->x"), "(λ x -> x)");
        assert_eq!(parse("λx->λy->x"), "(λ x -> (λ y -> x))");
        assert_eq!(parse("a:b:c;;"), "(a : (b : c ;) ;)");
        assert_eq!(parse("λx->y:z;"), "(λ x -> (y : z ;))"); // an unholy combination
        assert_eq!(parse("λx->a?b:c:z;"), "(λ x -> (a ? b : (c : z ;)))");
    }

    fn parse_c(input: &str) -> String {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .regex("Number", "0|[1-9][0-9]*")
            .constant("True", "true")
            .constant("False", "false")
            .rules_l(vec![
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
