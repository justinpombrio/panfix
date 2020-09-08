mod common;

#[cfg(test)]
mod parsing_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, WHITESPACE_REGEX};
    use panfix::{infixl, infixr, prefix, suffix};

    #[test]
    fn test_associativity() {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .rule_group(vec![
                infixl!("Plus", "+"),
                infixl!("Minus", "-"),
                prefix!("Not", "!"),
                suffix!("Div100", "%"),
            ])
            .build();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("x"), "x");
        assert_eq!(parse("x+y+z"), "((x + y) + z)");
    }

    fn parse_c(input: &str) -> String {
        let parser = Grammar::new(WHITESPACE_REGEX)
            .regex("Var", "[a-zA-Z]+")
            .regex("Number", "0|[1-9][0-9]*")
            .constant("True", "true")
            .constant("False", "false")
            .rule_group(vec![
                infixl!("Inc", "++"),
                suffix!("Subs", "[", "]"),
                infixl!("Dot", "."),
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
