mod shunting {
    use panfix::shunting::{
        Assoc, Fixity, Grammar, GrammarBuilder, GrammarBuilderError, Lexeme, Prec, ShuntError,
    };

    fn grammar() -> Result<Grammar<&'static str>, GrammarBuilderError<&'static str>> {
        use Assoc::{Left, Right};
        use Fixity::*;

        // Notice that we're converting from `char` to `usize` (and back, elsewhere).  This is very
        // fragile, and only works for ascii chars! However it is convenient for testing, so that
        // we don't have to keep a separate mapping between token ids and tokens.
        fn add_op(
            builder: GrammarBuilder<&'static str>,
            name: &'static str,
            prec: Prec,
            assoc: Assoc,
            fixity: Fixity,
            tokens: &str,
        ) -> Result<GrammarBuilder<&'static str>, GrammarBuilderError<&'static str>> {
            let mut tokens = tokens.chars();
            let first_token = tokens.next().unwrap() as usize;
            let followers = tokens.map(|tok| ("Expr", tok as usize)).collect::<Vec<_>>();
            builder.op("Expr", name, prec, assoc, fixity, first_token, followers)
        }

        let builder = GrammarBuilder::new("ShuntingTests");
        let builder = add_op(builder, "1", 0, Left, Nilfix, "1")?;
        let builder = add_op(builder, "2", 0, Left, Nilfix, "2")?;
        let builder = add_op(builder, "3", 0, Left, Nilfix, "3")?;
        let builder = add_op(builder, "b", 0, Left, Nilfix, "{}")?;
        let builder = add_op(builder, "-", 1, Left, Prefix, "-")?;
        let builder = add_op(builder, "!", 1, Left, Suffix, "!")?;
        let builder = add_op(builder, "*", 2, Right, Infix, "*")?;
        let builder = add_op(builder, "?", 3, Right, Infix, "?:")?;
        let builder = builder.op_juxtapose("Expr", 3)?;
        let builder = add_op(builder, "@", 3, Right, Suffix, "()")?;
        let builder = add_op(builder, "+", 4, Left, Infix, "+")?;
        let builder = add_op(builder, "^", 5, Left, Prefix, "^")?;
        let builder = add_op(builder, "s", 5, Left, Prefix, "[/]")?;
        let grammar = builder.finish("Expr")?;

        Ok(grammar)
    }

    fn lex(source: &str) -> impl Iterator<Item = Lexeme> + '_ {
        source.chars().enumerate().map(|(i, ch)| Lexeme {
            token: ch as usize,
            span: (i, i + 1),
        })
    }

    fn shunt(source: &str) -> String {
        let grammar = grammar().unwrap();
        let lexemes = lex(source);
        let rpn = grammar.shunt(lexemes);
        let mut output = String::new();
        for node in rpn {
            match node {
                Err(error) => {
                    let ch = match error {
                        ShuntError::LexError(_) => 'L',
                        ShuntError::UnexpectedToken(_) => 'X',
                        ShuntError::MissingFollower { .. } => 'S',
                    };
                    output.push(ch);
                    break;
                }
                Ok(node) => {
                    let ch = match node.op().name() {
                        "JUXTAPOSE" => 'J',
                        "MISSING_ATOM" => 'M',
                        _ => node.op().first_token().unwrap() as u8 as char,
                    };
                    output.push(ch);
                }
            }
        }
        output
    }

    #[test]
    fn test_basics() {
        assert_eq!(shunt("1+2"), "12+");
        assert_eq!(shunt("1+2+3"), "12+3+");
        assert_eq!(shunt("1*2*3"), "123**");
    }

    #[test]
    fn test_lex_error() {
        assert_eq!(shunt("X3"), "MX");
        assert_eq!(shunt("3X"), "3X");
    }

    #[test]
    fn test_missing() {
        assert_eq!(shunt(""), "M");
        assert_eq!(shunt("+"), "MM+");
        assert_eq!(shunt("123"), "123JJ");
    }

    #[test]
    fn test_prefix_and_suffix() {
        assert_eq!(shunt("-3"), "3-");
        assert_eq!(shunt("3!"), "3!");
        assert_eq!(shunt("-3!"), "3-!");
    }

    #[test]
    fn test_complicated() {
        assert_eq!(shunt("--+^1-2"), "M--12-J^+",);
        assert_eq!(shunt("!*^!3"), "M!M!3J^*",);
        assert_eq!(shunt("--+^1-2!*^!3"), "M--12-!M!3J^*J^+");
    }

    #[test]
    fn test_missing_sep() {
        assert_eq!(shunt("1(2"), "12S");
    }

    #[test]
    fn test_extra_sep() {
        assert_eq!(shunt(")33"), "MX");
        assert_eq!(shunt("1(2))"), "12(X");
    }

    #[test]
    fn test_mixfix_suffix() {
        assert_eq!(shunt("1(2)"), "12(");
        assert_eq!(shunt("1(2)(3)"), "12(3(");
        assert_eq!(shunt("1(2(3))"), "123((");
        assert_eq!(shunt("(2)"), "M2(");
        assert_eq!(shunt("1()"), "1M(");
        assert_eq!(shunt("-1+-1(2*3)(2+2)"), "1-1-23*(22+(+");
        assert_eq!(shunt("1*2(3)"), "12*3(");
    }

    #[test]
    fn test_mixfix_prefix() {
        assert_eq!(shunt("[1/2]3"), "123[");
        assert_eq!(shunt("[1+2/2+3]1+2"), "12+23+12+[");
    }

    #[test]
    fn test_circumfix() {
        assert_eq!(shunt("{1}"), "1{");
        assert_eq!(shunt("{1+2}"), "12+{");
        assert_eq!(shunt("3*{1+2}"), "312+{*");
        assert_eq!(shunt("{1+2}*3"), "12+{3*");
        assert_eq!(shunt("{1+2}*{2+3}"), "12+{23+{*");
    }

    #[test]
    fn test_mixfix_infix() {
        assert_eq!(shunt("1?2:3"), "123?");
        assert_eq!(shunt("1?1:2?2:2"), "11222??");
        assert_eq!(
            shunt("1+2*3?1+2*3:2*3?1+2*3:1+2*3"),
            "123*123*+23*123*+1??+23*+"
        );
    }

    #[test]
    fn test_mixed_mixfix() {
        assert_eq!(shunt("{1(2)}?2+3:[1/2]3"), "12({23+123[?");
    }
}
