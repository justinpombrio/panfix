mod shunting {
    use panfix::shunting::{Assoc, Fixity, Grammar, Lexeme, OpSpec, Prec, ShuntError, Token};

    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct CharToken(char);

    impl Token for CharToken {
        const LEX_ERROR: CharToken = CharToken('L');

        fn as_usize(self) -> usize {
            self.0 as usize
        }
    }

    fn lex(source: &str) -> impl Iterator<Item = Lexeme<CharToken>> + '_ {
        source.chars().enumerate().map(|(i, ch)| Lexeme {
            token: CharToken(ch),
            span: (i, i + 1),
        })
    }

    fn grammar() -> Grammar<CharToken> {
        use Assoc::{Left, Right};
        use Fixity::*;

        fn nilfix(name: &str, tokens: &str) -> OpSpec<CharToken> {
            op(name, tokens, Nilfix, Left, 0)
        }
        fn op(
            name: &str,
            tokens: &str,
            fixity: Fixity,
            assoc: Assoc,
            prec: Prec,
        ) -> OpSpec<CharToken> {
            let (first, followers) = to_followers(tokens);
            OpSpec {
                nonterminal: "Expr".to_owned(),
                name: name.to_owned(),
                fixity,
                assoc,
                first_token: Some(first),
                followers,
                prec,
            }
        }
        fn juxtapose(prec: Prec) -> OpSpec<CharToken> {
            OpSpec::juxtapose("Expr", prec)
        }
        fn to_followers(tokens_str: &str) -> (CharToken, Vec<(String, CharToken)>) {
            let mut chars = tokens_str.chars();
            let first = CharToken(chars.next().unwrap());
            let followers = chars.map(|c| ("Expr".to_owned(), CharToken(c))).collect();
            (first, followers)
        }

        Grammar::new(
            "ShuntingTesting".to_owned(),
            "Expr".to_owned(),
            vec![
                nilfix("1", "1"),
                nilfix("2", "2"),
                nilfix("3", "3"),
                nilfix("b", "{}"),
                op("-", "-", Prefix, Left, 1),
                op("!", "!", Suffix, Left, 1),
                op("*", "*", Infix, Right, 2),
                op("?", "?:", Infix, Right, 3),
                juxtapose(3),
                op("@", "()", Suffix, Right, 3),
                op("+", "+", Infix, Left, 4),
                op("^", "^", Prefix, Left, 5),
                op("s", "[/]", Prefix, Left, 5),
            ],
        )
    }

    fn shunt(source: &str) -> String {
        let grammar = grammar();
        let lexemes = lex(source);
        let rpn = grammar.shunt(lexemes);
        let mut output = String::new();
        for node in rpn {
            match node {
                Err(error) => {
                    let ch = match error {
                        ShuntError::LexError(_) => 'L',
                        ShuntError::ExtraSep(_) => 'X',
                        ShuntError::MissingSep { .. } => 'S',
                    };
                    output.push(ch);
                    break;
                }
                Ok(node) => {
                    let ch = match node.op.name() {
                        "$Juxtapose" => 'J',
                        "$MissingAtom" => 'M',
                        _ => node.op.first_token().unwrap().0,
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
