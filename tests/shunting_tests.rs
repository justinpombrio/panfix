mod shunting {
    use panfix::shunting::{Lexeme, OpSpec, ShuntError, Shunter, ShunterBuilder, Token};

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

    fn grammar() -> Shunter<CharToken> {
        fn nilfix(name: &str, tokens: &str) -> OpSpec<CharToken> {
            OpSpec::nilfix(name.to_owned(), to_tokens(tokens))
        }
        fn prefix(name: &str, tokens: &str) -> OpSpec<CharToken> {
            OpSpec::prefix(name.to_owned(), to_tokens(tokens))
        }
        fn suffix(name: &str, tokens: &str) -> OpSpec<CharToken> {
            OpSpec::suffix(name.to_owned(), to_tokens(tokens))
        }
        fn infix(name: &str, tokens: &str) -> OpSpec<CharToken> {
            OpSpec::infix(name.to_owned(), to_tokens(tokens))
        }
        fn to_tokens(tokens_str: &str) -> Vec<CharToken> {
            tokens_str.chars().map(CharToken).collect()
        }

        ShunterBuilder::new()
            .op(nilfix("1", "1"))
            .op(nilfix("2", "2"))
            .op(nilfix("3", "3"))
            .op(nilfix("b", "{}"))
            .ops_l(vec![prefix("-", "-"), suffix("!", "!")])
            .op_r(infix("*", "*"))
            .ops_r(vec![
                infix("?", "?:"),
                OpSpec::juxtapose(),
                suffix("@", "()"),
            ])
            .op_l(infix("+", "+"))
            .ops_r(vec![prefix("^", "^"), prefix("s", "[/]")])
            .build()
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
                        _ => node.op.tokens()[0].0,
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
