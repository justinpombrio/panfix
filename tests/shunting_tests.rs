#[cfg(test)]
mod shunting_tests {
    use panfix::shunting::{Lexeme, ShuntError, Shunter, ShunterBuilder, Token};

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
        ShunterBuilder::new()
            .nilfix("1", CharToken('1'))
            .nilfix("2", CharToken('2'))
            .nilfix("3", CharToken('3'))
            .prefix("-", CharToken('-'), 20)
            .prefix("^", CharToken('^'), 80)
            .suffix("!", CharToken('!'), 20)
            .infixl("+", CharToken('+'), 60)
            .infixr("*", CharToken('*'), 40)
            // Suffix mixfix
            .mixfix("@", Some(50), None, vec![CharToken('('), CharToken(')')])
            // Prefix mixfix
            .mixfix(
                "s",
                None,
                Some(80),
                vec![CharToken('['), CharToken('/'), CharToken(']')],
            )
            // Circumfix mixfix
            .mixfix("b", None, None, vec![CharToken('{'), CharToken('}')])
            // Infix mixfix
            .mixfix(
                "?",
                Some(50),
                Some(51),
                vec![CharToken('?'), CharToken(':')],
            )
            .juxtapose_prec(50, 50)
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
                    let ch = match node.rule.name.as_ref() {
                        "$Juxtapose" => 'J',
                        "$MissingAtom" => 'M',
                        _ => node.rule.tokens[0].0,
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
    fn test_missing_atom() {
        assert_eq!(shunt(""), "M");
        assert_eq!(shunt("+"), "MM+");
        assert_eq!(shunt("123"), "12J3J");
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
