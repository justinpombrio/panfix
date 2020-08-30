use panfix::shunter::{Grammar, GrammarBuilder, Lexeme, Token};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CharToken(char);

impl Token for CharToken {
    const LEX_ERROR: CharToken = CharToken('E');
    const MISSING: CharToken = CharToken('J');
    const JUXTAPOSE: CharToken = CharToken('M');

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
    GrammarBuilder::new()
        .nilfix("1", CharToken('1'))
        .nilfix("2", CharToken('2'))
        .nilfix("3", CharToken('3'))
        .prefix("-", CharToken('-'), 20)
        .prefix("@", CharToken('@'), 80)
        .suffix("!", CharToken('!'), 20)
        .infixl("+", CharToken('+'), 60)
        .infixr("*", CharToken('*'), 40)
        .nilfix("Missing", CharToken('M'))
        .infixl("Juxtapose", CharToken('J'), 50)
        .build()
}

fn shunt(source: &str) -> String {
    let grammar = grammar();
    let lexemes = lex(source);
    let rpn = grammar.shunt(lexemes);
    rpn.map(|node| node.op.tokens[0].0).collect::<String>()
}

#[test]
fn test_shunt_normal() {
    //assert_eq!(shunt("!"), "wrong");
    assert_eq!(shunt("1+2"), "12+");
    assert_eq!(shunt("1+2+3"), "12+3+");
    assert_eq!(shunt("1*2*3"), "123**");
}

#[test]
fn test_shunt_missing() {
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
    assert_eq!(shunt("--+@1-2"), "M--12-J@+",);
    assert_eq!(shunt("!*@!3"), "M!M!3J@*",);
    assert_eq!(shunt("--+@1-2!*@!3"), "M--12-!M!3J@*J@+");
}
