use panfix::implementation::shunter::shunt;
use panfix::{Col, Lexeme, Position, Prec, TokenId, TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE};

#[test]
fn test_shunting() {
    const TOKEN_ID: TokenId = 3;
    const TOKEN_TIMES: TokenId = 4;
    const TOKEN_PLUS: TokenId = 5;
    const TOKEN_NEG: TokenId = 6;
    const TOKEN_MINUS: TokenId = 7;
    const TOKEN_BANG: TokenId = 8;
    const TOKEN_OPEN: TokenId = 9;
    const TOKEN_CLOSE: TokenId = 10;
    const TOKEN_QMARK: TokenId = 11;
    const TOKEN_COLON: TokenId = 12;
    const NUM_TOKENS: usize = 13;

    fn lex(src: &str) -> impl Iterator<Item = Lexeme> {
        let mut lexemes = vec![];
        for i in 0..src.len() {
            let ch = src[i..i + 1].chars().next().unwrap();
            if ch == ' ' {
                continue;
            }
            let token = match ch {
                '_' => TOKEN_BLANK,
                '.' => TOKEN_JUXTAPOSE,
                'a'..='z' => TOKEN_ID,
                '*' => TOKEN_TIMES,
                '+' => TOKEN_PLUS,
                '~' => TOKEN_NEG,
                '-' => TOKEN_MINUS,
                '!' => TOKEN_BANG,
                '(' => TOKEN_OPEN,
                ')' => TOKEN_CLOSE,
                '?' => TOKEN_QMARK,
                ':' => TOKEN_COLON,
                _ => TOKEN_ERROR,
            };
            let start = Position {
                line: 0,
                col: i as Col,
                utf8_col: i as Col,
            };
            let end = Position {
                line: 0,
                col: (i + 1) as Col,
                utf8_col: (i + 1) as Col,
            };
            lexemes.push(Lexeme::new(token, start, end));
        }
        lexemes.into_iter()
    }

    fn show_stream(src: &str, stream: impl Iterator<Item = Lexeme>) -> String {
        stream
            .map(|lex| &src[lex.span.start.col as usize..lex.span.end.col as usize])
            .map(|lex| if lex.is_empty() { "_" } else { lex })
            .collect::<Vec<_>>()
            .join(" ")
    }

    let mut prec_table = Vec::new();
    for _ in 0..NUM_TOKENS {
        prec_table.push((0, 0));
    }
    prec_table[TOKEN_ERROR] = (0, 0);
    prec_table[TOKEN_BLANK] = (0, 0);
    prec_table[TOKEN_JUXTAPOSE] = (10, 10);
    prec_table[TOKEN_ID] = (0, 0);
    prec_table[TOKEN_BANG] = (50, 0);
    prec_table[TOKEN_TIMES] = (60, 60);
    prec_table[TOKEN_PLUS] = (100, 99);
    prec_table[TOKEN_MINUS] = (100, 99);
    prec_table[TOKEN_NEG] = (0, 80);
    prec_table[TOKEN_OPEN] = (0, Prec::MAX);
    prec_table[TOKEN_CLOSE] = (Prec::MAX, 0);
    prec_table[TOKEN_QMARK] = (80, Prec::MAX);
    prec_table[TOKEN_COLON] = (Prec::MAX, 80);

    let mut link_table = Vec::new();
    for _ in 0..NUM_TOKENS {
        link_table.push(vec![false; NUM_TOKENS]);
    }

    let src = "_";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_");

    let src = "_+_";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_ _ +");

    let src = "1-2+3*4*5!-~6";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 2 - 3 4 5 ! * * + 6 ~ -");

    let src = "(~_)";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_ ~ ) (");

    let src = "%";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "%");

    let src = "1 + %";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 % +");

    let src = "1 ? 2 : 3";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 2 3 : ?");

    // TODO: This is not great behavior
    let src = "( 2 : 3";
    let lexemes = shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "2 3 : (");
}
