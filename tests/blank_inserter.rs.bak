use panfix::implementation::blank_inserter::insert_blanks;
use panfix::{Lexeme, Position, Token, TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE};

#[test]
fn test_blank_insertion() {
    const TOKEN_ID: Token = 3;
    const TOKEN_TIMES: Token = 4;
    const TOKEN_PLUS: Token = 5;
    const TOKEN_NEG: Token = 6;
    const TOKEN_MINUS: Token = 7;
    const TOKEN_BANG: Token = 8;
    const TOKEN_OPEN: Token = 9;
    const TOKEN_CLOSE: Token = 10;
    const NUM_TOKENS: usize = 11;

    fn lex(src: &str) -> Vec<Lexeme> {
        let mut lexemes = vec![];
        let mut pos = Position::start();
        for i in 0..src.len() {
            let ch = src[i..i + 1].chars().next().unwrap();
            if ch == ' ' {
                pos = pos.advance_by_char(' ');
                continue;
            }
            let token = match ch {
                'a'..='z' => TOKEN_ID,
                '*' => TOKEN_TIMES,
                '+' => TOKEN_PLUS,
                '-' => TOKEN_NEG,
                '!' => TOKEN_BANG,
                '(' => TOKEN_OPEN,
                ')' => TOKEN_CLOSE,
                _ => TOKEN_ERROR,
            };
            let start_pos = pos;
            pos = pos.advance_by_char(ch);
            let end_pos = pos;
            lexemes.push(Lexeme::new(token, start_pos, end_pos));
        }
        lexemes
    }

    #[track_caller]
    fn assert_lexeme(
        src: &str,
        stream: &mut impl Iterator<Item = Lexeme>,
        expected: &str,
        token: Token,
    ) {
        let lex = stream
            .next()
            .expect("Token stream in test case ended early");
        assert_eq!(lex.token, token);
        let start = lex.span.start;
        let end = lex.span.end;
        // assuming single line input
        let start_pos = lex.span.start.col as usize;
        let end_pos = lex.span.end.col as usize;
        let lexeme = &src[start_pos..end_pos];
        let actual = format!(
            "{}:{}-{}:{} {}",
            start.line, start.col, end.line, end.col, lexeme
        );
        assert_eq!(actual, expected);
    }

    let mut prefixy_tokens = Vec::new();
    let mut suffixy_tokens = Vec::new();
    for _ in 0..NUM_TOKENS {
        prefixy_tokens.push(None);
        suffixy_tokens.push(None);
    }
    prefixy_tokens[TOKEN_ERROR] = Some((TOKEN_ERROR, false));
    prefixy_tokens[TOKEN_ID] = Some((TOKEN_ID, false));
    suffixy_tokens[TOKEN_PLUS] = Some((TOKEN_PLUS, true));
    prefixy_tokens[TOKEN_NEG] = Some((TOKEN_NEG, true));
    suffixy_tokens[TOKEN_NEG] = Some((TOKEN_MINUS, true));
    suffixy_tokens[TOKEN_BANG] = Some((TOKEN_BANG, false));
    prefixy_tokens[TOKEN_OPEN] = Some((TOKEN_OPEN, true));
    suffixy_tokens[TOKEN_CLOSE] = Some((TOKEN_CLOSE, false));

    let src = " a";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:1-0:2 a", TOKEN_ID);
    assert!(lexemes.next().is_none());

    let src = "";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:0-0:0 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:0-0:1 -", TOKEN_NEG);
    assert_lexeme(src, lexemes, "0:1-0:1 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-o o-";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:0-0:1 -", TOKEN_NEG);
    assert_lexeme(src, lexemes, "0:1-0:2 o", TOKEN_ID);
    assert_lexeme(src, lexemes, "0:2-0:2 ", TOKEN_JUXTAPOSE);
    assert_lexeme(src, lexemes, "0:3-0:4 o", TOKEN_ID);
    assert_lexeme(src, lexemes, "0:4-0:5 -", TOKEN_MINUS);
    assert_lexeme(src, lexemes, "0:5-0:5 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "(x)";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:0-0:1 (", TOKEN_OPEN);
    assert_lexeme(src, lexemes, "0:1-0:2 x", TOKEN_ID);
    assert_lexeme(src, lexemes, "0:2-0:3 )", TOKEN_CLOSE);
    assert!(lexemes.next().is_none());

    let src = "%";
    let lexemes = &mut insert_blanks(&prefixy_tokens, &suffixy_tokens, lex(src)).into_iter();
    assert_lexeme(src, lexemes, "0:0-0:1 %", TOKEN_ERROR);
    assert!(lexemes.next().is_none());
}
