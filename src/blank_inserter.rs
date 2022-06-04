use crate::{Lexeme, Position, Token, TOKEN_BLANK, TOKEN_JUXTAPOSE};
use std::iter;

pub struct BlankInserter {
    pub prefixy_tokens: Vec<Option<(Token, bool)>>,
    pub suffixy_tokens: Vec<Option<(Token, bool)>>,
}

impl BlankInserter {
    pub fn transform<'h, 's: 'h, I>(&'h self, iter: I) -> impl Iterator<Item = Lexeme<'s>> + 'h
    where
        I: Iterator<Item = Lexeme<'s>> + 'h,
    {
        BlankInserterIter {
            last_pos: Position::start(),
            arg_mode: true,
            prefixy_tokens: &self.prefixy_tokens,
            suffixy_tokens: &self.suffixy_tokens,
            iter: iter.peekable(),
        }
    }
}

struct BlankInserterIter<'h, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    last_pos: Position,
    arg_mode: bool,
    prefixy_tokens: &'h [Option<(Token, bool)>],
    suffixy_tokens: &'h [Option<(Token, bool)>],
    iter: iter::Peekable<I>,
}

impl<'h, 's, I> BlankInserterIter<'h, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    fn insert_fake_token(&self, token: Token) -> Lexeme<'s> {
        Lexeme::new(token, "", self.last_pos, self.last_pos)
    }

    fn consume_lexeme(&mut self, token: Token) -> Lexeme<'s> {
        let lexeme = self.iter.next().unwrap();
        self.last_pos = lexeme.span.end;
        Lexeme { token, ..lexeme }
    }
}

impl<'h, 's, I> Iterator for BlankInserterIter<'h, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    type Item = Lexeme<'s>;

    fn next(&mut self) -> Option<Lexeme<'s>> {
        match (self.iter.peek().copied(), self.arg_mode) {
            (None, true) => {
                self.arg_mode = false;
                Some(self.insert_fake_token(TOKEN_BLANK))
            }
            (None, false) => None,
            (Some(lexeme), true) => match self.prefixy_tokens[lexeme.token] {
                Some((token, arg_mode)) => {
                    self.arg_mode = arg_mode;
                    Some(self.consume_lexeme(token))
                }
                None => {
                    self.arg_mode = false;
                    Some(self.insert_fake_token(TOKEN_BLANK))
                }
            },
            (Some(lexeme), false) => match self.suffixy_tokens[lexeme.token] {
                Some((token, arg_mode)) => {
                    self.arg_mode = arg_mode;
                    Some(self.consume_lexeme(token))
                }
                None => {
                    self.arg_mode = true;
                    Some(self.insert_fake_token(TOKEN_JUXTAPOSE))
                }
            },
        }
    }
}

#[test]
fn test_blank_insertion() {
    use crate::TOKEN_ERROR;

    const TOKEN_ID: Token = 3;
    const TOKEN_TIMES: Token = 4;
    const TOKEN_PLUS: Token = 5;
    const TOKEN_NEG: Token = 6;
    const TOKEN_MINUS: Token = 7;
    const TOKEN_BANG: Token = 8;
    const TOKEN_OPEN: Token = 9;
    const TOKEN_CLOSE: Token = 10;
    const NUM_TOKENS: usize = 11;

    fn lex<'s>(src: &'s str) -> impl Iterator<Item = Lexeme<'s>> {
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
            lexemes.push(Lexeme::new(token, &src[i..i + 1], start_pos, end_pos));
        }
        lexemes.into_iter()
    }

    #[track_caller]
    fn assert_lexeme<'a>(
        stream: &mut impl Iterator<Item = Lexeme<'a>>,
        expected: &str,
        token: Token,
    ) {
        let lex = stream
            .next()
            .expect("Token stream in test case ended early");
        assert_eq!(lex.token, token);
        let start = lex.span.start;
        let end = lex.span.end;
        let actual = format!(
            "{}:{}-{}:{} {}",
            start.line, start.col, end.line, end.col, lex.lexeme
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

    let blank_inserter = BlankInserter {
        prefixy_tokens,
        suffixy_tokens,
    };

    let src = " a";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:1-0:2 a", TOKEN_ID);
    assert!(lexemes.next().is_none());

    let src = "";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:0-0:0 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:0-0:1 -", TOKEN_NEG);
    assert_lexeme(lexemes, "0:1-0:1 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-o o-";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:0-0:1 -", TOKEN_NEG);
    assert_lexeme(lexemes, "0:1-0:2 o", TOKEN_ID);
    assert_lexeme(lexemes, "0:2-0:2 ", TOKEN_JUXTAPOSE);
    assert_lexeme(lexemes, "0:3-0:4 o", TOKEN_ID);
    assert_lexeme(lexemes, "0:4-0:5 -", TOKEN_MINUS);
    assert_lexeme(lexemes, "0:5-0:5 ", TOKEN_BLANK);
    assert!(lexemes.next().is_none());

    let src = "(x)";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:0-0:1 (", TOKEN_OPEN);
    assert_lexeme(lexemes, "0:1-0:2 x", TOKEN_ID);
    assert_lexeme(lexemes, "0:2-0:3 )", TOKEN_CLOSE);
    assert!(lexemes.next().is_none());

    let src = "%";
    let lexemes = &mut blank_inserter.transform(lex(src));
    assert_lexeme(lexemes, "0:0-0:1 %", TOKEN_ERROR);
    assert!(lexemes.next().is_none());
}
