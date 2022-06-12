use crate::{Lexeme, Position, Token, TOKEN_BLANK, TOKEN_JUXTAPOSE};
use std::iter;

type OpToken = Token;

/// Resolve tokens into "operator tokens", and insert a `blank` token when an argument is missing
/// and a `juxtapose` token when arguments are placed next to each other.
///
/// There are 1-2 "operator tokens" per token, depending on whether the token can be used with or
/// without a left argument. For example, `-` could be a token, and "unary minus" (like `- 2) and
/// "binary minus" (like `1 - 2`) would be its two operator tokens.
///
/// ## How the tables are constructed
///
/// "Prefixy" tokens have no left argument, and "suffixy" tokens do. `prefixy_tokens` and
/// `suffixy_tokens` are indexed by token, and give (i) the operator token that token should be
/// mapped to, and (ii) whether that operator token takes a right argument.
pub fn resolve<'a, 's: 'a, I>(
    prefixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    suffixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    iter: I,
) -> impl Iterator<Item = Lexeme> + 'a
where
    I: Iterator<Item = Lexeme> + 'a,
{
    Resolver {
        last_pos: Position::start(),
        expr_mode: true,
        prefixy_tokens,
        suffixy_tokens,
        iter: iter.peekable(),
    }
}

struct Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    last_pos: Position,
    expr_mode: bool,
    prefixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    suffixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    iter: iter::Peekable<I>,
}

impl<'a, I> Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    fn insert_fake_token(&self, token: Token) -> Lexeme {
        Lexeme::new(token, self.last_pos, self.last_pos)
    }

    fn consume_lexeme(&mut self, token: Token) -> Lexeme {
        let lexeme = self.iter.next().unwrap();
        self.last_pos = lexeme.span.end;
        Lexeme { token, ..lexeme }
    }
}

impl<'a, I> Iterator for Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    type Item = Lexeme;

    #[allow(clippy::collapsible_else_if)]
    fn next(&mut self) -> Option<Lexeme> {
        let lexeme = match self.iter.peek() {
            Some(lexeme) => lexeme,
            None if self.expr_mode => {
                self.expr_mode = false;
                return Some(self.insert_fake_token(TOKEN_BLANK));
            }
            None => return None,
        };
        if self.expr_mode {
            if let Some((optok, has_arg)) = self.prefixy_tokens[lexeme.token] {
                self.expr_mode = has_arg;
                Some(self.consume_lexeme(optok))
            } else {
                self.expr_mode = false;
                Some(self.insert_fake_token(TOKEN_BLANK))
            }
        } else {
            if let Some((optok, has_arg)) = self.suffixy_tokens[lexeme.token] {
                self.expr_mode = has_arg;
                Some(self.consume_lexeme(optok))
            } else {
                self.expr_mode = true;
                Some(self.insert_fake_token(TOKEN_JUXTAPOSE))
            }
        }
    }
}

#[test]
fn test_resolver() {
    use crate::TOKEN_ERROR;

    // SYNTAX:
    //   _ + _
    //   - _
    //   _ - _
    //   ( _ )
    //   _ ( _ )
    //   _ ? _ : _
    //   _ : _

    const TOKEN_ID: Token = 3;
    const TOKEN_TIMES: Token = 4;
    const TOKEN_PLUS: Token = 5;
    const TOKEN_MINUS: Token = 6;
    const TOKEN_OPEN: Token = 7;
    const TOKEN_CLOSE: Token = 8;
    const TOKEN_QMARK: Token = 9;
    const TOKEN_COLON: Token = 10;
    const NUM_TOKENS: usize = 11;

    const OP_ERROR: Token = TOKEN_ERROR;
    const OP_BLANK: Token = TOKEN_BLANK;
    const OP_JUXTAPOSE: Token = TOKEN_JUXTAPOSE;
    const OP_ID: Token = 3;
    const OP_TIMES: Token = 4;
    const OP_PLUS: Token = 5;
    const OP_NEG: Token = 6;
    const OP_MINUS: Token = 7;
    const OP_GROUP: Token = 8;
    const OP_APPLY: Token = 10;
    const OP_CLOSE: Token = 9;
    const OP_TERNARY: Token = 11;
    const OP_COLON: Token = 12;

    fn lex(src: &str) -> impl Iterator<Item = Lexeme> {
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
                '-' => TOKEN_MINUS,
                '(' => TOKEN_OPEN,
                ')' => TOKEN_CLOSE,
                '?' => TOKEN_QMARK,
                ':' => TOKEN_COLON,
                _ => TOKEN_ERROR,
            };
            let start_pos = pos;
            pos = pos.advance_by_char(ch);
            let end_pos = pos;
            lexemes.push(Lexeme::new(token, start_pos, end_pos));
        }
        lexemes.into_iter()
    }

    #[track_caller]
    fn assert_lexeme<'s>(
        src: &'s str,
        stream: &mut impl Iterator<Item = Lexeme>,
        expected: &str,
        token: Token,
    ) {
        let lex = stream.next().expect("Token stream ended early");
        let start = lex.span.start.col as usize;
        let end = lex.span.end.col as usize;
        assert_eq!(&src[start..end], expected);
        assert_eq!(lex.token, token);
    }

    let mut prefixy_tokens = Vec::new();
    let mut suffixy_tokens = Vec::new();
    for _ in 0..NUM_TOKENS {
        prefixy_tokens.push(None);
        suffixy_tokens.push(None);
    }
    prefixy_tokens[TOKEN_ERROR] = Some((TOKEN_ERROR, false));
    prefixy_tokens[TOKEN_ID] = Some((OP_ID, false));
    suffixy_tokens[TOKEN_TIMES] = Some((OP_TIMES, true));
    suffixy_tokens[TOKEN_PLUS] = Some((OP_PLUS, true));
    prefixy_tokens[TOKEN_MINUS] = Some((OP_NEG, true));
    suffixy_tokens[TOKEN_MINUS] = Some((OP_MINUS, true));
    prefixy_tokens[TOKEN_OPEN] = Some((OP_GROUP, true));
    suffixy_tokens[TOKEN_OPEN] = Some((OP_APPLY, true));
    suffixy_tokens[TOKEN_CLOSE] = Some((OP_CLOSE, false));
    suffixy_tokens[TOKEN_QMARK] = Some((OP_TERNARY, true));
    suffixy_tokens[TOKEN_COLON] = Some((OP_COLON, true));

    let src = " a";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "a", OP_ID);
    assert!(lexemes.next().is_none());

    let src = "";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "-", OP_NEG);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-o o-";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "-", OP_NEG);
    assert_lexeme(src, lexemes, "o", OP_ID);
    assert_lexeme(src, lexemes, "", OP_JUXTAPOSE);
    assert_lexeme(src, lexemes, "o", OP_ID);
    assert_lexeme(src, lexemes, "-", OP_MINUS);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "(x)";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP);
    assert_lexeme(src, lexemes, "x", OP_ID);
    assert_lexeme(src, lexemes, ")", OP_CLOSE);
    assert!(lexemes.next().is_none());

    let src = "f()";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "f", OP_ID);
    assert_lexeme(src, lexemes, "(", OP_APPLY);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, ")", OP_CLOSE);
    assert!(lexemes.next().is_none());

    let src = "%";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, lex(src));
    assert_lexeme(src, lexemes, "%", OP_ERROR);
    assert!(lexemes.next().is_none());
}
