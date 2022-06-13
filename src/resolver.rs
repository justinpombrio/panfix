use crate::{Lexeme, OpToken, Position, Token, TOKEN_BLANK, TOKEN_JUXTAPOSE};
use std::iter;

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
    follower_tokens: &'a Vec<Option<(Token, OpToken, bool)>>,
    iter: I,
) -> impl Iterator<Item = Result<Lexeme, ResolverError>> + 'a
where
    I: Iterator<Item = Lexeme> + 'a,
{
    Resolver {
        last_pos: Position::start(),
        expr_mode: true,
        prefixy_tokens,
        suffixy_tokens,
        follower_tokens,
        stack: vec![],
        iter: iter.peekable(),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolverError {
    WrongToken { expected: OpToken, found: Lexeme },
    UnexpectedToken { found: Lexeme },
    UnexpectedEof { expected: OpToken },
}

struct Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    last_pos: Position,
    expr_mode: bool,
    prefixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    suffixy_tokens: &'a Vec<Option<(OpToken, bool)>>,
    follower_tokens: &'a Vec<Option<(Token, OpToken, bool)>>,
    stack: Vec<(Token, OpToken, bool)>,
    iter: iter::Peekable<I>,
}

impl<'a, I> Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    fn insert_fake_token(&self, token: Token) -> Lexeme {
        Lexeme::new(token, self.last_pos, self.last_pos)
    }

    fn consume_lexeme(&mut self, op_token: OpToken) -> Lexeme {
        let lexeme = self.iter.next().unwrap();
        self.last_pos = lexeme.span.end;
        if let Some(follower) = self.follower_tokens[op_token] {
            self.stack.push(follower);
        }
        Lexeme {
            token: op_token,
            ..lexeme
        }
    }
}

impl<'a, I> Iterator for Resolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    type Item = Result<Lexeme, ResolverError>;

    #[allow(clippy::collapsible_else_if)]
    fn next(&mut self) -> Option<Result<Lexeme, ResolverError>> {
        let lexeme = match self.iter.peek().copied() {
            Some(lexeme) => lexeme,
            None if self.expr_mode => {
                self.expr_mode = false;
                return Some(Ok(self.insert_fake_token(TOKEN_BLANK)));
            }
            None => {
                if let Some((_, optok, _)) = self.stack.last().copied() {
                    self.stack.clear();
                    return Some(Err(ResolverError::UnexpectedEof { expected: optok }));
                } else {
                    return None;
                }
            }
        };
        if let Some((expected_token, replacement_token, has_arg)) = self.stack.last().copied() {
            if lexeme.token == expected_token {
                if self.expr_mode {
                    self.expr_mode = false;
                    return Some(Ok(self.insert_fake_token(TOKEN_BLANK)));
                } else {
                    self.stack.pop();
                    self.expr_mode = has_arg;
                    return Some(Ok(self.consume_lexeme(replacement_token)));
                }
            }
        }
        if self.expr_mode {
            if let Some((optok, has_arg)) = self.prefixy_tokens[lexeme.token] {
                self.expr_mode = has_arg;
                Some(Ok(self.consume_lexeme(optok)))
            } else {
                self.expr_mode = false;
                Some(Ok(self.insert_fake_token(TOKEN_BLANK)))
            }
        } else {
            if let Some((optok, has_arg)) = self.suffixy_tokens[lexeme.token] {
                self.expr_mode = has_arg;
                Some(Ok(self.consume_lexeme(optok)))
            } else if self.prefixy_tokens[lexeme.token].is_some() {
                self.expr_mode = true;
                Some(Ok(self.insert_fake_token(TOKEN_JUXTAPOSE)))
            } else {
                if let Some((_, optok, _)) = self.stack.pop() {
                    self.iter.next();
                    self.stack.clear();
                    Some(Err(ResolverError::WrongToken {
                        expected: optok,
                        found: lexeme,
                    }))
                } else {
                    self.iter.next();
                    Some(Err(ResolverError::UnexpectedToken { found: lexeme }))
                }
            }
        }
    }
}

#[test]
fn test_resolver() {
    use crate::{Position, Span, TOKEN_ERROR};

    // SYNTAX:
    //   - _
    //   _ - _
    //   ( _ )
    //   _ ( _ )
    //   _ ? _ : _
    //   _ : _

    const TOKEN_ID: Token = 3;
    const TOKEN_MINUS: Token = 4;
    const TOKEN_OPEN: Token = 5;
    const TOKEN_CLOSE: Token = 6;
    const TOKEN_QMARK: Token = 7;
    const TOKEN_COLON: Token = 8;
    const NUM_TOKENS: usize = 9;

    const OP_ERROR: OpToken = TOKEN_ERROR;
    const OP_BLANK: OpToken = TOKEN_BLANK;
    const OP_JUXTAPOSE: OpToken = TOKEN_JUXTAPOSE;
    const OP_ID: OpToken = 3;
    const OP_NEG: OpToken = 4;
    const OP_MINUS: OpToken = 5;
    const OP_GROUP_1: OpToken = 6;
    const OP_GROUP_2: OpToken = 7;
    const OP_APPLY_1: OpToken = 8;
    const OP_APPLY_2: OpToken = 9;
    const OP_TERNARY_1: OpToken = 10;
    const OP_TERNARY_2: OpToken = 11;
    const OP_HASTYPE: OpToken = 12;
    const NUM_OP_TOKENS: usize = 13;

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
    fn assert_lexeme(
        src: &str,
        stream: &mut impl Iterator<Item = Result<Lexeme, ResolverError>>,
        expected: &str,
        token: Token,
    ) {
        let lex = stream.next().expect("Token stream ended early").unwrap();
        let start = lex.span.start.col as usize;
        let end = lex.span.end.col as usize;
        assert_eq!(&src[start..end], expected);
        assert_eq!(lex.token, token);
    }

    #[track_caller]
    fn assert_error(
        src: &str,
        stream: &mut impl Iterator<Item = Result<Lexeme, ResolverError>>,
        expected_error: ResolverError,
    ) {
        let err = stream
            .next()
            .expect("Token stream ended early")
            .unwrap_err();
        assert_eq!(err, expected_error);
    }

    let mut prefixy_tokens = Vec::new();
    let mut suffixy_tokens = Vec::new();
    for _ in 0..NUM_TOKENS {
        prefixy_tokens.push(None);
        suffixy_tokens.push(None);
    }
    prefixy_tokens[TOKEN_ERROR] = Some((TOKEN_ERROR, false));
    prefixy_tokens[TOKEN_ID] = Some((OP_ID, false));
    prefixy_tokens[TOKEN_MINUS] = Some((OP_NEG, true));
    suffixy_tokens[TOKEN_MINUS] = Some((OP_MINUS, true));
    prefixy_tokens[TOKEN_OPEN] = Some((OP_GROUP_1, true));
    suffixy_tokens[TOKEN_OPEN] = Some((OP_APPLY_1, true));
    suffixy_tokens[TOKEN_QMARK] = Some((OP_TERNARY_1, true));
    suffixy_tokens[TOKEN_COLON] = Some((OP_HASTYPE, true));

    let mut follower_tokens = Vec::new();
    for _ in 0..NUM_OP_TOKENS {
        follower_tokens.push(None);
    }
    follower_tokens[OP_GROUP_1] = Some((TOKEN_CLOSE, OP_GROUP_2, false));
    follower_tokens[OP_APPLY_1] = Some((TOKEN_CLOSE, OP_APPLY_2, false));
    follower_tokens[OP_TERNARY_1] = Some((TOKEN_COLON, OP_TERNARY_2, true));

    let src = " a";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "a", OP_ID);
    assert!(lexemes.next().is_none());

    let src = "";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "-", OP_NEG);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "-o o-";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "-", OP_NEG);
    assert_lexeme(src, lexemes, "o", OP_ID);
    assert_lexeme(src, lexemes, "", OP_JUXTAPOSE);
    assert_lexeme(src, lexemes, "o", OP_ID);
    assert_lexeme(src, lexemes, "-", OP_MINUS);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert!(lexemes.next().is_none());

    let src = "(x)";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "x", OP_ID);
    assert_lexeme(src, lexemes, ")", OP_GROUP_2);
    assert!(lexemes.next().is_none());

    let src = "f()";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "f", OP_ID);
    assert_lexeme(src, lexemes, "(", OP_APPLY_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, ")", OP_APPLY_2);
    assert!(lexemes.next().is_none());

    let src = "(x()?(y):z())";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "x", OP_ID);
    assert_lexeme(src, lexemes, "(", OP_APPLY_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, ")", OP_APPLY_2);
    assert_lexeme(src, lexemes, "?", OP_TERNARY_1);
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "y", OP_ID);
    assert_lexeme(src, lexemes, ")", OP_GROUP_2);
    assert_lexeme(src, lexemes, ":", OP_TERNARY_2);
    assert_lexeme(src, lexemes, "z", OP_ID);
    assert_lexeme(src, lexemes, "(", OP_APPLY_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, ")", OP_APPLY_2);
    assert_lexeme(src, lexemes, ")", OP_GROUP_2);
    assert!(lexemes.next().is_none());

    let src = "%";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "%", OP_ERROR);
    assert!(lexemes.next().is_none());

    let src = "(?";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, "?", OP_TERNARY_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_error(
        src,
        lexemes,
        ResolverError::UnexpectedEof {
            expected: OP_TERNARY_2,
        },
    );
    assert!(lexemes.next().is_none());

    let src = "(?)";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, "?", OP_TERNARY_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    let start = Position {
        line: 0,
        col: 2,
        utf8_col: 2,
    };
    let end = Position {
        line: 0,
        col: 3,
        utf8_col: 3,
    };
    assert_error(
        src,
        lexemes,
        ResolverError::WrongToken {
            expected: OP_TERNARY_2,
            found: Lexeme::new(TOKEN_CLOSE, start, end),
        },
    );
    assert!(lexemes.next().is_none());

    let src = "())";
    let lexemes = &mut resolve(&prefixy_tokens, &suffixy_tokens, &follower_tokens, lex(src));
    assert_lexeme(src, lexemes, "(", OP_GROUP_1);
    assert_lexeme(src, lexemes, "", OP_BLANK);
    assert_lexeme(src, lexemes, ")", OP_GROUP_2);
    assert_error(
        src,
        lexemes,
        ResolverError::UnexpectedToken {
            found: Lexeme::new(TOKEN_CLOSE, start, end),
        },
    );
    assert!(lexemes.next().is_none());
}
