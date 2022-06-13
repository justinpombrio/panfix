use crate::{Lexeme, OpToken, Token};

pub fn resolve<'a, 's: 'a, I>(
    op_table: &'a Vec<Option<OpToken>>,
    follower_table: &'a Vec<Option<(Token, OpToken)>>,
    iter: I,
) -> impl Iterator<Item = Result<Lexeme, OpResolverError>> + 'a
where
    I: Iterator<Item = Lexeme> + 'a,
{
    OpResolver {
        op_table,
        follower_table,
        stack: vec![],
        iter: iter.peekable(),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum OpResolverError {
    WrongToken { expected: OpToken, found: Lexeme },
    UnexpectedToken { found: Lexeme },
    UnexpectedEof { expected: OpToken },
}

struct OpResolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    op_table: &'a Vec<Option<OpToken>>,
    follower_table: &'a Vec<Option<(Token, OpToken)>>,
    stack: Vec<(Token, OpToken)>,
    iter: I,
}

impl<'a, I> OpResolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    fn push_follower(
        &mut self,
        op_token: OpToken,
        lexeme: Lexeme,
    ) -> Option<Result<Lexeme, OpResolverError>> {
        if let Some(next_follower) = self.follower_table[op_token] {
            self.stack.push(next_follower);
        }
        Some(Ok(Lexeme {
            token: op_token,
            ..lexeme
        }))
    }
}

impl<'a, I> Iterator for OpResolver<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    type Item = Result<Lexeme, OpResolverError>;

    fn next(&mut self) -> Option<Result<Lexeme, OpResolverError>> {
        use OpResolverError::{UnexpectedEof, UnexpectedToken, WrongToken};

        match (self.stack.last().copied(), self.iter.next()) {
            (None, None) => None,
            (Some((_, optok)), None) => Some(Err(UnexpectedEof { expected: optok })),
            (Some((tok, optok)), Some(lexeme)) if lexeme.token == tok => {
                self.stack.pop();
                self.push_follower(optok, lexeme)
            }
            (None, Some(lexeme)) => match self.op_table[lexeme.token] {
                None => Some(Err(UnexpectedToken { found: lexeme })),
                Some(op_token) => self.push_follower(op_token, lexeme),
            },
            (Some((tok, optok)), Some(lexeme)) => match self.op_table[lexeme.token] {
                None => Some(Err(WrongToken {
                    expected: tok,
                    found: lexeme,
                })),
                Some(op_token) => self.push_follower(op_token, lexeme),
            },
        }
    }
}

#[test]
fn test_op_resolver() {
    use crate::{Position, TOKEN_ERROR};
    use std::iter;

    // SYNTAX:
    //
    // Sort Expr:
    //   _ < _         -- less than
    //   _ > _         -- greater than
    //   _ ? Expr : _  -- ternary expression
    //   _ : Type ?    -- is of type?

    const TOKEN_ID: Token = 3;
    const TOKEN_LANGLE: Token = 4;
    const TOKEN_RANGLE: Token = 5;
    const TOKEN_QMARK: Token = 6;
    const TOKEN_COLON: Token = 7;
    const NUM_TOKENS: usize = 8;

    const OP_ID: Token = 3;
    const OP_TERNARY_1: Token = 4;
    const OP_TERNARY_2: Token = 5;
    const OP_HASTYPE_1: Token = 6;
    const OP_HASTYPE_2: Token = 7;
    const OP_LT: Token = 8;
    const OP_GT: Token = 9;
    const NUM_OP_TOKENS: usize = 10;

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
                '<' => TOKEN_LANGLE,
                '>' => TOKEN_RANGLE,
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

    // SortId -> Token -> op starting with that token, if any.
    let mut op_table = vec![];
    for _ in 0..NUM_TOKENS {
        op_table.push(None);
    }
    op_table[TOKEN_ID] = Some(OP_ID);
    op_table[TOKEN_LANGLE] = Some(OP_LT);
    op_table[TOKEN_RANGLE] = Some(OP_GT);
    op_table[TOKEN_QMARK] = Some(OP_TERNARY_1);
    op_table[TOKEN_COLON] = Some(OP_HASTYPE_1);

    // OpToken -> next follower, if any.
    let mut follower_table = iter::repeat(None)
        .take(NUM_OP_TOKENS)
        .collect::<Vec<Option<(Token, OpToken)>>>();
    follower_table[OP_TERNARY_1] = Some((TOKEN_COLON, OP_TERNARY_2));
    follower_table[OP_HASTYPE_1] = Some((TOKEN_QMARK, OP_HASTYPE_2));

    fn to_ops_vec(
        stream: &mut impl Iterator<Item = Result<Lexeme, OpResolverError>>,
    ) -> Result<Vec<OpToken>, OpResolverError> {
        stream.map(|res| res.map(|lex| lex.token)).collect()
    }

    let src = "a";
    let lexemes = &mut resolve(&op_table, &follower_table, lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID]));

    let src = "a < b";
    let lexemes = &mut resolve(&op_table, &follower_table, lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID, OP_LT, OP_ID]));

    let src = "a ? b > c : d > e";
    let lexemes = &mut resolve(&op_table, &follower_table, lex(src));
    assert_eq!(
        to_ops_vec(lexemes),
        Ok(vec![
            OP_ID,
            OP_TERNARY_1,
            OP_ID,
            OP_GT,
            OP_ID,
            OP_TERNARY_2,
            OP_ID,
            OP_GT,
            OP_ID
        ])
    );
}
