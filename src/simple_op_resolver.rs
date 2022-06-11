use crate::{Lexeme, Token};

type OpToken = Token;

#[derive(Debug, PartialEq, Eq)]
pub struct OpResolverError<'s> {
    expected: Option<OpToken>,
    found: Option<Lexeme<'s>>,
}

pub fn resolve_ops<'a, 's: 'a, I>(
    op_table: &'a Vec<Option<OpToken>>,
    follower_table: &'a Vec<Option<(Token, OpToken)>>,
    iter: I,
) -> impl Iterator<Item = Result<Lexeme<'s>, OpResolverError<'s>>> + 'a
where
    I: Iterator<Item = Lexeme<'s>> + 'a,
{
    OpResolverIter {
        op_table,
        follower_table,
        stack: Vec::new(),
        iter,
    }
}

struct OpResolverIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    op_table: &'a Vec<Option<OpToken>>,
    follower_table: &'a Vec<Option<(Token, OpToken)>>,
    stack: Vec<(Token, OpToken)>,
    iter: I,
}

impl<'a, 's, I> OpResolverIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    fn push_follower(
        &mut self,
        op_token: OpToken,
        lexeme: Lexeme<'s>,
    ) -> Option<Result<Lexeme<'s>, OpResolverError<'s>>> {
        if let Some(next_follower) = self.follower_table[op_token] {
            self.stack.push(next_follower);
        }
        Some(Ok(Lexeme {
            token: op_token,
            ..lexeme
        }))
    }

    fn error(
        &mut self,
        expected: Option<OpToken>,
        found: Option<Lexeme<'s>>,
    ) -> Option<Result<Lexeme<'s>, OpResolverError<'s>>> {
        let parent = self.stack.iter().rev().skip(1).next();
        Some(Err(OpResolverError { expected, found }))
    }
}

impl<'a, 's, I> Iterator for OpResolverIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    type Item = Result<Lexeme<'s>, OpResolverError<'s>>;

    fn next(&mut self) -> Option<Result<Lexeme<'s>, OpResolverError<'s>>> {
        match (self.stack.last(), self.iter.next()) {
            (None, None) => None,
            (Some((_, op_token)), None) => self.error(Some(*op_token), None),
            (Some((token, op_token)), Some(lexeme)) if lexeme.token == *token => {
                let op_token = *op_token;
                self.stack.pop();
                self.push_follower(op_token, lexeme)
            }
            (None, Some(lexeme)) => match self.op_table[lexeme.token] {
                None => self.error(None, Some(lexeme)),
                Some(op_token) => self.push_follower(op_token, lexeme),
            },
            (Some((token, op_token)), Some(lexeme)) => match self.op_table[lexeme.token] {
                None => self.error(Some(*op_token), Some(lexeme)),
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
    const OP_TERNARY: Token = 4;
    const OP_TERNARY_2: Token = 5;
    const OP_HASTYPE: Token = 6;
    const OP_HASTYPE_2: Token = 7;
    const OP_LT: Token = 8;
    const OP_GT: Token = 9;
    const NUM_OP_TOKENS: usize = 10;

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
                '<' => TOKEN_LANGLE,
                '>' => TOKEN_RANGLE,
                '?' => TOKEN_QMARK,
                ':' => TOKEN_COLON,
                _ => TOKEN_ERROR,
            };
            let start_pos = pos;
            pos = pos.advance_by_char(ch);
            let end_pos = pos;
            lexemes.push(Lexeme::new(token, &src[i..i + 1], start_pos, end_pos));
        }
        lexemes.into_iter()
    }

    // Token -> op starting with that token, if any.
    let mut op_table = iter::repeat(None)
        .take(NUM_TOKENS)
        .collect::<Vec<Option<OpToken>>>();
    op_table[TOKEN_ID] = Some(OP_ID);
    op_table[TOKEN_LANGLE] = Some(OP_LT);
    op_table[TOKEN_RANGLE] = Some(OP_GT);
    op_table[TOKEN_QMARK] = Some(OP_TERNARY);
    op_table[TOKEN_COLON] = Some(OP_HASTYPE);

    // OpToken -> next follower, if any.
    let mut follower_table = iter::repeat(None)
        .take(NUM_OP_TOKENS)
        .collect::<Vec<Option<(Token, OpToken)>>>();
    follower_table[OP_TERNARY] = Some((TOKEN_COLON, OP_TERNARY_2));
    follower_table[OP_HASTYPE] = Some((TOKEN_QMARK, OP_HASTYPE_2));

    fn to_ops_vec<'s>(
        stream: &mut impl Iterator<Item = Result<Lexeme<'s>, OpResolverError<'s>>>,
    ) -> Result<Vec<OpToken>, OpResolverError<'s>> {
        stream.map(|res| res.map(|lex| lex.token)).collect()
    }

    let src = "a";
    let lexemes = &mut resolve_ops(&op_table, &follower_table, lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID]));

    let src = "a < b";
    let lexemes = &mut resolve_ops(&op_table, &follower_table, lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID, OP_LT, OP_ID]));

    let src = "a ? b > c : d > e";
    let lexemes = &mut resolve_ops(&op_table, &follower_table, lex(src));
    assert_eq!(
        to_ops_vec(lexemes),
        Ok(vec![
            OP_ID,
            OP_TERNARY,
            OP_ID,
            OP_GT,
            OP_ID,
            OP_TERNARY_2,
            OP_ID,
            OP_GT,
            OP_ID
        ])
    );

    let src = "a : b<c ?";
    let lexemes = &mut resolve_ops(&op_table, &follower_table, lex(src));
    assert_eq!(
        to_ops_vec(lexemes),
        Ok(vec![OP_ID, OP_HASTYPE, OP_ID, OP_LT, OP_ID, OP_HASTYPE_2,])
    );
}
