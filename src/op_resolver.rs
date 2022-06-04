use crate::{Lexeme, Token};

pub type SortId = usize;
type OpToken = Token;

#[derive(Debug, PartialEq, Eq)]
pub struct OpResolverError<'s> {
    sort: SortId,
    expected: Option<OpToken>,
    found: Option<Lexeme<'s>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Follower {
    sort: SortId,
    token: Token,
    op_token: OpToken,
}

pub struct OpResolver {
    /// SortId -> Token -> op starting with that token, if any.
    pub op_table: Vec<Vec<Option<OpToken>>>,
    /// OpToken -> next follower, if any.
    pub follower_table: Vec<Option<Follower>>,
    pub starting_sort: SortId,
}

impl OpResolver {
    pub fn transform<'a, 's: 'a, I>(
        &'a self,
        iter: I,
    ) -> impl Iterator<Item = Result<Lexeme<'s>, OpResolverError<'s>>> + 'a
    where
        I: Iterator<Item = Lexeme<'s>> + 'a,
    {
        OpResolverIter {
            op_table: &self.op_table,
            follower_table: &self.follower_table,
            starting_sort: self.starting_sort,
            stack: Vec::new(),
            iter,
        }
    }
}

struct OpResolverIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    op_table: &'a Vec<Vec<Option<OpToken>>>,
    follower_table: &'a Vec<Option<Follower>>,
    starting_sort: SortId,
    stack: Vec<Follower>,
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
        let sort = parent.map(|f| f.sort).unwrap_or(self.starting_sort);
        Some(Err(OpResolverError {
            sort,
            expected,
            found,
        }))
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
            (Some(follower), None) => self.error(Some(follower.op_token), None),
            (Some(follower), Some(lexeme)) if lexeme.token == follower.token => {
                let op_token = follower.op_token;
                self.stack.pop();
                self.push_follower(op_token, lexeme)
            }
            (None, Some(lexeme)) => match self.op_table[self.starting_sort][lexeme.token] {
                None => self.error(None, Some(lexeme)),
                Some(op_token) => self.push_follower(op_token, lexeme),
            },
            (Some(follower), Some(lexeme)) => match self.op_table[follower.sort][lexeme.token] {
                None => self.error(Some(follower.op_token), Some(lexeme)),
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
    //
    // Sort Type:
    //   _ < Type >    -- type parameter
    //   _ ?           -- optional type

    const TOKEN_ID: Token = 3;
    const TOKEN_LANGLE: Token = 4;
    const TOKEN_RANGLE: Token = 5;
    const TOKEN_QMARK: Token = 6;
    const TOKEN_COLON: Token = 7;
    const NUM_TOKENS: usize = 8;

    const SORT_EXPR: usize = 0;
    const SORT_TYPE: usize = 1;

    const OP_ID: Token = 3;
    const OP_TERNARY: Token = 4;
    const OP_TERNARY_2: Token = 5;
    const OP_HASTYPE: Token = 6;
    const OP_HASTYPE_2: Token = 7;
    const OP_TYPARAM: Token = 8;
    const OP_TYPARAM_2: Token = 9;
    const OP_OPTTYPE: Token = 10;
    const OP_LT: Token = 11;
    const OP_GT: Token = 12;
    const OP_TYPEID: Token = 13;
    const NUM_OP_TOKENS: usize = 14;

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

    let starting_sort = SORT_EXPR;

    // SortId -> Token -> op starting with that token, if any.
    let mut op_table: Vec<Vec<Option<OpToken>>> = vec![
        iter::repeat(None)
            .take(NUM_TOKENS)
            .collect::<Vec<Option<OpToken>>>(),
        iter::repeat(None)
            .take(NUM_TOKENS)
            .collect::<Vec<Option<OpToken>>>(),
    ];
    op_table[SORT_EXPR][TOKEN_ID] = Some(OP_ID);
    op_table[SORT_EXPR][TOKEN_LANGLE] = Some(OP_LT);
    op_table[SORT_EXPR][TOKEN_RANGLE] = Some(OP_GT);
    op_table[SORT_EXPR][TOKEN_QMARK] = Some(OP_TERNARY);
    op_table[SORT_EXPR][TOKEN_COLON] = Some(OP_HASTYPE);
    op_table[SORT_TYPE][TOKEN_LANGLE] = Some(OP_TYPARAM);
    op_table[SORT_TYPE][TOKEN_QMARK] = Some(OP_OPTTYPE);
    op_table[SORT_TYPE][TOKEN_ID] = Some(OP_TYPEID);

    // OpToken -> next follower, if any.
    let mut follower_table = iter::repeat(None)
        .take(NUM_OP_TOKENS)
        .collect::<Vec<Option<Follower>>>();
    follower_table[OP_TERNARY] = Some(Follower {
        sort: SORT_EXPR,
        token: TOKEN_COLON,
        op_token: OP_TERNARY_2,
    });
    follower_table[OP_HASTYPE] = Some(Follower {
        sort: SORT_TYPE,
        token: TOKEN_QMARK,
        op_token: OP_HASTYPE_2,
    });
    follower_table[OP_TYPARAM] = Some(Follower {
        sort: SORT_TYPE,
        token: TOKEN_RANGLE,
        op_token: OP_TYPARAM_2,
    });

    let op_resolver = OpResolver {
        starting_sort,
        op_table,
        follower_table,
    };

    fn to_ops_vec<'s>(
        stream: &mut impl Iterator<Item = Result<Lexeme<'s>, OpResolverError<'s>>>,
    ) -> Result<Vec<OpToken>, OpResolverError<'s>> {
        stream.map(|res| res.map(|lex| lex.token)).collect()
    }

    let src = "a";
    let lexemes = &mut op_resolver.transform(lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID]));

    let src = "a < b";
    let lexemes = &mut op_resolver.transform(lex(src));
    assert_eq!(to_ops_vec(lexemes), Ok(vec![OP_ID, OP_LT, OP_ID]));

    let src = "a ? b > c : d > e";
    let lexemes = &mut op_resolver.transform(lex(src));
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

    let src = "a : b<t?> ?";
    let lexemes = &mut op_resolver.transform(lex(src));
    assert_eq!(
        to_ops_vec(lexemes),
        Ok(vec![
            OP_ID,
            OP_HASTYPE,
            OP_TYPEID,
            OP_TYPARAM,
            OP_TYPEID,
            OP_OPTTYPE,
            OP_TYPARAM_2,
            OP_HASTYPE_2
        ])
    );
}
