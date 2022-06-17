use crate::{Lexeme, OpToken, Token};

#[derive(Debug, PartialEq, Eq)]
pub enum OpResolverError {
    WrongToken {
        op: OpToken,
        expected: Token,
        found: Lexeme,
    },
    UnexpectedToken {
        found: Lexeme,
    },
    UnexpectedEof {
        op: OpToken,
        expected: Token,
    },
}

pub fn resolve(
    op_table: &[Option<OpToken>],
    follower_table: &[Option<(Token, OpToken)>],
    lexemes: Vec<Lexeme>,
) -> Result<Vec<Lexeme>, OpResolverError> {
    use OpResolverError::{UnexpectedEof, UnexpectedToken, WrongToken};

    let mut output = vec![];
    let mut produce = |stack: &mut Vec<(Token, OpToken)>, optok: OpToken, lexeme: Lexeme| {
        if let Some(next_follower) = follower_table[optok] {
            stack.push(next_follower);
        }
        output.push(Lexeme {
            token: optok,
            span: lexeme.span,
        });
    };

    let mut stack = vec![];
    for lexeme in lexemes {
        match stack.last().copied() {
            None => match op_table[lexeme.token] {
                None => return Err(UnexpectedToken { found: lexeme }),
                Some(optok) => produce(&mut stack, optok, lexeme),
            },
            Some((tok, optok)) if lexeme.token == tok => {
                stack.pop();
                produce(&mut stack, optok, lexeme);
            }
            Some((tok, optok)) => match op_table[lexeme.token] {
                None => {
                    return Err(WrongToken {
                        op: optok,
                        expected: tok,
                        found: lexeme,
                    })
                }
                Some(optok) => produce(&mut stack, optok, lexeme),
            },
        }
    }
    if let Some((tok, optok)) = stack.pop() {
        return Err(UnexpectedEof {
            op: optok,
            expected: tok,
        });
    }
    Ok(output)
}
