use crate::{
    Lexeme, OpToken, OpTokenInfo, Position, Span, Token, TokenInfo, TOKEN_BLANK, TOKEN_JUXTAPOSE,
};
use std::iter;

pub(crate) fn resolve(
    token_table: &[TokenInfo],
    op_token_table: &[OpTokenInfo],
    input: Vec<Lexeme>,
) -> Result<Vec<Lexeme>, ResolverError> {
    Resolver::new(token_table, op_token_table).resolve(input)
}

struct Resolver<'a> {
    token_table: &'a [TokenInfo],
    op_token_table: &'a [OpTokenInfo],
    arg_mode: bool,
    last_pos: Position,
    stack: Vec<(Token, OpToken, bool)>,
    output: Vec<Lexeme>,
}

impl<'a> Resolver<'a> {
    fn new(token_table: &'a [TokenInfo], op_token_table: &'a [OpTokenInfo]) -> Resolver<'a> {
        Resolver {
            token_table,
            op_token_table,
            arg_mode: true,
            last_pos: Position::start_of_file(),
            stack: vec![],
            output: vec![],
        }
    }

    fn produce(&mut self, optok: OpToken, span: Span) {
        if let Some(follower) = self.op_token_table[optok].follower {
            self.stack.push(follower);
        }
        self.output.push(Lexeme { token: optok, span });
    }

    fn produce_blank(&mut self) {
        self.output.push(Lexeme {
            token: TOKEN_BLANK,
            span: Span::new_at_pos(self.last_pos),
        });
    }

    fn produce_juxtapose(&mut self) {
        self.output.push(Lexeme {
            token: TOKEN_JUXTAPOSE,
            span: Span::new_at_pos(self.last_pos),
        });
    }

    fn error(&mut self, top: Option<(Token, OpToken, bool)>, lexeme: Lexeme) -> ResolverError {
        if let Some((tok, optok, _)) = top {
            ResolverError::WrongToken {
                op: optok,
                expected: tok,
                found: lexeme,
            }
        } else {
            ResolverError::UnexpectedToken { found: lexeme }
        }
    }

    fn resolve(mut self, input: Vec<Lexeme>) -> Result<Vec<Lexeme>, ResolverError> {
        for lexeme in input {
            match self.stack.last().copied() {
                Some((tok, optok, has_arg)) if lexeme.token == tok => {
                    self.arg_mode = has_arg;
                    self.stack.pop();
                    self.produce(optok, lexeme.span);
                }
                top => {
                    let row = &self.token_table[lexeme.token];
                    if self.arg_mode {
                        if let Some((optok, has_arg)) = row.as_prefix {
                            self.arg_mode = has_arg;
                            self.produce(optok, lexeme.span);
                        } else if let Some((optok, has_arg)) = row.as_suffix {
                            self.arg_mode = has_arg;
                            self.produce_blank();
                            self.produce(optok, lexeme.span);
                        } else {
                            return Err(self.error(top, lexeme));
                        }
                    } else {
                        if let Some((optok, has_arg)) = row.as_suffix {
                            self.arg_mode = has_arg;
                            self.produce(optok, lexeme.span);
                        } else if let Some((optok, has_arg)) = row.as_prefix {
                            self.arg_mode = has_arg;
                            self.produce_juxtapose();
                            self.produce(optok, lexeme.span);
                        } else {
                            return Err(self.error(top, lexeme));
                        }
                    }
                }
            }
            self.last_pos = lexeme.span.end;
        }
        if let Some((tok, optok, _)) = self.stack.pop() {
            return Err(ResolverError::UnexpectedEof {
                op: optok,
                expected: tok,
            });
        }
        if self.arg_mode {
            self.produce_blank();
        }
        Ok(self.output)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolverError {
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
