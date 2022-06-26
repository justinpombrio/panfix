use crate::{Lexeme, OpToken, Position, Span, Token, TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE};

/// Resolve "tokens" into "op tokens". Both have type `Token` but are in different spaces (i.e. the
/// token `4` and the op token `4` are likely unrelated). This resolution achieves two tasks:
///
/// 1. If the same token is used in multiple operators, disambiguate them. For example, unary minus
///    and binary minus would have the same _token_ but different _op tokens_.
/// 2. Insert a `TOKEN_BLANK` for every missing argument and a `TOKEN_JUXTAPOSE` for every missing
///    binary operator.
pub fn resolve(
    tok_to_prefix: &[Option<(OpToken, bool)>],
    tok_to_suffix: &[Option<(OpToken, bool)>],
    optok_to_follower: &[Option<(Token, OpToken, bool)>],
    input: impl IntoIterator<Item = Lexeme>,
) -> Result<Vec<Lexeme>, ResolverError> {
    Resolver::new(tok_to_prefix, tok_to_suffix, optok_to_follower).resolve(input.into_iter())
}

#[derive(Debug, PartialEq, Eq)]
pub enum ResolverError {
    /// Error while lexing.
    LexError(Lexeme),
    /// Did not expect token; it does not start an operator.
    UnexpectedToken(Lexeme),
    /// While parsing `op`, expected token `expected` but found token `found.token` (or found
    /// end-of-file if None).
    IncompleteOp {
        op: OpToken,
        expected: Token,
        found: Option<Lexeme>,
    },
}

struct Resolver<'a> {
    tok_to_prefix: &'a [Option<(OpToken, bool)>],
    tok_to_suffix: &'a [Option<(OpToken, bool)>],
    optok_to_follower: &'a [Option<(Token, OpToken, bool)>],
    arg_mode: bool,
    last_pos: Position,
    stack: Vec<(Token, OpToken, bool)>,
    output: Vec<Lexeme>,
}

impl<'a> Resolver<'a> {
    fn new(
        tok_to_prefix: &'a [Option<(OpToken, bool)>],
        tok_to_suffix: &'a [Option<(OpToken, bool)>],
        optok_to_follower: &'a [Option<(Token, OpToken, bool)>],
    ) -> Resolver<'a> {
        Resolver {
            tok_to_prefix,
            tok_to_suffix,
            optok_to_follower,
            arg_mode: true,
            last_pos: Position::start_of_file(),
            stack: vec![],
            output: vec![],
        }
    }

    fn produce(&mut self, optok: OpToken, span: Span) {
        if let Some(follower) = self.optok_to_follower[optok] {
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
            ResolverError::IncompleteOp {
                op: optok,
                expected: tok,
                found: Some(lexeme),
            }
        } else {
            ResolverError::UnexpectedToken(lexeme)
        }
    }

    #[allow(clippy::collapsible_else_if)]
    fn resolve(
        mut self,
        input: impl Iterator<Item = Lexeme>,
    ) -> Result<Vec<Lexeme>, ResolverError> {
        for lexeme in input {
            if lexeme.token == TOKEN_ERROR {
                return Err(ResolverError::LexError(lexeme));
            }
            match self.stack.last().copied() {
                Some((tok, optok, has_arg)) if lexeme.token == tok => {
                    if self.arg_mode {
                        self.produce_blank();
                    }
                    self.arg_mode = has_arg;
                    self.stack.pop();
                    self.produce(optok, lexeme.span);
                }
                top => {
                    if self.arg_mode {
                        if let Some((optok, has_arg)) = self.tok_to_prefix[lexeme.token] {
                            self.arg_mode = has_arg;
                            self.produce(optok, lexeme.span);
                        } else if let Some((optok, has_arg)) = self.tok_to_suffix[lexeme.token] {
                            self.arg_mode = has_arg;
                            self.produce_blank();
                            self.produce(optok, lexeme.span);
                        } else {
                            return Err(self.error(top, lexeme));
                        }
                    } else {
                        if let Some((optok, has_arg)) = self.tok_to_suffix[lexeme.token] {
                            self.arg_mode = has_arg;
                            self.produce(optok, lexeme.span);
                        } else if let Some((optok, has_arg)) = self.tok_to_prefix[lexeme.token] {
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
            return Err(ResolverError::IncompleteOp {
                op: optok,
                expected: tok,
                found: None,
            });
        }
        if self.arg_mode {
            self.produce_blank();
        }
        Ok(self.output)
    }
}
