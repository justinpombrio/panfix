use crate::lexer::{LexerBuilder, RegexError, UNICODE_WHITESPACE_REGEX};
use crate::op::{Assoc, Fixity, Op, Prec};
use crate::{OpTokenId, Parser, Token, TokenId, TOKEN_ERROR, TOKEN_JUXTAPOSE};
use thiserror::Error;

const PREC_DELTA: Prec = 10;
const JUXTAPOSE_PREC: Prec = 5;

/// A grammar for a language. Add operators until the grammar is complete, then call `.finish()` to
/// construct a `Parser` you can use to parse.
#[derive(Debug, Clone)]
pub struct Grammar<T: Token> {
    lexer_builder: LexerBuilder,
    // TokenId -> info about that token
    token_table: Vec<TokenInfo>,
    // OpTokenId -> info about that optoken
    op_token_table: Vec<OpTokenInfo<T>>,
    current_prec: Prec,
    current_assoc: Assoc,
}

#[derive(Debug, Clone)]
struct TokenInfo {
    name: String,
    as_prefix: Option<(OpTokenId, bool)>,
    as_suffix: Option<(OpTokenId, bool)>,
}

#[derive(Debug, Clone)]
struct OpTokenInfo<T: Token> {
    token: T,
    op: Option<Op<T>>,
    lprec: Prec,
    rprec: Prec,
    follower: Option<(TokenId, OpTokenId, bool)>,
}

/// An error while constructing a grammar.
#[derive(Error, Debug)]
pub enum GrammarError {
    /// Ambiguity: two operators were defined with the same starting token, and both are
    /// "prefixy": have no left argument.
    #[error(
        "Duplicate token usage. Each token can be used at most once with a left argument and at
        most once without a right argument. However the token {0} was used twice without a left
        argument."
    )]
    PrefixyConflict(String),
    /// Ambiguity: two operators were defined with the same starting token, and both are
    /// "suffixy": have a left argument.
    #[error(
        "Duplicate token usage. Each token can be used at most once with a left argument and at
        most once without a right argument. However the token {0} was used twice with a left
        argument."
    )]
    SuffixyConflict(String),
    /// Bad regex.
    #[error("Regex error in grammar. {0}")]
    RegexError(RegexError),
}

/// Describe the syntax of an operator. You typically want to construct this with the `pattern!`
/// macro.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    pub fixity: Fixity,
    pub tokens: Vec<String>,
}

impl<T: Token> Grammar<T> {
    /// An empty grammar, that uses Unicode's Pattern_White_Space for whitespace.
    pub fn new_with_unicode_whitespace() -> Result<Grammar<T>, GrammarError> {
        Grammar::new(UNICODE_WHITESPACE_REGEX)
    }

    /// An empty grammar. `whitespace_regex` is the regex to use to match whitespace, in the syntax
    /// of the `regex` crate.
    pub fn new(whitespace_regex: &str) -> Result<Grammar<T>, GrammarError> {
        use GrammarError::RegexError;

        let lexer_builder = LexerBuilder::new(whitespace_regex).map_err(RegexError)?;
        let juxt_op = Op::new_juxtapose(Assoc::Left, JUXTAPOSE_PREC);
        Ok(Grammar {
            // First three tokens: ERROR, BLANK, JUXTAPOSE
            lexer_builder,
            token_table: vec![
                TokenInfo {
                    name: T::LEX_ERROR.to_string(),
                    as_prefix: Some((TOKEN_ERROR, false)),
                    as_suffix: None,
                },
                TokenInfo {
                    name: T::BLANK.to_string(),
                    as_prefix: None,
                    as_suffix: None,
                },
                TokenInfo {
                    name: T::JUXTAPOSE.to_string(),
                    as_prefix: None,
                    as_suffix: None,
                },
            ],
            op_token_table: vec![
                OpTokenInfo {
                    token: T::LEX_ERROR,
                    op: Some(Op::new_error()),
                    lprec: 0,
                    rprec: 0,
                    follower: None,
                },
                OpTokenInfo {
                    token: T::BLANK,
                    op: Some(Op::new_blank()),
                    lprec: 0,
                    rprec: 0,
                    follower: None,
                },
                OpTokenInfo {
                    token: T::JUXTAPOSE,
                    lprec: juxt_op.left_prec.unwrap_or(0),
                    rprec: juxt_op.right_prec.unwrap_or(0),
                    op: Some(juxt_op),
                    follower: None,
                },
            ],
            current_prec: 10,
            current_assoc: Assoc::Left,
        })
    }

    /// Add a new group of operators. They will have higher precedence (i.e.  bind _looser_) than
    /// any of the groups added so far. Any infix operators in this group will be _left
    /// associative_.
    pub fn left_assoc(&mut self) {
        self.current_prec += PREC_DELTA;
        self.current_assoc = Assoc::Left;
    }

    /// Add a new group of operators. They will have higher precedence (i.e.  bind _looser_) than
    /// any of the groups added so far. Any infix operators in this group will be _left
    /// associative_.
    pub fn right_assoc(&mut self) {
        self.current_prec += PREC_DELTA;
        self.current_assoc = Assoc::Right;
    }

    /// Extend the grammar with an atom: when parsing, if `string_pattern` is found exactly, parse
    /// it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.string(JsonToken::Null, "null")`.
    pub fn string(&mut self, token: T, string_pattern: &str) -> Result<(), GrammarError> {
        let token_id = self.add_string_token(string_pattern)?;
        let op = Op::new_atom(token.clone(), token_id);
        self.add_op_token(Some(op), token_id, token, None, None, None)?;
        Ok(())
    }

    /// Extend the grammar with an atom: when parsing, if `regex_pattern` is matched, parse it as
    /// an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.atom_regex(JsonToken::Number, "[0-9]*")`
    /// (though with a better regex).
    pub fn regex(&mut self, token: T, regex_pattern: &str) -> Result<(), GrammarError> {
        let token_id = self.add_regex_token(token.clone(), regex_pattern)?;
        let op = Op::new_atom(token.clone(), token_id);
        self.add_op_token(Some(op), token_id, token, None, None, None)?;
        Ok(())
    }

    /// Extend the grammar with a "juxtapose" operator that is applied whenever two expressions are
    /// found next to each other with nothing to join them. For example, `myFunc 15` would be
    /// parsed as `myFunc JUXTAPOSE 15`. This is useful, for example, in languages where
    /// `myFunc 15` denotes function application.
    ///
    /// By default, every grammar has an implicit juxtapose operator with minimum precedence and
    /// left associativity. Every time you call this function, you overwrite the precedence the
    /// associativity of the juxtaposition operator (so the last call wins).
    pub fn juxtapose(&mut self) -> Result<(), GrammarError> {
        let (prec, assoc) = (self.current_prec, self.current_assoc);
        let op = Op::new_juxtapose(assoc, prec);
        let (lprec, rprec) = (op.left_prec, op.right_prec);
        let row = &mut self.op_token_table[TOKEN_JUXTAPOSE];
        row.op = Some(op);
        row.lprec = lprec.unwrap_or(0);
        row.rprec = rprec.unwrap_or(0);
        Ok(())
    }

    /// Extend the grammar with an operator. When parsing, if `pattern.tokens[0]` is found
    /// exactly, parse it as an operator with the given fixity, precedence, and followers.  For
    /// details on what all of those mean, see the [module level docs](`crate`).
    ///
    /// For example, a JSON grammar might have:
    /// ```no_run
    /// # use panfix::{Grammar, Fixity, pattern};
    /// # let mut grammar = Grammar::new("").unwrap();
    /// grammar.left_assoc();
    /// grammar.op("comma", pattern!(_ "," _));
    /// grammar.left_assoc();
    /// grammar.op("colon", pattern!(_ ":" _));
    /// ```
    pub fn op(&mut self, token: T, pattern: Pattern) -> Result<(), GrammarError> {
        if pattern.fixity == Fixity::Nilfix {
            self.add_op(token, Assoc::Left, 0, pattern.fixity, pattern.tokens)
        } else {
            let (prec, assoc) = (self.current_prec, self.current_assoc);
            self.add_op(token, assoc, prec, pattern.fixity, pattern.tokens)
        }
    }

    /// Ignore the builder pattern and nice abstractions that `Grammar` otherwise uses, and add
    /// an op into the table exactly as specified.
    pub fn add_raw_op(
        &mut self,
        token: T,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
        tokens: Vec<String>,
    ) -> Result<(), GrammarError> {
        self.add_op(token, assoc, prec, fixity, tokens)
    }

    pub fn finish(self) -> Result<Parser<T>, GrammarError> {
        let lexer = self
            .lexer_builder
            .finish()
            .map_err(GrammarError::RegexError)?;
        let mut tok_to_name = vec![];
        let mut tok_to_prefix = vec![];
        let mut tok_to_suffix = vec![];
        let mut optok_to_token = vec![];
        let mut optok_to_follower = vec![];
        let mut optok_to_op = vec![];
        let mut optok_to_prec = vec![];
        for row in self.token_table {
            tok_to_name.push(row.name);
            tok_to_prefix.push(row.as_prefix);
            tok_to_suffix.push(row.as_suffix);
        }
        for row in self.op_token_table {
            optok_to_token.push(row.token);
            optok_to_follower.push(row.follower);
            optok_to_op.push(row.op);
            optok_to_prec.push((row.lprec, row.rprec));
        }
        Ok(Parser {
            lexer,
            tok_to_name,
            tok_to_prefix,
            tok_to_suffix,
            optok_to_token,
            optok_to_follower,
            optok_to_op,
            optok_to_prec,
        })
    }

    fn add_op(
        &mut self,
        token: T,
        assoc: Assoc,
        prec: Prec,
        fixity: Fixity,
        tokens: Vec<String>,
    ) -> Result<(), GrammarError> {
        let maxprec = Some(Prec::MAX);

        let token_id = self.add_string_token(&tokens[0])?;
        let op = Op::new(token.clone(), fixity, assoc, prec, tokens.len());
        let (lprec, rprec) = (op.left_prec, op.right_prec);
        if tokens.len() == 1 {
            self.add_op_token(Some(op), token_id, token, lprec, rprec, None)?;
        } else {
            let patt = tokens.last().unwrap();
            let token_id = self.add_string_token(patt)?;
            let optok =
                self.add_op_token(None, token_id, token.clone(), Some(Prec::MAX), rprec, None)?;
            let mut follower = (token_id, optok, rprec.is_some());
            for patt in tokens.iter().skip(1).rev().skip(1) {
                let token_id = self.add_string_token(patt)?;
                let optok = self.add_op_token(
                    None,
                    token_id,
                    token.clone(),
                    maxprec,
                    maxprec,
                    Some(follower),
                )?;
                follower = (token_id, optok, true);
            }
            let patt = tokens.first().unwrap();
            let token_id = self.add_string_token(patt)?;
            self.add_op_token(Some(op), token_id, token, lprec, maxprec, Some(follower))?;
        }
        Ok(())
    }

    fn add_string_token(&mut self, string: &str) -> Result<TokenId, GrammarError> {
        let token_id = match self.lexer_builder.string(string) {
            Ok(token_id) => token_id,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.insert_token(token_id, string.to_owned());
        Ok(token_id)
    }

    fn add_regex_token(&mut self, token: T, regex_pattern: &str) -> Result<TokenId, GrammarError> {
        let token_id = match self.lexer_builder.regex(regex_pattern) {
            Ok(token_id) => token_id,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.insert_token(token_id, token.to_string());
        Ok(token_id)
    }

    fn insert_token(&mut self, token_id: TokenId, name: String) {
        if token_id == self.token_table.len() {
            self.token_table.push(TokenInfo {
                name,
                as_prefix: None,
                as_suffix: None,
            });
        } else {
            let row = &self.token_table[token_id];
            assert_eq!(row.name, name, "Duplicate token {}", name);
        }
    }

    fn add_op_token(
        &mut self,
        op: Option<Op<T>>,
        token_id: TokenId,
        token: T,
        lprec: Option<Prec>,
        rprec: Option<Prec>,
        follower: Option<(TokenId, OpTokenId, bool)>,
    ) -> Result<OpTokenId, GrammarError> {
        let op_token = self.op_token_table.len();
        if op.is_some() {
            if lprec.is_none() {
                if self.token_table[token_id].as_prefix.is_some() {
                    return Err(GrammarError::PrefixyConflict(
                        self.token_table[token_id].name.clone(),
                    ));
                }
                self.token_table[token_id].as_prefix = Some((op_token, rprec.is_some()));
            } else {
                if self.token_table[token_id].as_suffix.is_some() {
                    return Err(GrammarError::SuffixyConflict(
                        self.token_table[token_id].name.clone(),
                    ));
                }
                self.token_table[token_id].as_suffix = Some((op_token, rprec.is_some()));
            }
        }
        self.op_token_table.push(OpTokenInfo {
            token,
            op,
            lprec: lprec.unwrap_or(0),
            rprec: rprec.unwrap_or(0),
            follower,
        });
        Ok(op_token)
    }
}
