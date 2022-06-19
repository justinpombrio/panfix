use crate::lexer::{LexerBuilder, RegexError, UNICODE_WHITESPACE_REGEX};
use crate::op::{Assoc, Fixity, Op, Prec};
use crate::{
    OpToken, OpTokenInfo, Parser, Token, TokenInfo, NAME_BLANK, NAME_ERROR, NAME_JUXTAPOSE,
    TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE,
};
use std::fmt;
use thiserror::Error;

const PREC_DELTA: Prec = 10;
const JUXTAPOSE_PREC: Prec = 5;

/// A grammar for a language. Add operators until the grammar is complete, then call `.finish()` to
/// construct a `Parser` you can use to parse.
#[derive(Debug, Clone)]
pub struct Grammar {
    lexer_builder: LexerBuilder,
    // Token -> info about that token
    token_table: Vec<TokenInfo>,
    // OpToken -> info about that optoken
    op_token_table: Vec<OpTokenInfo>,
    current_prec: Prec,
    current_assoc: Assoc,
}

/// An error while constructing a grammar.
#[derive(Error, Debug)]
pub enum GrammarError {
    #[error(
        "Duplicate token usage. Each token can be used at most once with a left argument and at
        most once without a right argument. However the token {token} was used without a left
        argument."
    )]
    PrefixyConflict { token: String },
    #[error(
        "Duplicate token usage. Each token can be used at most once with a left argument and at
        most once without a right argument. However the token {token} was used with a left
        argument."
    )]
    SuffixyConflict { token: String },
    #[error("Regex error in grammar. {0}")]
    RegexError(RegexError),
    #[error("Grammar error: you must call `group()` before adding operators.")]
    PrecNotSet,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern {
    pub fixity: Fixity,
    pub tokens: Vec<String>,
}

impl Grammar {
    /// An empty grammar, that uses Unicode's Pattern_White_Space for whitespace.
    pub fn new_with_unicode_whitespace() -> Result<Grammar, GrammarError> {
        Grammar::new(UNICODE_WHITESPACE_REGEX)
    }

    /// An empty grammar. `whitespace_regex` is the regex to use to match whitespace, in the syntax
    /// of the `regex` crate.
    pub fn new(whitespace_regex: &str) -> Result<Grammar, GrammarError> {
        use GrammarError::RegexError;

        let lexer_builder = LexerBuilder::new(whitespace_regex).map_err(RegexError)?;
        let juxt_op = Op::new_juxtapose(Assoc::Left, JUXTAPOSE_PREC);
        Ok(Grammar {
            // First three tokens: ERROR, BLANK, JUXTAPOSE
            lexer_builder,
            token_table: vec![
                TokenInfo {
                    name: NAME_ERROR.to_owned(),
                    as_prefix: Some((TOKEN_ERROR, false)),
                    as_suffix: None,
                },
                TokenInfo {
                    name: NAME_BLANK.to_owned(),
                    as_prefix: None,
                    as_suffix: None,
                },
                TokenInfo {
                    name: NAME_JUXTAPOSE.to_owned(),
                    as_prefix: None,
                    as_suffix: None,
                },
            ],
            op_token_table: vec![
                OpTokenInfo {
                    name: NAME_ERROR.to_owned(),
                    op: None,
                    lprec: 0,
                    rprec: 0,
                    follower: None,
                },
                OpTokenInfo {
                    name: NAME_BLANK.to_owned(),
                    op: Some(Op::new_blank()),
                    lprec: 0,
                    rprec: 0,
                    follower: None,
                },
                OpTokenInfo {
                    name: NAME_JUXTAPOSE.to_owned(),
                    lprec: juxt_op.left_prec.unwrap_or(0),
                    rprec: juxt_op.right_prec.unwrap_or(0),
                    op: Some(juxt_op),
                    follower: None,
                },
            ],
            current_prec: 0,
            current_assoc: Assoc::Left,
        })
    }

    /// Add a new group of operators. They will have higher precedence (i.e.  bind _looser_) than
    /// any of the groups added so far. Any infix operators in this group will be _left
    /// associative_.
    pub fn lgroup(&mut self) {
        self.current_prec += PREC_DELTA;
        self.current_assoc = Assoc::Left;
    }

    /// Add a new group of operators. They will have higher precedence (i.e.  bind _looser_) than
    /// any of the groups added so far. Any infix operators in this group will be _left
    /// associative_.
    pub fn rgroup(&mut self) {
        self.current_prec += PREC_DELTA;
        self.current_assoc = Assoc::Right;
    }

    /// Extend the grammar with an atom: when parsing, if `string_pattern` is found exactly, parse
    /// it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.string("value", "null")`.
    pub fn string(&mut self, name: &str, string_pattern: &str) -> Result<(), GrammarError> {
        let token = self.add_string_token(string_pattern)?;
        let op = Op::new_atom(name, token);
        self.add_op_token(Some(op), name, token, None, None, None);
        Ok(())
    }

    /// Extend the grammar with an atom: when parsing, if `regex_pattern` is matched, parse it as
    /// an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.atom_regex("value", "[0-9]*")` (though with
    /// a better regex).
    pub fn regex(&mut self, name: &str, regex_pattern: &str) -> Result<(), GrammarError> {
        let token = self.add_regex_token(regex_pattern, name)?;
        let op = Op::new_atom(name, token);
        self.add_op_token(Some(op), name, token, None, None, None);
        Ok(())
    }

    // TODO: docs
    pub fn juxtapose(&mut self) -> Result<(), GrammarError> {
        let (prec, assoc) = self.get_prec_and_assoc()?;
        let op = Op::new_juxtapose(assoc, prec);
        let (lprec, rprec) = (op.left_prec, op.right_prec);
        println!("?? {:?}/{:?}", lprec, rprec);
        let mut row = &mut self.op_token_table[TOKEN_JUXTAPOSE];
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
    /// grammar.lgroup();
    /// grammar.op("comma", pattern!(_ "," _));
    /// grammar.lgroup();
    /// grammar.op("colon", pattern!(_ ":" _));
    /// ```
    pub fn op(&mut self, name: &str, pattern: Pattern) -> Result<(), GrammarError> {
        if pattern.fixity == Fixity::Nilfix {
            self.add_op(name, Assoc::Left, 0, pattern)
        } else {
            let (prec, assoc) = self.get_prec_and_assoc()?;
            self.add_op(name, assoc, prec, pattern)
        }
    }

    /// Ignore the builder pattern and nice abstractions that `Grammar` otherwise uses, and add
    /// an op into the table exactly as specified.
    pub fn add_raw_op(
        &mut self,
        name: &str,
        assoc: Assoc,
        prec: Prec,
        pattern: Pattern,
    ) -> Result<(), GrammarError> {
        self.add_op(name, assoc, prec, pattern)
    }

    pub fn finish(self) -> Result<Parser, GrammarError> {
        Ok(Parser {
            lexer: self
                .lexer_builder
                .finish()
                .map_err(GrammarError::RegexError)?,
            token_table: self.token_table,
            prec_table: self
                .op_token_table
                .iter()
                .map(|row| (row.lprec, row.rprec))
                .collect(),
            op_token_table: self.op_token_table,
        })
    }

    fn add_op(
        &mut self,
        name: &str,
        assoc: Assoc,
        prec: Prec,
        pattern: Pattern,
    ) -> Result<(), GrammarError> {
        let maxprec = Some(Prec::MAX);

        let token = self.add_string_token(&pattern.tokens[0])?;
        let op = Op::new(name, pattern.fixity, assoc, prec, pattern.tokens.clone());
        let (lprec, rprec) = (op.left_prec, op.right_prec);
        if pattern.tokens.len() == 1 {
            self.add_op_token(Some(op), name, token, lprec, rprec, None)?;
        } else {
            let patt = pattern.tokens.last().unwrap();
            let token = self.add_string_token(patt)?;
            let optok = self.add_op_token(None, name, token, Some(Prec::MAX), rprec, None)?;
            let mut follower = (token, optok, rprec.is_some());
            for patt in pattern.tokens.iter().skip(1).rev().skip(1) {
                let token = self.add_string_token(patt)?;
                let optok =
                    self.add_op_token(None, name, token, maxprec, maxprec, Some(follower))?;
                follower = (token, optok, true);
            }
            let patt = pattern.tokens.first().unwrap();
            let token = self.add_string_token(patt)?;
            self.add_op_token(Some(op), name, token, lprec, maxprec, Some(follower))?;
        }
        Ok(())
    }

    fn add_string_token(&mut self, string: &str) -> Result<Token, GrammarError> {
        let token = match self.lexer_builder.string(string) {
            Ok(token) => token,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.insert_token(token, string);
        Ok(token)
    }

    fn add_regex_token(&mut self, regex_pattern: &str, name: &str) -> Result<Token, GrammarError> {
        let token = match self.lexer_builder.regex(regex_pattern) {
            Ok(token) => token,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.insert_token(token, name);
        Ok(token)
    }

    fn insert_token(&mut self, token: Token, name: &str) {
        if token == self.token_table.len() {
            self.token_table.push(TokenInfo {
                name: name.to_owned(),
                as_prefix: None,
                as_suffix: None,
            });
        } else {
            let row = &self.token_table[token];
            assert_eq!(&row.name, name, "Duplicate token {}", name);
        }
    }

    fn add_op_token(
        &mut self,
        op: Option<Op>,
        name: &str,
        token: Token,
        lprec: Option<Prec>,
        rprec: Option<Prec>,
        follower: Option<(Token, OpToken, bool)>,
    ) -> Result<OpToken, GrammarError> {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let op_token = self.op_token_table.len();
        if op.is_some() {
            if lprec.is_none() {
                if self.token_table[token].as_prefix.is_some() {
                    return Err(GrammarError::PrefixyConflict {
                        token: self.token_table[token].name.clone(),
                    });
                }
                self.token_table[token].as_prefix = Some((op_token, rprec.is_some()));
            } else {
                if self.token_table[token].as_suffix.is_some() {
                    return Err(GrammarError::SuffixyConflict {
                        token: self.token_table[token].name.clone(),
                    });
                }
                self.token_table[token].as_suffix = Some((op_token, rprec.is_some()));
            }
        }
        self.op_token_table.push(OpTokenInfo {
            name: name.to_owned(),
            op,
            lprec: lprec.unwrap_or(0),
            rprec: rprec.unwrap_or(0),
            follower,
        });
        Ok(op_token)
    }

    fn get_prec_and_assoc(&self) -> Result<(Prec, Assoc), GrammarError> {
        if self.current_prec > 0 {
            Ok((self.current_prec, self.current_assoc))
        } else {
            Err(GrammarError::PrecNotSet)
        }
    }
}
