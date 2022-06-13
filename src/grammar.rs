use crate::lexer::{LexerBuilder, RegexError, UNICODE_WHITESPACE_REGEX};
use crate::op::{Assoc, Fixity, Op, Prec};
use crate::{
    OpToken, Parser, Token, NAME_BLANK, NAME_ERROR, NAME_JUXTAPOSE, TOKEN_BLANK, TOKEN_ERROR,
    TOKEN_JUXTAPOSE,
};
use thiserror::Error;

const PREC_DELTA: Prec = 10;
const JUXTAPOSE_PREC: Prec = 10;

/// A grammar for a language. Add operators until the grammar is complete, then call `.finish()` to
/// construct a `Parser` you can use to parse.
#[derive(Debug, Clone)]
pub struct Grammar {
    lexer_builder: LexerBuilder,
    // Token -> user-facing name
    token_names: Vec<String>,
    // Token -> Option<(OpToken, has_right_arg)>
    prefixy_tokens: Vec<Option<(OpToken, bool)>>,
    // Token -> Option<(OpToken, has_right_arg)>
    suffixy_tokens: Vec<Option<(OpToken, bool)>>,
    // Token -> Option<OpToken that starts with that token>
    op_table: Vec<Option<OpToken>>,
    // OpToken -> Option<(next token, next optoken)>
    follower_tokens: Vec<Option<(Token, OpToken)>>,
    // OpToken -> user-facing name
    op_token_names: Vec<String>,
    // OpToken -> (prec, prec)
    prec_table: Vec<(Prec, Prec)>,
    // OpToken -> Op that starts with that token
    ops: Vec<Option<Op>>,
    current_prec: Prec,
    current_assoc: Assoc,
}

/// An error while constructing a grammar.
#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Duplicate operators. Operators in a must start with distinct tokens, unless one is Prefix or Nilfix and the other is Suffix or Infix. This rule was broken by the oeprators {op_1:?} and {op_2:?}.")]
    DuplicateOp { op_1: String, op_2: String },
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
        Ok(Grammar {
            // First three tokens: ERROR, BLANK, JUXTAPOSE
            lexer_builder,
            token_names: vec![
                NAME_ERROR.to_owned(),
                NAME_BLANK.to_owned(),
                NAME_JUXTAPOSE.to_owned(),
            ],
            op_token_names: vec![
                NAME_ERROR.to_owned(),
                NAME_BLANK.to_owned(),
                NAME_JUXTAPOSE.to_owned(),
            ],
            prefixy_tokens: vec![Some((TOKEN_ERROR, false)), None, None],
            suffixy_tokens: vec![None, None, None],
            op_table: vec![Some(TOKEN_ERROR), Some(TOKEN_BLANK), Some(TOKEN_JUXTAPOSE)],
            follower_tokens: vec![None, None, None],
            prec_table: vec![(0, 0), (0, 0), (JUXTAPOSE_PREC, JUXTAPOSE_PREC)],
            ops: vec![None, None, None],
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
        self.add_op_token(
            Some(op),
            NAME_JUXTAPOSE,
            TOKEN_JUXTAPOSE,
            lprec,
            rprec,
            None,
        );
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
            token_names: self.token_names,
            op_token_names: self.op_token_names,
            prefixy_tokens: self.prefixy_tokens,
            suffixy_tokens: self.suffixy_tokens,
            op_table: self.op_table,
            follower_tokens: self.follower_tokens,
            prec_table: self.prec_table,
            ops: self.ops,
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
        let op = Op::new(name, pattern.fixity, assoc, prec, pattern.tokens.len());
        let (lprec, rprec) = (op.left_prec, op.right_prec);
        if pattern.tokens.len() == 1 {
            self.add_op_token(Some(op), name, token, lprec, rprec, None)?;
        } else {
            let patt = pattern.tokens.last().unwrap();
            let token = self.add_string_token(patt)?;
            let optok = self.add_op_token(None, patt, token, Some(Prec::MAX), rprec, None)?;
            let mut follower: (Token, OpToken) = (token, optok);
            for patt in pattern.tokens.iter().skip(1).rev().skip(1) {
                let token = self.add_string_token(patt)?;
                let optok =
                    self.add_op_token(None, patt, token, maxprec, maxprec, Some(follower))?;
                follower = (token, optok);
            }
            let patt = pattern.tokens.first().unwrap();
            let token = self.add_string_token(patt)?;
            self.add_op_token(Some(op), patt, token, lprec, maxprec, Some(follower))?;
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
        self.token_names.push(name.to_owned());
        self.prefixy_tokens.push(None);
        self.suffixy_tokens.push(None);
        self.op_table.push(None);
        self.follower_tokens.push(None);
    }

    fn add_op_token(
        &mut self,
        op: Option<Op>,
        name: &str,
        token: Token,
        lprec: Option<Prec>,
        rprec: Option<Prec>,
        follower: Option<(Token, OpToken)>,
    ) -> Result<OpToken, GrammarError> {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let op_token = self.ops.len();
        self.op_table[token] = Some(op_token);
        self.ops.push(op);
        self.op_token_names.push(name.to_owned());
        if lprec.is_none() {
            if self.prefixy_tokens[token].is_some() {
                return Err(GrammarError::PrefixyConflict {
                    token: self.token_names[token].clone(),
                });
            }
            self.prefixy_tokens[token] = Some((op_token, rprec.is_some()));
        } else {
            if self.suffixy_tokens[token].is_some() {
                return Err(GrammarError::SuffixyConflict {
                    token: self.token_names[token].clone(),
                });
            }
            self.suffixy_tokens[token] = Some((op_token, rprec.is_some()));
        }
        self.follower_tokens.push(follower);
        self.prec_table
            .push((lprec.unwrap_or(0), rprec.unwrap_or(0)));
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
