use crate::lexer::{LexerBuilder, RegexError, UNICODE_WHITESPACE_REGEX};
use crate::op::{Assoc, Fixity, Op, Prec};
use crate::op_resolver::Follower;
use crate::{Token, TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE};
use std::collections::HashMap;
use thiserror::Error;

type OpToken = Token;

const PREC_DELTA: Prec = 10;

/// A grammar for a language. Add operators until the grammar is complete, then call `.finish()` to
/// construct a `Parser` you can use to parse.
#[derive(Debug, Clone)]
pub struct Grammar {
    lexer_builder: LexerBuilder,
    // Token -> Option<OpToken>
    op_table: Vec<Option<OpToken>>,
    // OpToken -> Op
    ops: Vec<Op>,
    // OpToken -> Option<(OpToken, has_right_arg)>
    prefixy_tokens: Vec<Option<(Token, bool)>>,
    // OpToken -> Option<(OpToken, has_right_arg)>
    suffixy_tokens: Vec<Option<(Token, bool)>>,
    // OpToken -> (prec, prec)
    prec_table: Vec<(Prec, Prec)>,
    // OpToken -> Option<(Token, OpToken)> for follower
    follower_table: Vec<Option<Follower>>,

    next_op_token: OpToken,
    token_names: HashMap<Token, String>,

    current_prec: Prec,
    current_assoc: Assoc,
}

/// An error while constructing a grammar.
#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Duplicate operators. Operators in a must start with distinct tokens, unless one is Prefix or Nilfix and the other is Suffix or Infix. This rule was broken by the oeprators {op_1:?} and {op_2:?}.")]
    DuplicateOp { op_1: String, op_2: String },
    #[error("Regex error in grammar. {0}")]
    RegexError(RegexError),
    #[error("Grammar error: you must call `group()` before adding operators.")]
    PrecNotSet,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern<'a> {
    pub fixity: Fixity,
    pub first_token: &'a str,
    pub followers: Vec<(&'a str, &'a str)>,
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
        let mut token_names = HashMap::new();
        token_names.insert(TOKEN_BLANK, "_".to_owned());
        token_names.insert(TOKEN_JUXTAPOSE, "_".to_owned());
        Ok(Grammar {
            // First three tokens: ERROR, BLANK, JUXTAPOSE
            lexer_builder,
            prefixy_tokens: vec![Some((TOKEN_ERROR, false)), None, None],
            suffixy_tokens: vec![None, None, None],
            prec_table: vec![(0, 0), (0, 0), (10, 10)],
            op_table: vec![],
            ops: vec![],
            follower_table: vec![None, None, None],

            next_op_token: 2,
            token_names,

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

    /*
    /// Extend the grammar with an atom: when parsing the given `sort`, if `string_pattern` is
    /// found exactly, parse it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.atom_string("value", "Null" "null")`.
    pub fn string(&mut self, name: &str, string_pattern: &str) -> Result<(), GrammarError> {
        let token = self.add_string_token(string_pattern)?;
        let op = Op::new_atom(name, token);
        let sort_id = self.get_sort_id()?;
        self.add_token(token);
    }

    // Token -> Option<OpToken>
    op_table: Vec<Option<OpToken>>,
    // OpToken -> Op
    ops: Vec<Op>,
    // OpToken -> Option<(OpToken, has_right_arg)>
    prefixy_tokens: Vec<Option<(Token, bool)>>,
    // OpToken -> Option<(OpToken, has_right_arg)>
    suffixy_tokens: Vec<Option<(Token, bool)>>,
    // OpToken -> (prec, prec)
    prec_table: Vec<(Prec, Prec)>,
    // OpToken -> Option<(Token, OpToken)> for follower
    follower_table: Vec<Option<Follower>>,
    */
}
