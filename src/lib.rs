// TODO:
// - Thorough testing
// - Method to display the grammar as a pretty table, given a way to display a token
// - Think about what happens if a starting token is also a follower token in the same subgrammar.

// TODO: temporary
#![allow(unused)]
#![allow(clippy::diverging_sub_expression)]

mod blank_inserter;
mod grammar;
mod lexer;
mod op;
mod op_resolver;
mod parse_error;
mod parse_tree;
mod shunter;
mod source;
mod tree_visitor;

use lexer::Lexer;
use op::Op;
use std::fmt;

pub use grammar::{Grammar, GrammarError, Pattern};
pub use op::{Fixity, Prec};
pub use parse_error::ParseError;
pub use parse_tree::{ParseTree, Visitor};
pub use shunter::shunt;
pub use source::Source;

/// A category of lexeme, such as "INTEGER" or "VARIABLE" or "OPEN_PAREN". The special Token called
/// [`TOKEN_ERROR`] represents a lexing error.
pub type Token = usize;
type OpToken = Token;

/// Represents a lexing error.
pub const TOKEN_ERROR: Token = 0;
/// Represents a missing argument.
pub const TOKEN_BLANK: Token = 1;
/// Represents a missing operator.
pub const TOKEN_JUXTAPOSE: Token = 2;

const NAME_ERROR: &str = "LexError";
const NAME_BLANK: &str = "Blank";
const NAME_JUXTAPOSE: &str = "Juxtapose";

/// One "word" in the stream returned by the lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

pub type Offset = usize;
pub type Line = u32;
pub type Col = u32;

/// A position in the source text. Positions are _between_ characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    /// Line number. Zero-indexed.
    pub line: Line,
    /// Column number, counted in bytes. Zero-indexed.
    pub col: Col,
    /// Column number, counted in utf8 codepoints. Zero-indexed.
    pub utf8_col: Col,
}

/// A start and end position in the source text. Positions are _between_ characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    token_names: Vec<String>,
    op_token_names: Vec<String>,
    prefixy_tokens: Vec<Option<(OpToken, bool)>>,
    suffixy_tokens: Vec<Option<(OpToken, bool)>>,
    op_table: Vec<Option<OpToken>>,
    follower_tokens: Vec<Option<(Token, OpToken)>>,
    prec_table: Vec<(Prec, Prec)>,
    ops: Vec<Option<Op>>,
}

impl Parser {
    /// Parse `source`. Runs in linear time.
    pub fn parse<'s, 'g>(&'g self, source: &'s Source) -> Result<ParseTree<'s, 'g>, ()> {
        use blank_inserter::insert_blanks;
        use op_resolver::resolve;
        use parse_tree::Item;
        use std::iter::FromIterator;
        use tree_visitor::Forest;

        let stream = self.lexer.lex(source.source());
        let stream = resolve(&self.op_table, &self.follower_tokens, stream);
        let stream = insert_blanks(&self.prefixy_tokens, &self.suffixy_tokens, stream);
        let stream = shunt(&self.prec_table, stream);
        let stream = stream.filter_map(|lexeme| match self.ops[lexeme.token] {
            None => None,
            Some(op) => Some(Item {
                op,
                span: lexeme.span,
            }),
        });
        let forest = Forest::from_iter(stream);
        Ok(ParseTree::new(source, forest))
    }

    fn arity(&self, op_token: OpToken) -> usize {
        let (lprec, rprec) = self.prec_table[op_token];
        (lprec != 0) as usize + (rprec != 0) as usize
    }

    fn name(&self, op_token: OpToken) -> &str {
        &self.token_names[op_token]
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.utf8_col)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl Position {
    /// The position at the start of any document.
    pub fn start() -> Position {
        Position {
            line: 0,
            col: 0,
            utf8_col: 0,
        }
    }

    /// Assuming that `ch` appears just to the right of this position, return the position just
    /// after `ch`.
    pub fn advance_by_char(self, ch: char) -> Position {
        if ch == '\n' {
            Position {
                line: self.line + 1,
                col: 0,
                utf8_col: 0,
            }
        } else {
            Position {
                line: self.line,
                col: self.col + ch.len_utf8() as Col,
                utf8_col: self.utf8_col + 1,
            }
        }
    }
}

impl Lexeme {
    pub fn new(token: Token, start: Position, end: Position) -> Lexeme {
        Lexeme {
            token,
            span: Span { start, end },
        }
    }
}

#[macro_export]
macro_rules! pattern {
    (_ $($items:tt)*) => {
        pattern!(@ Y [ ] $($items)*)
    };

    ($tok:literal $($items:tt)*) => {
        pattern!(@ N [ ] $tok $($items)*)
    };

    (@ $l:ident [ $($tokens:tt)* ] $tok:literal $($rest:tt)*) => {
        pattern!(@ $l [ $($tokens)* $tok.to_string(), ] $($rest)*)
    };

    (@ Y [ $($tokens:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Infix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ Y [ $($tokens:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Suffix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ N [ $($tokens:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Prefix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ N [ $($tokens:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Nilfix,
            tokens: vec![$($tokens)*],
        }
    };
}

// TODO: private?
pub mod lexing {
    pub use super::lexer::*;
}

/*
//! # Panfix
//!
//! Panfix is a new approach to parsing, using a modified version of [operator precedence
//! grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar):
//!
//! - It is not a CFG parser nor a PEG parser, it's something new.
//! - It runs in linear time. (O(N), to be precise, not O(NG) like PEG packrat parsing.)
//! - It has *very* clear error cases both around constructing grammars and parsing.
//! - It's quite simple to implement.
//!
//! The main question is how expressive it is. You'll find examples of things you might want to
//! parse, and how to parse them, in the [Examples](#examples) section below. If you encounter
//! something that you have difficulty parsing with this approach, please open an issue to let me
//! know!
//!
//! ## Overview
//!
//!
//!
//! [FILL]
//!
//! ## Examples
//!
//! ### Unary vs. Binary Minus
//!
//! Two operators are allowed to start with the same token, as long as one of them takes a left
//! argument and the other does not. This let's us parse both unary and binary minus:
//!
//! [TODO]
//! ```
//! use panfix::{Grammar, Visitor, pattern};
//!
//! fn to_sexpr(visitor: Visitor) -> String {
//!     if visitor.num_children() == 0 {
//!         visitor.source().to_string()
//!     } else {
//!         let mut sexpr = "(".to_string();
//!         sexpr.push_str(visitor.op());
//!         for child in visitor.children() {
//!             sexpr.push_str(" ");
//!             sexpr.push_str(&to_sexpr(child));
//!         }
//!         sexpr
//!     }
//! }
//!
//! let mut grammar = Grammar::new_with_unicode_whitespace().unwrap();
//! ```
//!
//! ## Spec
//!
//! ## Independent Modules
//!
//! This crate has two modules used by the parser, that could be used indepdendently of parsing:
//!
//! - [`lexing`] is the lexer used by the parser.
//! - [`rpn`] is used by the parser to store the parse tree with only a single allocation.
*/
