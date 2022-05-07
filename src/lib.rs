// TODO:
// - Thorough testing
// - Method to display the grammar as a pretty table, given a way to display a token
// - Think about what happens if a starting token is also a follower token in the same subgrammar.

//! # Panfix
//!
//! Panfix is a new approach to parsing, using a modified version of [operator precedence
//! grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar):
//!
//! - It is not a CFG parser nor a PEG parser, it's something new.
//! - It runs in linear time. (Not like PEG Packrat O(NG), this is O(N).)
//! - It has both "soft" and "hard" parse errors, and will continue parsing after any number of
//!   "soft" errors, but stops on the first "hard" error.
//! - It's quite simple to implement.
//!
//! [FILL]
//!
//! This crate is incomplete, and in a messy state. For now, you should use the reference
//! implementation:
//!
//! [**Reference Implementation**](refn_impl)
//!
//! Below is information about the (incomplete!) more robust/efficient implementation.
//!
//! ------------------------
//!
//! The Panfix parser is here. It is probably what you want to view:
//!
//! [**☞ The Panfix Parser**](parsing)
//!
//! The parser is made up of three pieces. If you want more control over the parsing process,
//! you can use or replace any combination of these pieces (they are all independent). Here are the
//! pieces, in the order that they run in the parsing pipeline:
//!
//! 1. [Lexing](lexing)
//! 2. [Shunting](shunting)
//! 3. [Visiting](rpn_visitor)

mod grammar;
mod lexer;
mod op;
mod parse_tree;
mod parser;
mod rpn_visitor;

pub use grammar::{Grammar, GrammarBuilder, GrammarError};
pub use op::{Assoc, Fixity, Prec};
pub use parse_tree::{ParseTree, Visitor};
pub use parser::{parse_lexeme_stream, ParseError};

pub mod lexing {
    pub use crate::lexer::{Lexer, LexerBuilder};
}

pub mod rpn {
    pub use crate::rpn_visitor::{RpnStack, RpnVisitor};
}
