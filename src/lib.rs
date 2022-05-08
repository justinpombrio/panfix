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
//! use panfix::{GrammarBuilder, Visitor, pattern};
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
//! let mut builder = GrammarBuilder::new_with_unicode_whitespace().unwrap();
//!
//! builder.atom_regex("Expr", "Number", "[0-9]+").unwrap();
//! builder.op("Expr", "Minus", 40, pattern!(_ "-" _)).unwrap();
//! builder.op("Expr", "Negative", 50, pattern!("-" _)).unwrap();
//!
//! let grammar = builder.finish().unwrap();
//! /*
//! assert_eq!(
//!     to_sexpr(grammar.parse("Expr", "1 - 2 - 3").unwrap().visitor()),
//!     "(Minus (Minus 1 2) 3)");
//! assert_eq!(grammar.parse("Expr", "- 2").unwrap().visitor().op(), "Negative");
//! */
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

mod grammar;
mod lexer;
mod op;
mod parse_tree;
mod parser;
mod rpn_visitor;

pub use grammar::{Grammar, GrammarBuilder, GrammarError, Pattern};
pub use op::{Fixity, Prec, Sort};
pub use parse_tree::{ParseTree, Visitor};
pub use parser::ParseError;

/// The lexer used internally by the parser. It's provided here in case you wish to use it
/// independently.
pub mod lexing {
    pub use crate::lexer::{Lexeme, Lexer, LexerBuilder, Position, Span, Token, LEX_ERROR};
}

/// An separate utility for constructing a "tree" with only a single allocation. It's used
/// internally by the parser, and provided here in case you wish to use it independently.
pub mod rpn {
    pub use crate::rpn_visitor::{RpnStack, RpnVisitor};
}

// TODO: docs
#[macro_export]
macro_rules! pattern {
    (_ $token:literal $($followers:tt)*) => {
        pattern!(@ Y $token [ ] $($followers)*)
    };

    ($token:literal $($followers:tt)*) => {
        pattern!(@ N $token [ ] $($followers)*)
    };

    (@ $l:ident $token:literal [ $($followers:tt)* ] $nt:ident $tok:literal $($rest:tt)*) => {
        pattern!(@ $l $token [ $($followers)* (std::stringify!($nt), $tok), ] $($rest)*)
    };

    (@ Y $token:literal [ $($followers:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::InfixL,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };

    (@ Y $token:literal [ $($followers:tt)* ] _ infixl) => {
        $crate::Pattern {
            fixity: $crate::Fixity::InfixL,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };

    (@ Y $token:literal [ $($followers:tt)* ] _ infixr) => {
        $crate::Pattern {
            fixity: $crate::Fixity::InfixR,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };

    (@ Y $token:literal [ $($followers:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Suffix,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };

    (@ N $token:literal [ $($followers:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Prefix,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };

    (@ N $token:literal [ $($followers:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Nilfix,
            first_token: $token,
            followers: vec![$($followers)*],
        }
    };
}
