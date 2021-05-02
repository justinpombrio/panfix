//! A general purpose lexer. You do not need to use this directly if you're using the `panfix`
//! parser.
//!
//! To construct one, you specify:
//!
//! - A token type
//! - Whitespace to be skipped
//! - String and regex patterns for the tokens
//!
//! The lexer can then split a source string into a stream of lexemes matching the tokens.
//!
//! For example, to match words and parentheses, you would write:
//!
//! ```
//! use panfix::lexing::{LexerBuilder, Token};
//!
//! #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
//! enum ExampleToken {
//!     Open,
//!     Close,
//!     Word,
//!     _LexError,
//! }
//! use ExampleToken::*;
//!
//! impl Token for ExampleToken {
//!     // Used in case of a lexing error
//!     const LEX_ERROR: ExampleToken = _LexError;
//! }
//!
//! let lexer = LexerBuilder::new()
//!     .unicode_whitespace()
//!     // Match the regex pattern.
//!     .regex("[a-zA-Z]+", Word)
//!     // Match the literal string. No need to escape special regex chars.
//!     .string("(", Open)
//!     .string(")", Close)
//!     .build()
//!     .unwrap();
//!
//! // Split the source into tokens.
//! let source = "((hello )\n  world)%";
//! let lexemes = lexer.lex(source);
//! let just_the_tokens = lexemes.map(|lexeme| lexeme.token);
//! assert_eq!(
//!     just_the_tokens.collect::<Vec<_>>(),
//!     vec![Open, Open, Word, Close, Word, Close, _LexError]);
//!
//! // Oops, we need more than just the tokens!
//! let mut lexemes = lexer.lex(source);
//! let hello_lexeme = lexemes.nth(2).unwrap();
//! assert_eq!(hello_lexeme.text(source), "hello");
//! ```
//!
//! ## Multiple matches
//!
//! What if more than one pattern matches? There are three tie-breakers, that happen in this order:
//!
//! 1. If one match is longer than another, the longer match is used.
//! 2. If there's a tie for length among a `.string()` pattern an a `.regex()` pattern, the
//!    `.string()` pattern is used.  This handles the common case where the text `true` could be
//!    considered either a variable, or a boolean constant: it should be a boolean.
//! 3. If there's _still_ a tie, the first pattern that matched is used. This shouldn't come up
//!    very often.
//!
//! ## Errors
//!
//! If none of the patterns match (i.e., the lexer encounters gibberish), it's indicated by a
//! lexeme with `lexeme.token == T::LEX_ERROR`.
//!
//! ## Side note: lexemes vs. tokens
//!
//! What's the difference between a token and a lexeme? You might hear these words used
//! interchangeably, but there's an important distinction they get at.
//!
//! A token is a class of substrings you want to match, and a lexeme is an _occurrence_ of a token
//! within the source.
//!
//! For example, this source text:
//!
//! ```text
//! 2 + 3
//! ```
//!
//! has two tokens `INT` and `PLUS`, and three lexemes `2`, `+`, and `3`.

mod lexer;
mod lexer_builder;
mod pattern;

use std::fmt::Debug;
use std::hash::Hash;

pub use lexer::Lexer;
pub use lexer_builder::{LexerBuilder, LexerBuilderError};

/// The start and end byte of a lexeme within the source string.
pub type Span = (usize, usize);

/// A particular occurrence of a token within the source text. Contains the token, plus the
/// span at which it was found in the source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}

impl<T: Token> Lexeme<T> {
    /// Get the source text of this lexeme.
    ///
    /// # Panics
    ///
    /// May panic if you pass it a `source` that isn't actually the source it was taken from.
    pub fn text(self, source: &str) -> &str {
        &source[self.span.0..self.span.1]
    }
}

/// The trait that tokens must obey. The lexer will produce a `LEX_ERROR` if none of the other
/// tokens match.
pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;
}

impl Token for &'static str {
    const LEX_ERROR: &'static str = "$LEX_ERROR";
}

impl Token for usize {
    const LEX_ERROR: usize = 0;
}

impl Token for u64 {
    const LEX_ERROR: u64 = 0;
}

impl Token for u32 {
    const LEX_ERROR: u32 = 0;
}

impl Token for u16 {
    const LEX_ERROR: u16 = 0;
}

impl Token for u8 {
    const LEX_ERROR: u8 = 0;
}
