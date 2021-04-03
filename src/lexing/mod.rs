mod lexer;
mod lexer_builder;
mod pattern;

use std::fmt::Debug;
use std::hash::Hash;

pub use lexer_builder::LexerBuilder;

pub type Span = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}

pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;

    fn as_usize(self) -> usize;
    fn max_usize(self) -> usize;
}
