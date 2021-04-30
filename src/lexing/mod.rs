mod lexer;
mod lexer_builder;
mod pattern;

use std::fmt::Debug;
use std::hash::Hash;

pub use lexer::Lexer;
pub use lexer_builder::{LexerBuilder, LexerBuilderError};
pub use pattern::Pattern;

pub type Span = (usize, usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}

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
