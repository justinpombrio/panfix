mod lexer;
mod lexer_builder;
mod pattern;

use std::fmt::Debug;

pub use lexer_builder::LexerBuilder;

pub trait Token: Debug + Clone + Copy + PartialEq + Eq {
    const LEX_ERROR: Self;
}

impl Token for usize {
    const LEX_ERROR: usize = 0;
}
