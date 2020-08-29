use std::fmt::Debug;
use std::hash::Hash;

pub mod lexer;
pub mod parser;
pub mod rpn_visitor;

pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;

    fn as_usize(self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: (usize, usize),
}
