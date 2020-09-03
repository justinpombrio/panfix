use std::fmt::Debug;
use std::hash::Hash;

// TODO:
// - Multifix
// - ParsrConstructionError
// - Handle lexing errors
// - Proper Source data structure, with line numbers
// - Put builder in submodule
// - Eliminate rpn_visitor dependency

pub mod lexer;
pub mod rpn_visitor;
pub mod shunter;

pub type Span = (usize, usize);

pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;
    const MISSING_ATOM: Self;
    const MISSING_SEP: Self;
    const EXTRA_SEP: Self;
    const JUXTAPOSE: Self;

    fn as_usize(self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}
