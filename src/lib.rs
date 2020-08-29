use std::fmt::Debug;
use std::hash::Hash;

pub mod lexer;
mod parser;
pub mod rpn_visitor;

pub use lexer::{Lexer, LexerBuilder};
pub use parser::{parse, Grammar, GrammarBuilder, Node};
pub use rpn_visitor::Visitor;

pub type Span = (usize, usize);

pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;
    const MISSING: Self;
    const JUXTAPOSE: Self;

    fn as_usize(self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}
