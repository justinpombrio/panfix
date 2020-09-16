use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Iterator;

mod builder;

pub use builder::{Lexer, LexerBuilder};

pub type Span = (usize, usize);

pub trait Token: Debug + Clone + Copy + PartialEq + Eq + Hash {
    const LEX_ERROR: Self;

    fn as_usize(self) -> usize;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<T: Token> {
    pub token: T,
    pub span: Span,
}

#[derive(Debug)]
pub struct Lex<'s, T: Token> {
    lexer: &'s Lexer<T>,
    source: &'s str,
    index: usize,
}

impl<T: Token> Lexer<T> {
    pub fn lex<'s>(&'s self, source: &'s str) -> impl Iterator<Item = Lexeme<T>> + 's {
        Lex {
            lexer: self,
            source,
            index: 0,
        }
    }
}

impl<'s, T: Token> Iterator for Lex<'s, T> {
    type Item = Lexeme<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();
        for (regex, token) in &self.lexer.regexes {
            if let Some(matched) = regex.find(self.remaining()) {
                let token = *token;
                // TODO: Reduce the number of cases in which this is checked.
                // In case a constant also matches the regex
                for (constant, constant_token) in &self.lexer.constants {
                    if matched.as_str() == constant {
                        let constant_token = *constant_token;
                        return Some(self.eat_token(constant_token, constant.len()));
                    }
                }
                return Some(self.eat_token(token, matched.end()));
            }
        }
        for (constant, token) in &self.lexer.constants {
            if self.remaining().starts_with(constant) {
                let token = *token;
                let len = constant.len();
                return Some(self.eat_token(token, len));
            }
        }
        if let Some(first_char) = self.remaining().chars().next() {
            Some(self.eat_token(T::LEX_ERROR, first_char.len_utf8()))
        } else {
            None
        }
    }
}

impl<'s, T: Token> Lex<'s, T> {
    pub fn remaining(&self) -> &'s str {
        &self.source[self.index..]
    }

    fn eat_whitespace(&mut self) {
        if let Some(matched) = self.lexer.whitespace.find(self.remaining()) {
            self.index += matched.end();
        }
    }

    fn eat_token(&mut self, token: T, len: usize) -> Lexeme<T> {
        let span = (self.index, self.index + len);
        self.index += len;
        Lexeme { span, token }
    }
}
