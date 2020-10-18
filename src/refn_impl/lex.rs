use super::grammar::{Grammar, Token};
use std::fmt::Debug;
use std::hash::Hash;
use std::iter::Iterator;

pub(super) type Span = (usize, usize);

const LEX_ERROR: Token = 0;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

impl Lexeme {
    pub fn text(self, source: &str) -> &str {
        &source[self.span.0..self.span.1]
    }
}

#[derive(Debug)]
pub struct Lexer<'s> {
    source: &'s str,
    grammar: &'s Grammar,
    index: usize,
}

pub fn lex<'s>(grammar: &'s Grammar, source: &'s str) -> impl Iterator<Item = Lexeme> + 's {
    Lexer {
        source,
        grammar,
        index: 0,
    }
}

impl<'s> Iterator for Lexer<'s> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();
        for (regex, token) in &self.grammar.regexes {
            if let Some(matched) = regex.find(self.remaining()) {
                // TODO: Reduce the number of cases in which this is checked.
                // In case a constant also matches the regex
                for (constant, constant_token) in &self.grammar.constants {
                    if matched.as_str() == constant {
                        return Some(self.eat_token(*constant_token, constant.len()));
                    }
                }
                return Some(self.eat_token(*token, matched.end()));
            }
        }
        for (constant, token) in &self.grammar.constants {
            if self.remaining().starts_with(constant) {
                let len = constant.len();
                return Some(self.eat_token(*token, len));
            }
        }
        if let Some(first_char) = self.remaining().chars().next() {
            Some(self.eat_token(LEX_ERROR, first_char.len_utf8()))
        } else {
            None
        }
    }
}

impl<'s> Lexer<'s> {
    pub fn remaining(&self) -> &'s str {
        &self.source[self.index..]
    }

    fn eat_whitespace(&mut self) {
        if let Some(matched) = self.grammar.whitespace.find(self.remaining()) {
            self.index += matched.end();
        }
    }

    fn eat_token(&mut self, token: Token, len: usize) -> Lexeme {
        let span = (self.index, self.index + len);
        self.index += len;
        Lexeme { span, token }
    }
}
