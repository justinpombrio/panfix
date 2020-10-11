use super::grammar::Token;
use regex::Regex;
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

#[derive(Debug, Clone)]
pub struct Lexer {
    pub(super) whitespace: Regex,
    pub(super) regexes: Vec<(Regex, Token)>,
    pub(super) constants: Vec<(String, Token)>,
}

#[derive(Debug)]
pub struct Lex<'s> {
    lexer: &'s Lexer,
    source: &'s str,
    index: usize,
}

impl Lexer {
    pub fn lex<'s>(&'s self, source: &'s str) -> impl Iterator<Item = Lexeme> + 's {
        Lex {
            lexer: self,
            source,
            index: 0,
        }
    }
}

impl<'s> Iterator for Lex<'s> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();
        for (regex, token) in &self.lexer.regexes {
            if let Some(matched) = regex.find(self.remaining()) {
                // TODO: Reduce the number of cases in which this is checked.
                // In case a constant also matches the regex
                for (constant, constant_token) in &self.lexer.constants {
                    if matched.as_str() == constant {
                        return Some(self.eat_token(*constant_token, constant.len()));
                    }
                }
                return Some(self.eat_token(*token, matched.end()));
            }
        }
        for (constant, token) in &self.lexer.constants {
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

impl<'s> Lex<'s> {
    pub fn remaining(&self) -> &'s str {
        &self.source[self.index..]
    }

    fn eat_whitespace(&mut self) {
        if let Some(matched) = self.lexer.whitespace.find(self.remaining()) {
            self.index += matched.end();
        }
    }

    fn eat_token(&mut self, token: Token, len: usize) -> Lexeme {
        let span = (self.index, self.index + len);
        self.index += len;
        Lexeme { span, token }
    }
}
