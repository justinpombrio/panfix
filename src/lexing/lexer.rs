use super::pattern::Pattern;
use super::{Lexeme, Token};
use regex::{Regex, RegexSet};

/// A lexer that matches tokens of type `T`.
#[derive(Debug, Clone)]
pub struct Lexer<T: Token> {
    pub(super) whitespace: Regex,
    pub(super) unanchored_whitespace: Regex,
    pub(super) patterns: Vec<Pattern<T>>,
    pub(super) regex_set: RegexSet,
}

#[derive(Debug, Clone)]
struct Lex<'s, T: Token> {
    lexer: &'s Lexer<T>,
    source: &'s str,
    index: usize,
}

impl<T: Token> Lexer<T> {
    /// Split the source into a stream of lexemes.
    ///
    /// A lexing error is indicated by `lexeme.token == T::LEX_ERROR`. Whitespace is ignored. If
    /// there are multiple matches (over any combination of string and regex patterns), the longest
    /// match is used.
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

    fn next(&mut self) -> Option<Lexeme<T>> {
        self.consume_whitespace();

        // Find the longest match
        let mut longest_match: Option<(T, usize)> = None;
        for i in &self.lexer.regex_set.matches(self.remaining()) {
            // The i'th pattern matched. Ask the Pattern to determine its len.
            let pattern = &self.lexer.patterns[i];
            let len = pattern.unchecked_match_len(self.remaining());
            if longest_match.is_none() || len > longest_match.unwrap().1 {
                longest_match = Some((pattern.token(), len));
            }
        }

        // Consume said match
        match longest_match {
            Some((token, len)) => Some(self.consume_token(token, len)),
            None => {
                if self.index == self.source.len() {
                    // EOF. Success!
                    None
                } else {
                    // No pattern matched! Lex error!
                    Some(self.consume_lex_error_until_whitespace())
                }
            }
        }
    }
}

impl<'s, T: Token> Lex<'s, T> {
    fn remaining(&self) -> &'s str {
        &self.source[self.index..]
    }

    fn consume_whitespace(&mut self) {
        if let Some(matched) = self.lexer.whitespace.find(self.remaining()) {
            self.index += matched.end();
        }
    }

    fn consume_token(&mut self, token: T, len: usize) -> Lexeme<T> {
        let span = (self.index, self.index + len);
        self.index += len;
        Lexeme { token, span }
    }

    // Consume a chunk of text that failed to lex. We can't _truly_ delimit it, since by definition
    // they doesn't lex, but ending at the start of the next whitespace should be a good heuristic.
    fn consume_lex_error_until_whitespace(&mut self) -> Lexeme<T> {
        let len = if let Some(matched) = self.lexer.unanchored_whitespace.find(self.remaining()) {
            if matched.start() >= 1 {
                matched.start()
            } else {
                self.source.len() - self.index
            }
        } else {
            self.source.len() - self.index
        };
        self.consume_token(T::LEX_ERROR, len)
    }
}
