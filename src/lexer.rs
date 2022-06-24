//! A lexer (a.k.a. tokenizer) that produces an iterator of (token, lexeme) pairs.
//!
//! Usage:
//!
//! ```
//! use panfix::TOKEN_ERROR;
//! use panfix::implementation::lexer::{LexerBuilder, Lexer};
//!
//! let whitespace_regex = r#"[ \t\r\n]+"#;
//! let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
//! let tok_plus = builder.string("+").unwrap();
//! let tok_var = builder.regex("[a-zA-Z_]+").unwrap();
//! let lexer = builder.finish().unwrap();
//!
//! let mut lexemes = lexer.lex("x + y");
//! assert_eq!(lexemes.next().unwrap().token, tok_var);
//! assert_eq!(lexemes.next().unwrap().token, tok_plus);
//! assert_eq!(lexemes.next().unwrap().token, tok_var);
//! assert_eq!(lexemes.next(), None);
//!
//! let mut lexemes = lexer.lex("x @$!");
//! assert_eq!(lexemes.next().unwrap().token, tok_var);
//! assert_eq!(lexemes.next().unwrap().token, TOKEN_ERROR);
//! ```
//!
//! Whitespace is skipped. If there is a lexing error, it is represented as an item in the iterator
//! whose `token` is `TOKEN_ERROR`.
//!
//! If there are multiple possible matches:
//!
//! - The longest match is used.
//! - If there is a tie, whichever token is a 'string' pattern instead of a 'regex' pattern will be
//! used.
//! - If there is _still_ a tie, the regex that's first in the list provided to `Lexer::new()` will
//! be used.

use crate::{Lexeme, Offset, Position, Span, Token, TOKEN_ERROR};
use regex::{escape, Regex, RegexSet};

pub use regex::Error as RegexError;

/// White space as defined by the Pattern_White_Space Unicode property.
pub const UNICODE_WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]*";

#[derive(Debug, Clone)]
struct Pattern {
    regex: Regex,
    length: Option<usize>,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Pattern) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.length == other.length
    }
}

impl Eq for Pattern {}

/// A builder for `Lexer`. Specify the patterns to match.
#[derive(Debug, Clone)]
pub struct LexerBuilder {
    whitespace: Regex,
    patterns: Vec<Pattern>,
}

impl LexerBuilder {
    pub fn new(whitespace_regex: &str) -> Result<LexerBuilder, RegexError> {
        let mut builder = LexerBuilder {
            whitespace: new_regex(whitespace_regex)?,
            patterns: vec![],
        };
        builder.reserve_token()?; // Reserved for TOKEN_ERROR
        builder.reserve_token()?; // Reserved for TOKEN_BLANK
        builder.reserve_token()?; // Reserved for TOKEN_JUXTAPOSE
        Ok(builder)
    }

    /// Add a pattern that matches exactly the string provided. Returns the token that will be
    /// produced whenever this pattern matches. If the provided pattern was already added,
    /// returns the pre-existing token for it.
    pub fn string(&mut self, constant: &str) -> Result<Token, RegexError> {
        let pattern = Pattern {
            regex: new_regex(&escape(constant))?,
            length: Some(constant.len()),
        };

        for (existing_token, existing_pattern) in self.patterns.iter().enumerate() {
            if &pattern == existing_pattern {
                return Ok(existing_token);
            }
        }

        let token = self.patterns.len();
        self.patterns.push(pattern);
        Ok(token)
    }

    /// Add a pattern that matches the given regex. Returns the token that will be produced whenever
    /// this pattern matches.
    ///
    /// The syntax is that of the `regex` crate. You do not need to begin the pattern with a
    /// start-of-string character `^`.
    pub fn regex(&mut self, regex: &str) -> Result<Token, RegexError> {
        let pattern = Pattern {
            regex: new_regex(regex)?,
            length: None,
        };

        for (existing_token, existing_pattern) in self.patterns.iter().enumerate() {
            if &pattern == existing_pattern {
                return Ok(existing_token);
            }
        }

        let token = self.patterns.len();
        self.patterns.push(pattern);
        Ok(token)
    }

    /// Reserve a token for personal use.
    pub fn reserve_token(&mut self) -> Result<Token, RegexError> {
        let pattern = Pattern {
            regex: Regex::new("$.")?,
            length: None,
        };

        let token = self.patterns.len();
        self.patterns.push(pattern);
        Ok(token)
    }

    /// Call this when you're done adding token patterns, to construct the lexer.
    pub fn finish(self) -> Result<Lexer, RegexError> {
        Ok(Lexer {
            whitespace: self.whitespace,
            regex_set: RegexSet::new(self.patterns.iter().map(|p| p.regex.as_str()))?,
            patterns: self.patterns,
        })
    }
}

fn new_regex(regex: &str) -> Result<Regex, RegexError> {
    Regex::new(&format!("^({})", regex))
}

/// A set of patterns to use to lex.
#[derive(Debug, Clone)]
pub struct Lexer {
    whitespace: Regex,
    patterns: Vec<Pattern>,
    regex_set: RegexSet,
}

impl Lexer {
    /// Split `source` into a stream of lexemes. It is frequently useful to wrap this in
    /// [`iter::Peekable`](https://doc.rust-lang.org/stable/std/iter/struct.Peekable.html).
    pub fn lex<'l, 's: 'l>(&'l self, source: &'s str) -> impl Iterator<Item = Lexeme> + 'l {
        LexemeIter::new(self, source)
    }

    /// The number of tokens. Each `Token` returned by the builder is guaranteed to be smaller than
    /// this number.
    pub fn num_tokens(&self) -> usize {
        self.patterns.len()
    }
}

#[derive(Debug, Clone)]
struct LexemeIter<'l, 's> {
    lexer: &'l Lexer,
    source: &'s str,
    position: Position,
    offset: Offset,
}

impl<'l, 's> LexemeIter<'l, 's> {
    fn new(lexer: &'l Lexer, source: &'s str) -> LexemeIter<'l, 's> {
        LexemeIter {
            lexer,
            source,
            position: Position {
                line: 0,
                col: 0,
                utf8_col: 0,
            },
            offset: 0,
        }
    }

    fn consume(&mut self, len: usize) -> Span {
        let start = self.position;
        for ch in self.source[..len].chars() {
            self.offset += ch.len_utf8();
            self.position = self.position.advance_by_char(ch);
        }
        let end = self.position;

        self.source = &self.source[len..];
        Span { start, end }
    }
}

impl<'l, 's> Iterator for LexemeIter<'l, 's> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Lexeme> {
        // Consume whitespace
        if let Some(span) = self.lexer.whitespace.find(self.source) {
            self.consume(span.end());
        }

        // If we're at the end of the file, we're done.
        if self.source.is_empty() {
            return None;
        }

        // Find the best match (longest, with a tie-breaker of is_str)
        let mut best_match: Option<(Token, usize, bool)> = None;
        for token in &self.lexer.regex_set.matches(self.source) {
            let pattern = &self.lexer.patterns[token];

            // Find the length (and tie-breaker is_str) of this match.
            let (len, is_str) = if let Some(len) = pattern.length {
                (len, true)
            } else {
                (pattern.regex.find(self.source).unwrap().end(), false)
            };

            // If this is longer (or tie breaks) the best match so far, replace it.
            let is_best_match = if let Some((_, best_len, best_is_str)) = best_match {
                (len, is_str) > (best_len, best_is_str)
            } else {
                true
            };
            if is_best_match {
                best_match = Some((token, len, is_str));
            }
        }

        // If there was a best match, consume and return it.
        if let Some((token, len, _)) = best_match {
            let span = self.consume(len);
            return Some(Lexeme { token, span });
        }

        // Otherwise, nothing matched. Lex error! By definition we can't lex, but let's say the
        // problem lies in the current chunk of non-basic-whitespace characters.
        let basic_whitespace = &[' ', '\t', '\r', '\n'];
        let len = self
            .source
            .find(basic_whitespace)
            .unwrap_or(self.source.len());
        let span = self.consume(len);
        Some(Lexeme {
            token: TOKEN_ERROR,
            span,
        })
    }
}
