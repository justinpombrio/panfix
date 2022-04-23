//! A lexer (a.k.a. tokenizer) that produces an iterator of (token, lexeme) pairs.
//!
//! Usage:
//!
//! ```
//! use panfix::lexing::{LexerBuilder, Lexer, LEX_ERROR};
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
//! assert_eq!(lexemes.next().unwrap().token, LEX_ERROR);
//! ```
//!
//! Whitespace is skipped. If there is a lexing error, it is represented as an item in the iterator
//! whose `token` is `LEX_ERROR`.
//!
//! If there are multiple possible matches:
//!
//! - The longest match is used.
//! - If there is a tie, whichever token is a 'string' pattern instead of a 'regex' pattern will be
//! used.
//! - If there is _still_ a tie, the regex that's first in the list provided to `Lexer::new()` will
//! be used.

use regex::{escape, Error as RegexError, Regex, RegexSet};
use std::fmt;

/// A category of lexeme, such as "INTEGER" or "VARIABLE" or "OPEN_PAREN". The special Token called
/// [`LEX_ERROR`] represents a lexing error.
pub type Token = usize;

/// Represents a lexing error.
pub const LEX_ERROR: Token = Token::MAX;

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
        Ok(LexerBuilder {
            whitespace: new_regex(whitespace_regex)?,
            patterns: vec![],
        })
    }

    /// Add a pattern that matches exactly the string provided. Returns the token that will be
    /// produced whenever this pattern matches.
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
    pub fn lex<'l, 's: 'l>(&'l self, source: &'s str) -> impl Iterator<Item = Lexeme<'s>> + 'l {
        LexemeIter {
            source,
            lexer: self,
            position: Position {
                pos: 0,
                line: 0,
                col: 0,
                utf8_col: 0,
            },
        }
    }
}

/// One "word" in the stream returned by the lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexeme<'s> {
    pub token: Token,
    pub lexeme: &'s str,
    /// The position just before the first character in the lexeme.
    pub start: Position,
    /// The position just after the last character in the lexeme.
    pub end: Position,
}

/// A position in the source text. Positions are _between_ characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    /// Byte offset from the beginning of the source string.
    pub pos: usize,
    /// Line number.
    pub line: usize,
    /// Column number, counted in bytes.
    pub col: usize,
    /// Column number, counted in utf8 codepoints.
    pub utf8_col: usize,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.utf8_col)
    }
}

impl Position {
    fn advance(&mut self, ch: char) {
        self.pos += ch.len_utf8();
        if ch == '\n' {
            self.col = 0;
            self.utf8_col = 0;
            self.line += 1;
        } else {
            self.col += ch.len_utf8();
            self.utf8_col += 1;
        }
    }
}

#[derive(Debug, Clone)]
struct LexemeIter<'l, 's> {
    position: Position,
    // The _remaining, unlexed_ source text
    source: &'s str,
    lexer: &'l Lexer,
}

impl<'l, 's> LexemeIter<'l, 's> {
    fn consume(&mut self, len: usize) -> (&'s str, Position, Position) {
        let start = self.position;
        for ch in self.source[..len].chars() {
            self.position.advance(ch);
        }
        let end = self.position;

        let lexeme = &self.source[..len];
        self.source = &self.source[len..];
        (lexeme, start, end)
    }
}

impl<'l, 's> Iterator for LexemeIter<'l, 's> {
    type Item = Lexeme<'s>;

    fn next(&mut self) -> Option<Lexeme<'s>> {
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
            let (lexeme, start, end) = self.consume(len);
            return Some(Lexeme {
                token,
                lexeme,
                start,
                end,
            });
        }

        // Otherwise, nothing matched. Lex error! By definition we can't lex, but let's say the
        // problem lies in the current chunk of non-basic-whitespace characters.
        let basic_whitespace = &[' ', '\t', '\r', '\n'];
        let len = if let Some(len) = self.source.find(basic_whitespace) {
            len
        } else {
            self.source.len()
        };
        let (lexeme, start, end) = self.consume(len);
        Some(Lexeme {
            token: LEX_ERROR,
            lexeme,
            start,
            end,
        })
    }
}
