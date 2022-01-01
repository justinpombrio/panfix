//! A lexer (a.k.a. tokenizer) that produces a stream of (token, lexeme) pairs.
//!
//! Usage:
//!
//! ```
//! use panfix::lexer::{LexerBuilder, Lexer, LEX_ERROR};
//!
//! let whitespace_regex = r#"[ \t\r\n]+"#;
//! let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
//! let tok_plus = builder.string("+").unwrap();
//! let tok_var = builder.regex("[a-zA-Z_]+").unwrap();
//! let lexer = builder.finish().unwrap();
//!
//! let mut lexemes = lexer.lex("x + y");
//! assert_eq!(lexemes.next().unwrap(), (tok_var, "x"));
//! assert_eq!(lexemes.next().unwrap(), (tok_plus, "+"));
//! assert_eq!(lexemes.next().unwrap(), (tok_var, "y"));
//! assert_eq!(lexemes.next(), None);
//!
//! let mut lexemes = lexer.lex("x @$!");
//! assert_eq!(lexemes.next().unwrap(), (tok_var, "x"));
//! assert_eq!(lexemes.next().unwrap(), (LEX_ERROR, "@$!"));
//! ```
//!
//! Whitespace is skipped. If there is a lexing error, the stream will end with a lexeme whose
//! `token` is `[LEX_ERROR]`.
//!
//! If there are multiple possible matches:
//!
//! - The longest match is used.
//! - If there is a tie, whichever token is a 'string' pattern instead of a 'regex' pattern will be
//! used.
//! - If there is _still_ a tie, the regex that's first in the list provided to `Lexer::new()` will
//! be used.

use regex::{escape, Error as RegexError, Regex, RegexSet};

/// A category of lexeme, such as "INTEGER" or "VARIABLE" or "OPEN_PAREN". The special Token called
/// [`LEX_ERROR`] represents a lexing error.
pub type Token = usize;

pub const LEX_ERROR: Token = Token::MAX;

#[derive(Debug, Clone)]
pub struct Pattern {
    regex: Regex,
    length: Option<usize>,
}

impl PartialEq for Pattern {
    fn eq(&self, other: &Pattern) -> bool {
        self.regex.as_str() == other.regex.as_str() && self.length == other.length
    }
}

impl Eq for Pattern {}

fn new_regex(regex: &str) -> Result<Regex, RegexError> {
    Regex::new(&format!("^({})", regex))
}

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

#[derive(Debug, Clone)]
pub struct Lexer {
    whitespace: Regex,
    patterns: Vec<Pattern>,
    regex_set: RegexSet,
}

impl Lexer {
    /// Split `source` into a stream of lexemes. It is frequently useful to wrap this in
    /// [`iter::Peekable`](https://doc.rust-lang.org/stable/std/iter/struct.Peekable.html).
    pub fn lex<'s>(&'s self, source: &'s str) -> impl Iterator<Item = (Token, &'s str)> {
        LexemeIter {
            source,
            whitespace: &self.whitespace,
            patterns: &self.patterns,
            regex_set: &self.regex_set,
        }
    }
}

#[derive(Debug, Clone)]
struct LexemeIter<'s> {
    // The _remaining, unlexed_ source text
    source: &'s str,
    whitespace: &'s Regex,
    patterns: &'s [Pattern],
    regex_set: &'s RegexSet,
}

impl<'s> LexemeIter<'s> {
    fn consume(&mut self, len: usize) -> &'s str {
        let lexeme = &self.source[..len];
        self.source = &self.source[len..];
        lexeme
    }
}

impl<'s> Iterator for LexemeIter<'s> {
    type Item = (Token, &'s str);

    fn next(&mut self) -> Option<(Token, &'s str)> {
        // Consume whitespace
        if let Some(span) = self.whitespace.find(self.source) {
            self.source = &self.source[span.end()..];
        }

        // If we're at the end of the file, we're done.
        if self.source.is_empty() {
            return None;
        }

        // Find the best match (longest, with a tie-breaker of is_str)
        let mut best_match: Option<(Token, usize, bool)> = None;
        for token in &self.regex_set.matches(self.source) {
            let pattern = &self.patterns[token];

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
            let lexeme = self.consume(len);
            return Some((token, lexeme));
        }

        // Otherwise, nothing matched. Lex error! By definition we can't lex, but let's say the
        // problem lies in the current chunk of non-basic-whitespace characters.
        let basic_whitespace = &[' ', '\t', '\r', '\n'];
        let len = if let Some(len) = self.source.find(basic_whitespace) {
            len
        } else {
            self.source.len()
        };
        let lexeme = self.consume(len);
        Some((LEX_ERROR, lexeme))
    }
}

#[test]
fn test_lexer() {
    let mut builder = LexerBuilder::new(r#"[ \t\r\n]+"#).unwrap();
    let tok_var = builder.regex("[a-zA-Z_]+").unwrap();
    //let _duplicate = builder.string("raise").unwrap();
    let tok_lparen = builder.string("(").unwrap();
    let tok_raise = builder.string("raise").unwrap();
    let tok_rparen = builder.string(")").unwrap();
    let lexer = builder.finish().unwrap();

    let source = "raised";
    let mut lexemes = lexer.lex(source);
    assert_eq!(lexemes.next(), Some((tok_var, "raised")));
    assert_eq!(lexemes.next(), None);

    let source = "raise(my_error)";
    let mut lexemes = lexer.lex(source);
    assert_eq!(lexemes.next(), Some((tok_raise, "raise")));
    assert_eq!(lexemes.next(), Some((tok_lparen, "(")));
    assert_eq!(lexemes.next(), Some((tok_var, "my_error")));
    assert_eq!(lexemes.next(), Some((tok_rparen, ")")));
    assert_eq!(lexemes.next(), None);

    let source = "x $$ !";
    let mut lexemes = lexer.lex(source);
    assert_eq!(lexemes.next(), Some((tok_var, "x")));
    assert_eq!(lexemes.next(), Some((LEX_ERROR, "$$")));
    assert_eq!(lexemes.next(), Some((LEX_ERROR, "!")));
    assert_eq!(lexemes.next(), None);
}
