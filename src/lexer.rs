//! A lexer (a.k.a. tokenizer) that produces an iterator of (token, lexeme) pairs.
//!
//! Usage:
//!
//! ```
//! use panfix::TOKEN_ERROR;
//! use panfix::lexing::{LexerBuilder, Lexer};
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

use crate::{Lexeme, Position, Span, Token, TOKEN_ERROR};
use regex::{escape, Regex, RegexSet};
use std::fmt;

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
    pub fn lex<'l, 's: 'l>(&'l self, source: &'s str) -> impl Iterator<Item = Lexeme<'s>> + 'l {
        LexemeIter {
            source,
            lexer: self,
            position: Position {
                offset: 0,
                line: 0,
                col: 0,
                utf8_col: 0,
            },
        }
    }

    /// The number of tokens. Each `Token` returned by the builder is guaranteed to be smaller than
    /// this number.
    pub fn num_tokens(&self) -> usize {
        self.patterns.len()
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
    fn consume(&mut self, len: usize) -> (&'s str, Span) {
        let start = self.position;
        for ch in self.source[..len].chars() {
            self.position = self.position.advance_by_char(ch);
        }
        let end = self.position;

        let lexeme = &self.source[..len];
        self.source = &self.source[len..];
        (lexeme, Span { start, end })
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
            let (lexeme, span) = self.consume(len);
            return Some(Lexeme {
                token,
                lexeme,
                span,
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
        let (lexeme, span) = self.consume(len);
        Some(Lexeme {
            token: TOKEN_ERROR,
            lexeme,
            span,
        })
    }
}

impl<'s> fmt::Display for Lexeme<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.lexeme)
    }
}

#[track_caller]
#[cfg(test)]
fn assert_lexeme<'a>(
    src: &str,
    stream: &mut impl Iterator<Item = Lexeme<'a>>,
    expected: &str,
    token: Token,
) {
    let lex = stream
        .next()
        .expect("Token stream in test case ended early");
    assert_eq!(lex.token, token);
    let start = lex.span.start;
    let end = lex.span.end;
    assert_eq!(&src[start.offset..end.offset], lex.lexeme);
    let actual = format!(
        "{}:{}({})-{}:{}({}) {}",
        start.line, start.col, start.utf8_col, end.line, end.col, end.utf8_col, lex.lexeme
    );
    assert_eq!(actual, expected);
}

#[test]
fn test_lexing_json() {
    let string_regex = "\"([^\"\\\\]|\\\\.)*\"";
    let number_regex = "-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?";
    let whitespace_regex = "[ \\t\\n\\r\\v]*";

    let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
    let tok_str = builder.regex(string_regex).unwrap();
    let tok_num = builder.regex(number_regex).unwrap();
    let tok_true = builder.string("true").unwrap();
    let tok_false = builder.string("false").unwrap();
    let tok_null = builder.string("null").unwrap();
    let tok_colon = builder.string(":").unwrap();
    let tok_comma = builder.string(",").unwrap();
    let tok_lbracket = builder.string("[").unwrap();
    let tok_rbracket = builder.string("]").unwrap();
    let tok_lbrace = builder.string("{").unwrap();
    let tok_rbrace = builder.string("}").unwrap();
    let lexer = builder.finish().unwrap();

    let src = " 3.1e5";
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "0:1(1)-0:6(6) 3.1e5", tok_num);
    assert!(lexemes.next().is_none());

    let src = r#"{false]true  [5"5\"" "#;
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "0:0(0)-0:1(1) {", tok_lbrace);
    assert_lexeme(src, lexemes, "0:1(1)-0:6(6) false", tok_false);
    assert_lexeme(src, lexemes, "0:6(6)-0:7(7) ]", tok_rbracket);
    assert_lexeme(src, lexemes, "0:7(7)-0:11(11) true", tok_true);
    assert_lexeme(src, lexemes, "0:13(13)-0:14(14) [", tok_lbracket);
    assert_lexeme(src, lexemes, "0:14(14)-0:15(15) 5", tok_num);
    assert_lexeme(src, lexemes, "0:15(15)-0:20(20) \"5\\\"\"", tok_str);
    assert!(lexemes.next().is_none());

    let src = " \r\n\"Καλημέρα\" :\"Καλησπέρα\",\n  \"Καληνύχτα\"null}";
    let lexemes = &mut lexer.lex(src);
    assert_lexeme(src, lexemes, "1:0(0)-1:18(10) \"Καλημέρα\"", tok_str);
    assert_lexeme(src, lexemes, "1:19(11)-1:20(12) :", tok_colon);
    assert_lexeme(src, lexemes, "1:20(12)-1:40(23) \"Καλησπέρα\"", tok_str);
    assert_lexeme(src, lexemes, "1:40(23)-1:41(24) ,", tok_comma);
    assert_lexeme(src, lexemes, "2:2(2)-2:22(13) \"Καληνύχτα\"", tok_str);
    assert_lexeme(src, lexemes, "2:22(13)-2:26(17) null", tok_null);
    assert_lexeme(src, lexemes, "2:26(17)-2:27(18) }", tok_rbrace);
    assert!(lexemes.next().is_none());
}

#[test]
fn test_lexing_horrible_things() {
    let word_regex = "[a-yA-Y]+";
    let angry_word_regex = "[A-Y]+";
    let short_word_regex = "[a-zA-Z]";
    let whitespace_regex = "[ \\t\\n\\re]*";

    let mut builder = LexerBuilder::new(whitespace_regex).unwrap();
    builder.regex(angry_word_regex).unwrap();
    builder.regex(word_regex).unwrap();
    builder.regex(short_word_regex).unwrap();
    builder.string(":").unwrap();
    builder.string("::").unwrap();
    builder.string(":::").unwrap();
    builder.string("(").unwrap();
    builder.string(")").unwrap();
    builder.string(":(").unwrap();
    builder.string("true").unwrap();
    builder.string("truer").unwrap();
    builder.string("truest").unwrap();
    let lexer = builder.finish().unwrap();

    let lex = |source| lexer.lex(source).map(|lex| lex.lexeme).collect::<Vec<_>>();
    assert_eq!(lex("HELLO"), vec!["HELLO"]);
    assert_eq!(lex("Hello"), vec!["Hello"]);
    assert_eq!(lex("hello"), vec!["hello"]);
    assert_eq!(lex(":()"), vec![":(", ")"]);
    assert_eq!(lex(":::()"), vec![":::", "(", ")"]);
    assert_eq!(lex("::::()"), vec![":::", ":(", ")"]);
    assert_eq!(lex(":::::()"), vec![":::", "::", "(", ")"]);
    assert_eq!(lex("truertruetruerest"), vec!["truertruetruerest"]);
    assert_eq!(
        lex("truer true truerest"),
        vec!["truer", "true", "truerest"]
    );
    assert_eq!(
        lex("truery truey truerestytrue"),
        vec!["truery", "truey", "truerestytrue"]
    );
    assert_eq!(lex(" eprom "), vec!["prom"]);
    assert_eq!(lex("true! true"), vec!["true", "!", "true"]);
    assert_eq!(lex("tr\nue"), vec!["tr", "ue"]);
    assert_eq!(lex("tr%ue%% %%true"), vec!["tr", "%ue%%", "%%true"]);
}
