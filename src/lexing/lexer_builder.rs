use super::lexer::Lexer;
use super::pattern::Pattern;
use super::Token;
use regex::{Error as RegexError, Regex, RegexSet};
use std::mem;
use thiserror::Error;

/// White space, according to the Pattern_White_Space Unicode property.
const DEFAULT_WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]";

/// Construct a lexer.
pub struct LexerBuilder<T: Token> {
    whitespace_regexes: Vec<String>,
    strings: Vec<(T, String)>,
    regexes: Vec<(T, String)>,
}

/// Error while constructing a lexer.
#[derive(Debug, Clone, Error)]
pub enum LexerBuilderError {
    #[error(transparent)]
    InvalidRegex(#[from] RegexError),
}

impl<T: Token> LexerBuilder<T> {
    /// Start building a [`Lexer`].
    ///
    /// Initially there are _no_ whitespace patterns. For a reasonable default, call
    /// `unicode_whitespace()`.
    pub fn new() -> LexerBuilder<T> {
        LexerBuilder {
            whitespace_regexes: vec![],
            strings: vec![],
            regexes: vec![],
        }
    }

    /// Add the Unicode Pattern_White_Space pattern to the whitespace set.
    pub fn unicode_whitespace(&mut self) -> &mut LexerBuilder<T> {
        self.whitespace_regexes
            .push(DEFAULT_WHITESPACE_REGEX.to_owned());
        self
    }

    /// Add a regex pattern to the whitespace set. It must not match the empty string.
    ///
    /// Any combination of the patterns will be considered whitespace. For example, if you call
    /// `whitespace()` three times with regexes `{A, B, C}`, then anything matching the regex
    /// `(A|B|C)*` will be considered whitespace.
    pub fn whitespace(&mut self, regex: &str) -> &mut LexerBuilder<T> {
        self.whitespace_regexes.push(format!("({})", regex));
        self
    }

    /// Add a token that matches a literal string. Special regex characters are matched literally;
    /// you do not have to escape anything.
    pub fn string(&mut self, string: &str, token: T) -> &mut LexerBuilder<T> {
        self.strings.push((token, string.to_owned()));
        self
    }

    /// Add a token that matches a regex pattern. The regex syntax is that of the `regex` crate.
    pub fn regex(&mut self, regex: &str, token: T) -> &mut LexerBuilder<T> {
        self.regexes.push((token, regex.to_owned()));
        self
    }

    /// Finish the builder pattern, and construct the Lexer.
    pub fn build(&mut self) -> Result<Lexer<T>, LexerBuilderError> {
        // whitespace_regexes = [A, B, C] -> whitespace = (A|B|C)*
        let whitespace = Regex::new(&format!("^({})*", self.whitespace_regexes.join("|")))?;
        let unanchored_whitespace = Regex::new(&format!("{}", self.whitespace_regexes.join("|")))?;

        // Put the strings first so they take precedence over regexes.
        let mut patterns = vec![];
        for (token, string) in mem::take(&mut self.strings).into_iter() {
            patterns.push(Pattern::new_string(token, string)?);
        }
        for (token, regex) in mem::take(&mut self.regexes).into_iter() {
            patterns.push(Pattern::new_regex(token, regex)?);
        }

        // If regex_set.matches() = i, then patterns[i] gives the pattern and token whose regex matched.
        let regex_set = RegexSet::new(patterns.iter().map(|p| p.regex_pattern()))?;

        Ok(Lexer {
            whitespace,
            unanchored_whitespace,
            patterns,
            regex_set,
        })
    }
}
