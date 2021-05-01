use super::lexer::Lexer;
use super::pattern::Pattern;
use super::Token;
use regex::{Error as RegexError, Regex, RegexSet};
use std::mem;
use thiserror::Error;

/// White space, according to the Pattern_White_Space Unicode property.
const DEFAULT_WHITESPACE_REGEX: &str =
    "^[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]";

pub struct LexerBuilder<T: Token> {
    whitespace_regexes: Vec<String>,
    literals: Vec<(T, String)>,
    regexes: Vec<(T, String)>,
}

#[derive(Debug, Clone, Error)]
pub enum LexerBuilderError {
    #[error(transparent)]
    InvalidRegex(#[from] RegexError),
    // TODO: Error in case two of the regexes overlap.
}

impl<T: Token> LexerBuilder<T> {
    /// Start building a [`Lexer`].
    ///
    /// Initially there are _no_ whitespace patterns. For a reasonable default, call
    /// `unicode_whitespace()`.
    pub fn new() -> LexerBuilder<T> {
        LexerBuilder {
            whitespace_regexes: vec![],
            literals: vec![],
            regexes: vec![],
        }
    }

    /// Add the Unicode Pattern_White_Space pattern to the whitespace set.
    pub fn unicode_whitespace(&mut self) -> &mut LexerBuilder<T> {
        self.whitespace_regexes
            .push(DEFAULT_WHITESPACE_REGEX.to_owned());
        self
    }

    /// Add a regex pattern to the whitespace set.
    ///
    /// Any combination of the patterns will be considered whitespace. For example, if you call
    /// `whitespace()` three times with regexes `{A, B, C}`, then anything matching the regex
    /// `(A|B|C)*` will be considered whitespace.
    pub fn whitespace(&mut self, regex: &str) -> &mut LexerBuilder<T> {
        self.whitespace_regexes.push(format!("({})", regex));
        self
    }

    /// Add a token that matches a literal string.
    pub fn string_token(&mut self, string: &str, token: T) -> &mut LexerBuilder<T> {
        self.literals.push((token, string.to_owned()));
        self
    }

    /// Add a token that matches a regex pattern. The regex syntax is that of the `regex` crate.
    pub fn regex_token(&mut self, regex: &str, token: T) -> &mut LexerBuilder<T> {
        self.regexes.push((token, format!("^({})", regex)));
        self
    }

    /// Finish the builder pattern, and construct the Lexer.
    pub fn build(&mut self) -> Result<Lexer<T>, LexerBuilderError> {
        // whitespace_regexes = [A, B, C] -> whitespace = (A|B|C)*
        let whitespace = Regex::new(&format!("^({})*", self.whitespace_regexes.join("|")))?;
        let unanchored_whitespace = Regex::new(&format!("{}", self.whitespace_regexes.join("|")))?;

        // Sort the literals by reverse length, in case one is a prefix of another.
        // (It's the length of the escaped regex literal, but that still respects "is a prefix".)
        self.literals.sort_by_key(|(_, re)| -(re.len() as i32));

        // Put the literals first so they take precedence over regexes.
        let mut patterns = vec![];
        for (token, literal) in mem::take(&mut self.literals).into_iter() {
            patterns.push(Pattern::new_literal(token, literal)?);
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
