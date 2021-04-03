use super::Token;
use regex::{escape, Error as RegexError, Regex};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Pattern<T: Token> {
    literal: Option<String>,
    regex: Regex,
    token: T,
}

impl<T: Token> Pattern<T> {
    pub fn new_literal(token: T, literal: String) -> Result<Pattern<T>, RegexError> {
        let regex_pattern = escape(&literal);
        Ok(Pattern {
            literal: Some(literal),
            regex: Regex::new(&regex_pattern)?,
            token,
        })
    }

    pub fn new_regex(token: T, regex_pattern: String) -> Result<Pattern<T>, RegexError> {
        Ok(Pattern {
            literal: None,
            regex: Regex::new(&regex_pattern)?,
            token,
        })
    }

    pub fn regex_pattern(&self) -> &str {
        self.regex.as_str()
    }

    pub fn token(&self) -> T {
        self.token
    }

    pub fn unchecked_match_len(&self, input: &str) -> usize {
        match &self.literal {
            Some(literal) => literal.len(),
            None => self.regex.find(input).unwrap().end(),
        }
    }
}

impl<T: Token> fmt::Display for Pattern<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.literal {
            Some(literal) => write!(f, "\"{}\"", literal.escape_default()),
            None => write!(f, "/{}/", self.regex.as_str().escape_default()),
        }
    }
}
