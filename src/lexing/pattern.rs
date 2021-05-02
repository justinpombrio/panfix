use super::Token;
use regex::{escape, Error as RegexError, Regex};
use std::fmt;

#[derive(Debug, Clone)]
pub struct Pattern<T: Token> {
    string: Option<String>,
    regex: Regex,
    token: T,
}

impl<T: Token> Pattern<T> {
    pub fn new_string(token: T, string: String) -> Result<Pattern<T>, RegexError> {
        Ok(Pattern {
            regex: Regex::new(&format!("^({})", escape(&string)))?,
            string: Some(string),
            token,
        })
    }

    pub fn new_regex(token: T, regex_pattern: String) -> Result<Pattern<T>, RegexError> {
        Ok(Pattern {
            regex: Regex::new(&format!("^({})", regex_pattern))?,
            string: None,
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
        match &self.string {
            Some(string) => string.len(),
            None => self.regex.find(input).unwrap().end(),
        }
    }
}

impl<T: Token> fmt::Display for Pattern<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.string {
            Some(string) => write!(f, "\"{}\"", string.escape_default()),
            None => write!(f, "/{}/", self.regex.as_str().escape_default()),
        }
    }
}
