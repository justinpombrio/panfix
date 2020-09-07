use super::Token;
use regex::{Error as RegexError, Regex};
use std::error::Error;
use std::fmt;
use std::iter::Iterator;

#[derive(Debug, Clone)]
pub enum LexerConstructionError {
    InvalidRegex(RegexError),
}

#[derive(Debug, Clone)]
pub struct LexerBuilder<T: Token> {
    whitespace: String,
    regexes: Vec<(String, T)>,
    constants: Vec<(String, T)>,
}

#[derive(Debug, Clone)]
pub struct Lexer<T: Token> {
    pub(super) whitespace: Regex,
    pub(super) regexes: Vec<(Regex, T)>,
    pub(super) constants: Vec<(String, T)>,
}

impl Error for LexerConstructionError {}

impl From<RegexError> for LexerConstructionError {
    fn from(error: RegexError) -> LexerConstructionError {
        LexerConstructionError::InvalidRegex(error)
    }
}

impl fmt::Display for LexerConstructionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LexerConstructionError::*;
        match self {
            InvalidRegex(err) => write!(f, "{}", err),
        }
    }
}

impl<T: Token> LexerBuilder<T> {
    pub fn new(whitespace: &str) -> LexerBuilder<T> {
        LexerBuilder {
            regexes: vec![],
            constants: vec![],
            whitespace: whitespace.to_owned(),
        }
    }

    pub fn regex(&mut self, regex: &str, token: T) -> &mut LexerBuilder<T> {
        self.regexes.push((regex.to_owned(), token));
        self
    }

    pub fn constant(&mut self, constant: &str, token: T) -> &mut LexerBuilder<T> {
        self.constants.push((constant.to_owned(), token));
        self
    }

    pub fn build(&self) -> Result<Lexer<T>, LexerConstructionError> {
        Lexer::new(
            self.whitespace.to_owned(),
            self.regexes.clone(),
            self.constants.clone(),
        )
    }
}

impl<T: Token> Lexer<T> {
    pub fn new(
        whitespace: String,
        regexes: Vec<(String, T)>,
        mut constants: Vec<(String, T)>,
    ) -> Result<Lexer<T>, LexerConstructionError> {
        let whitespace = Regex::new(&format!("^({})", whitespace))?;
        // In case one constant is a prefix of another
        constants.sort_by_key(|(c, _)| -(c.chars().count() as i32));
        let mut compiled_regexes = vec![];
        for (regex, token) in regexes {
            compiled_regexes.push((Regex::new(&format!("^({})", regex))?, token));
        }
        Ok(Lexer {
            whitespace,
            regexes: compiled_regexes,
            constants,
        })
    }
}
