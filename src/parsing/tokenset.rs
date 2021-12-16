use crate::lexing::{Lexer, LexerBuilder, LexerBuilderError};
use crate::shunting::Token;
use std::collections::HashMap;

pub type RegexPattern = String;

#[derive(Debug)]
pub struct TokenSet {
    next_token: Token,
    strings: HashMap<String, Token>,
    regexes: HashMap<RegexPattern, Token>,
    whitespace: Vec<RegexPattern>,
}

impl TokenSet {
    pub fn new() -> TokenSet {
        TokenSet {
            next_token: 0 as Token,
            strings: HashMap::new(),
            regexes: HashMap::new(),
            whitespace: vec![],
        }
    }

    pub fn string(&mut self, string: &str) -> Token {
        if let Some(token) = self.strings.get(string) {
            return *token;
        }
        self.next_token += 1;
        self.strings.insert(string.to_owned(), self.next_token);
        self.next_token
    }

    pub fn regex(&mut self, regex: &str) -> Token {
        if let Some(token) = self.regexes.get(regex) {
            return *token;
        }
        self.next_token += 1;
        self.regexes.insert(regex.to_owned(), self.next_token);
        self.next_token
    }

    pub fn insert_whitespace(&mut self, regex: &str) {
        self.whitespace.push(regex.to_owned());
    }

    pub fn into_lexer(self) -> Result<(Lexer<Token>, HashMap<Token, String>), LexerBuilderError> {
        let mut builder = LexerBuilder::new();
        let mut builder = &mut builder;
        let mut token_names = HashMap::new();
        for whitespace in self.whitespace {
            builder = builder.whitespace(&whitespace);
        }
        for (string, token) in self.strings.into_iter() {
            token_names.insert(token, string.clone());
            builder = builder.string(&string, token);
        }
        for (regex, token) in self.regexes.into_iter() {
            builder = builder.regex(&regex, token);
        }
        let lexer = builder.build()?;
        Ok((lexer, token_names))
    }
}
