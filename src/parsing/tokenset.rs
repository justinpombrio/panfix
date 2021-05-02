use crate::lexing::{Lexer, LexerBuilder, LexerBuilderError};
use crate::shunting::Token;
use std::collections::HashMap;

pub type RegexPattern = String;

#[derive(Debug)]
pub struct TokenSet {
    next_token: Token,
    literals: HashMap<String, Token>,
    regexes: HashMap<RegexPattern, Token>,
    whitespace: Vec<RegexPattern>,
}

impl TokenSet {
    pub fn new() -> TokenSet {
        TokenSet {
            next_token: 0 as Token,
            literals: HashMap::new(),
            regexes: HashMap::new(),
            whitespace: vec![],
        }
    }

    pub fn string(&mut self, literal: &str) -> Token {
        if let Some(token) = self.literals.get(literal) {
            return *token;
        }
        self.next_token += 1;
        self.literals.insert(literal.to_owned(), self.next_token);
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

    pub fn into_lexer(self) -> Result<Lexer<Token>, LexerBuilderError> {
        let mut builder = LexerBuilder::new();
        let mut builder = &mut builder;
        for whitespace in self.whitespace {
            builder = builder.whitespace(&whitespace);
        }
        for (literal, token) in self.literals.into_iter() {
            builder = builder.string(&literal, token);
        }
        for (regex, token) in self.regexes.into_iter() {
            builder = builder.regex(&regex, token);
        }
        builder.build()
    }
}
