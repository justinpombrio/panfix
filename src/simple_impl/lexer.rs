use regex::{escape, Regex};

pub type Token = usize;

#[derive(Debug, Clone)]
pub struct Lexer {
    pub whitespace: Regex,
    pub tokens: Vec<Regex>,
}

#[derive(Debug, Clone, Copy)]
pub struct Lexeme<'s> {
    pub token: Token,
    pub substr: &'s str,
}

#[derive(Debug, Clone, Copy)]
pub enum LexError {
    InvalidLexeme,
}

fn new_regex(regex: &str) -> Regex {
    Regex::new(&format!("^({})", regex)).expect("invalid regex")
}

impl Lexer {
    pub fn new(whitespace_regex: &str) -> Lexer {
        Lexer {
            whitespace: new_regex(whitespace_regex),
            tokens: vec![],
        }
    }

    pub fn add_string(&mut self, literal: &str) -> Token {
        let token = self.tokens.len();
        self.tokens.push(new_regex(&escape(literal)));
        token
    }

    pub fn add_regex(&mut self, regex: &str) -> Token {
        let token = self.tokens.len();
        self.tokens.push(new_regex(regex));
        token
    }

    pub fn lex<'s>(
        &'s self,
        source: &'s str,
    ) -> impl Iterator<Item = Result<Lexeme<'s>, LexError>> {
        LexerIter {
            source,
            whitespace: &self.whitespace,
            tokens: &self.tokens,
            done: false,
        }
    }
}

#[derive(Debug, Clone)]
struct LexerIter<'s> {
    source: &'s str,
    whitespace: &'s Regex,
    tokens: &'s [Regex],
    done: bool,
}

impl<'s> Iterator for LexerIter<'s> {
    type Item = Result<Lexeme<'s>, LexError>;

    fn next(&mut self) -> Option<Result<Lexeme<'s>, LexError>> {
        if self.done {
            return None;
        }

        // Consume whitespace
        if let Some(bounds) = self.whitespace.find(&self.source) {
            self.source = &self.source[bounds.end()..];
        }

        // If we're at the end of the file, we're done.
        if self.source.is_empty() {
            self.done = true;
            return None;
        }

        // Check each op in turn.
        for (i, regex) in self.tokens.iter().enumerate() {
            let token = i as Token;
            if let Some(bounds) = regex.find(&self.source) {
                let substr = &self.source[..bounds.end()];
                self.source = &self.source[bounds.end()..];
                return Some(Ok(Lexeme { token, substr }));
            }
        }

        // Nothing matched! Lex error!
        self.done = true;
        Some(Err(LexError::InvalidLexeme))
    }
}

// NOTICE:
// - The order of the ops is important! Earlier ops are prioritized in lexing.
