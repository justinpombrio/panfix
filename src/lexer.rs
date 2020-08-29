use crate::{Lexeme, Token};
use regex::{Error as RegexError, Regex};
use std::error::Error;
use std::fmt;
use std::iter::Iterator;

// TODO:
// - line counting

#[derive(Debug, Clone)]
pub struct Lexer<T: Token> {
    whitespace: Regex,
    regexes: Vec<(Regex, T)>,
    constants: Vec<(String, T)>,
}

#[derive(Debug, Clone)]
pub struct LexerBuilder<T: Token> {
    whitespace: String,
    regexes: Vec<(String, T)>,
    constants: Vec<(String, T)>,
}

#[derive(Debug, Clone)]
pub enum LexerConstructionError {
    InvalidRegex(RegexError),
}

#[derive(Debug)]
pub struct Lex<'s, T: Token> {
    lexer: &'s Lexer<T>,
    source: &'s str,
    index: usize,
}

impl fmt::Display for LexerConstructionError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use LexerConstructionError::*;
        match self {
            InvalidRegex(err) => write!(f, "{}", err),
        }
    }
}

impl Error for LexerConstructionError {}

impl From<RegexError> for LexerConstructionError {
    fn from(error: RegexError) -> LexerConstructionError {
        LexerConstructionError::InvalidRegex(error)
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
        let whitespace = Regex::new(&format!("^{}", whitespace))?;
        // In case one constant is a prefix of another
        constants.sort_by_key(|(c, _)| -(c.chars().count() as i32));
        let mut compiled_regexes = vec![];
        for (regex, token) in regexes {
            compiled_regexes.push((Regex::new(&format!("^{}", regex))?, token));
        }
        Ok(Lexer {
            whitespace,
            regexes: compiled_regexes,
            constants,
        })
    }

    pub fn lex<'s>(&'s self, source: &'s str) -> Lex<'s, T> {
        Lex {
            lexer: self,
            source,
            index: 0,
        }
    }
}

impl<'s, T: Token> Iterator for Lex<'s, T> {
    type Item = Lexeme<T>;

    fn next(&mut self) -> Option<Self::Item> {
        self.eat_whitespace();
        for (regex, token) in &self.lexer.regexes {
            if let Some(matched) = regex.find(self.remaining()) {
                let token = *token;
                // TODO: Reduce the number of cases in which this is checked.
                // In case a constant also matches the regex
                for (constant, constant_token) in &self.lexer.constants {
                    if matched.as_str() == constant {
                        let constant_token = *constant_token;
                        return Some(self.eat_token(constant_token, constant.len()));
                    }
                }
                return Some(self.eat_token(token, matched.end()));
            }
        }
        for (constant, token) in &self.lexer.constants {
            if self.remaining().starts_with(constant) {
                let token = *token;
                let len = constant.len();
                return Some(self.eat_token(token, len));
            }
        }
        if self.remaining().is_empty() {
            None
        } else {
            Some(self.eat_token(T::LEX_ERROR, self.remaining().len()))
        }
    }
}

impl<'s, T: Token> Lex<'s, T> {
    pub fn remaining(&self) -> &'s str {
        &self.source[self.index..]
    }

    fn eat_whitespace(&mut self) {
        if let Some(matched) = self.lexer.whitespace.find(self.remaining()) {
            self.index += matched.end();
        }
    }

    fn eat_token(&mut self, token: T, len: usize) -> Lexeme<T> {
        let span = (self.index, self.index + len);
        self.index += len;
        Lexeme { span, token }
    }
}

#[test]
fn test_lexer() {
    use JsonToken::*;

    #[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
    enum JsonToken {
        True,
        False,
        Null,
        JString,
        Number,
        OpenBrace,
        CloseBrace,
        OpenBracket,
        CloseBracket,
        Colon,
        Comma,
        _LexError,
    }
    impl Token for JsonToken {
        const LEX_ERROR: JsonToken = JsonToken::_LexError;
        fn as_usize(self) -> usize {
            self as usize
        }
    }

    let string_regex = "^\"([^\"\\\\]|\\\\.)*\"";
    let number_regex = "^-?(?:0|[1-9]\\d*)(?:\\.\\d+)?(?:[eE][+-]?\\d+)?";
    let whitespace_regex = "^[ \\n\\r\\v]*";
    let lexer = LexerBuilder::new(whitespace_regex)
        .regex(string_regex, JString)
        .regex(number_regex, Number)
        .constant("true", True)
        .constant("false", False)
        .constant("null", Null)
        .constant(":", Colon)
        .constant(",", Comma)
        .constant("[", OpenBracket)
        .constant("]", CloseBracket)
        .constant("{", OpenBrace)
        .constant("}", CloseBrace)
        .build()
        .unwrap();

    let lex = |source| {
        lexer
            .lex(source)
            .map(|l| &source[l.span.0..l.span.1])
            .collect::<Vec<_>>()
    };
    assert_eq!(lex("3.1e5"), vec!["3.1e5"]);
    assert_eq!(
        lex("{false]true  [5\"5\\\"\""),
        vec!["{", "false", "]", "true", "[", "5", "\"5\\\"\""]
    );

    /*
    use std::fs::File;
    use std::io::{BufRead, BufReader};
    let file = File::open("large-file.json").unwrap();
    let reader = BufReader::new(file);
    let mut lexeme_count = 0;
    for line in reader.lines() {
        let line = &line.unwrap();
        for _ in lexer.lex(line) {
            lexeme_count += 1;
        }
    }
    // This could use some external verification
    assert_eq!(lexeme_count, 2507032);
    */
}
