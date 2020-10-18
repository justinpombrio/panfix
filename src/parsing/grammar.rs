use crate::lexing::Lexer;
use crate::lexing::Token as TokenTrait;
use crate::shunting::OpSpec as RealOpSpec;
use crate::shunting::{Fixity, Shunter, ShunterBuilder};
use std::collections::HashMap;

pub type RegexPattern = String;

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    LexError,
    Normal(u32),
}

pub struct OpSpec {
    pub name: String,
    pub fixity: Fixity,
    pub tokens: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Constant(String),
    Regex(RegexPattern),
}

pub struct Grammar {
    whitespace: RegexPattern,
    token_set: TokenSet,
    builder: ShunterBuilder<Token>,
    token_patterns: HashMap<Token, Pattern>,
}

pub struct Parser {
    pub(super) lexer: Lexer<Token>,
    pub(super) shunter: Shunter<Token>,
    pub(super) token_patterns: HashMap<Token, Pattern>,
}

struct TokenSet {
    next_token: u32,
    regexes: Vec<(RegexPattern, Token)>,
    constants: HashMap<String, Token>,
}

impl TokenTrait for Token {
    const LEX_ERROR: Token = Token::LexError;

    fn as_usize(self) -> usize {
        use Token::*;
        match self {
            LexError => 0,
            Normal(tok) => tok as usize + 1,
        }
    }
}

impl Grammar {
    pub fn new(whitespace: &str) -> Grammar {
        Grammar {
            whitespace: whitespace.to_string(),
            token_set: TokenSet::new(),
            builder: ShunterBuilder::new(),
            token_patterns: HashMap::new(),
        }
    }

    pub fn regex(self, name: &str, regex: &str) -> Self {
        let pattern = Pattern::Regex(regex.to_owned());
        self.add_atom(name.to_owned(), pattern)
    }

    pub fn constant(self, name: &str, constant: &str) -> Self {
        let pattern = Pattern::Constant(constant.to_owned());
        self.add_atom(name.to_owned(), pattern)
    }

    pub fn op_l(mut self, op: OpSpec) -> Self {
        let op = self.convert_op(op);
        self.builder = self.builder.op_l(op);
        self
    }

    pub fn op_r(mut self, op: OpSpec) -> Self {
        let op = self.convert_op(op);
        self.builder = self.builder.op_r(op);
        self
    }

    pub fn op(mut self, op: OpSpec) -> Self {
        let op = self.convert_op(op);
        self.builder = self.builder.op(op);
        self
    }

    pub fn ops_l(mut self, ops: Vec<OpSpec>) -> Self {
        let ops = ops.into_iter().map(|op| self.convert_op(op)).collect();
        self.builder = self.builder.ops_l(ops);
        self
    }

    pub fn ops_r(mut self, ops: Vec<OpSpec>) -> Self {
        let ops = ops.into_iter().map(|op| self.convert_op(op)).collect();
        self.builder = self.builder.ops_r(ops);
        self
    }

    fn add_atom(mut self, name: String, pattern: Pattern) -> Self {
        let token = match pattern.clone() {
            Pattern::Constant(constant) => self.token_set.insert_constant(constant),
            Pattern::Regex(regex) => self.token_set.insert_regex(regex),
        };
        self.token_patterns.insert(token, pattern);
        let op = RealOpSpec {
            name,
            tokens: vec![token],
            fixity: Fixity::Nilfix,
        };
        self.builder = self.builder.op(op);
        self
    }

    fn convert_op(&mut self, op: OpSpec) -> RealOpSpec<Token> {
        let mut tokens = vec![];
        for constant in op.tokens {
            let token = self.token_set.insert_constant(constant.clone());
            self.token_patterns
                .insert(token, Pattern::Constant(constant));
            tokens.push(token);
        }
        RealOpSpec {
            name: op.name,
            tokens,
            fixity: op.fixity,
        }
    }

    // TODO: Errors
    pub fn build(self) -> Parser {
        let lexer = self.token_set.into_lexer(self.whitespace);
        let shunter = self.builder.build();
        Parser {
            lexer,
            shunter,
            token_patterns: self.token_patterns,
        }
    }
}

impl TokenSet {
    fn new() -> TokenSet {
        TokenSet {
            next_token: 0,
            regexes: Vec::new(),
            constants: HashMap::new(),
        }
    }

    fn new_token(&mut self) -> Token {
        let token = Token::Normal(self.next_token);
        self.next_token += 1;
        token
    }

    fn insert_regex(&mut self, regex: RegexPattern) -> Token {
        let token = self.new_token();
        self.regexes.push((regex, token));
        token
    }

    fn insert_constant(&mut self, constant: String) -> Token {
        if constant.is_empty() {
            panic!("Constants cannot be empty! Add at least one character to the token.");
        }
        if let Some(token) = self.constants.get(&constant) {
            *token
        } else {
            let token = self.new_token();
            self.constants.insert(constant, token);
            token
        }
    }

    fn into_lexer(self, whitespace: RegexPattern) -> Lexer<Token> {
        Lexer::new(
            whitespace,
            self.regexes,
            self.constants.into_iter().collect(),
        )
        .unwrap()
    }
}

#[macro_export]
macro_rules! op {
    ($name:ident : _ $($tokens:literal)* _) => {
        op!(@ $name ($crate::parsing::Fixity::Infix) [] $($tokens)*)
    };

    ($name:ident : $($tokens:literal)* _) => {
        op!(@ $name ($crate::parsing::Fixity::Prefix) [] $($tokens)*)
    };

    ($name:ident : _ $($tokens:literal)*) => {
        op!(@ $name ($crate::parsing::Fixity::Suffix) [] $($tokens)*)
    };

    ($name:ident : $($tokens:literal)*) => {
        op!(@ $name ($crate::parsing::Fixity::Nilfix) [] $($tokens)*)
    };

    (@ $name:ident ($fixity:expr) [$($tokens:tt)*] $token:literal $($rest:literal)*) => {
        op!(@ $name ($fixity) [$($tokens)* $token.to_string() ,] $($rest)*)
    };

    (@ $name:ident ($fixity:expr) [$($tokens:tt)*]) => {
        $crate::parsing::OpSpec {
            name: stringify!($name).to_string(),
            fixity: $fixity,
            tokens: vec![$($tokens)*],
        }
    };
}

#[test]
fn test_op_macro() {
    assert_eq!(op!(Colon: _ ":" _).fixity, Fixity::Infix);
    assert_eq!(op!(Parens: "(" ")").fixity, Fixity::Nilfix);
}

#[macro_export]
macro_rules! juxtapose {
    () => {
        $crate::parsing::OpSpec {
            name: "$Juxtapose".to_owned(),
            fixity: $crate::parsing::Fixity::Infix,
            tokens: vec![],
        }
    };
}
