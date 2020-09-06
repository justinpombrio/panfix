use crate::lexer::Lexer;
use crate::lexer::Token as TokenTrait;
use crate::shunter::{Operator, Prec, Shunter};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    LexError,
    Juxtapose,
    MissingAtom,
    MissingSep,
    ExtraSep,
    Normal(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Prefix,
    Suffix,
    InfixL,
    InfixR,
}

pub struct Op {
    pub name: String,
    pub fixity: Fixity,
    pub tokens: Vec<String>,
}

pub struct Grammar {
    whitespace: String,
    juxtapose_prec: Option<(Prec, Prec)>,
    regexes: Vec<(String, String)>,
    constants: Vec<(String, String)>,
    ops: Vec<Vec<Op>>,
}

pub struct Parser {
    pub(super) lexer: Lexer<Token>,
    pub(super) shunter: Shunter<Token>,
}

struct TokenSet {
    next_token: u32,
    regexes: Vec<(String, Token)>,
    constants: HashMap<String, Token>,
}

impl TokenTrait for Token {
    const LEX_ERROR: Token = Token::LexError;
    const JUXTAPOSE: Token = Token::Juxtapose;
    const MISSING_ATOM: Token = Token::MissingAtom;
    const MISSING_SEP: Token = Token::MissingSep;
    const EXTRA_SEP: Token = Token::ExtraSep;

    fn as_usize(self) -> usize {
        use Token::*;
        match self {
            LexError => 0,
            Juxtapose => 1,
            MissingAtom => 2,
            MissingSep => 3,
            ExtraSep => 4,
            Normal(tok) => tok as usize + 5,
        }
    }
}

impl Grammar {
    pub fn new(whitespace: String) -> Grammar {
        Grammar {
            whitespace,
            juxtapose_prec: None,
            regexes: Vec::new(),
            constants: Vec::new(),
            ops: Vec::new(),
        }
    }

    pub fn juxtapose(mut self, left_prec: Prec, right_prec: Prec) -> Self {
        assert!(self.juxtapose_prec.is_none());
        self.juxtapose_prec = Some((left_prec, right_prec));
        self
    }

    pub fn regex(mut self, name: &str, regex: &str) -> Self {
        self.regexes.push((name.to_owned(), regex.to_owned()));
        self
    }

    pub fn constant(mut self, name: &str, constant: &str) -> Self {
        self.regexes.push((name.to_owned(), constant.to_owned()));
        self
    }

    pub fn op(mut self, op: Op) -> Self {
        self.ops.push(vec![op]);
        self
    }

    pub fn op_group(mut self, ops: Vec<Op>) -> Self {
        self.ops.push(ops);
        self
    }

    // TODO: Errors
    pub fn build(self) -> Parser {
        let mut token_set = TokenSet::new();
        let mut ops: Vec<Operator<Token>> = Vec::new();
        for (name, regex) in self.regexes {
            let token = token_set.insert_regex(regex);
            ops.push(Operator {
                name,
                left_prec: None,
                right_prec: None,
                tokens: vec![token],
            });
        }
        for (name, constant) in self.constants {
            let token = token_set.insert_constant(constant);
            ops.push(Operator {
                name,
                left_prec: None,
                right_prec: None,
                tokens: vec![token],
            });
        }
        let mut prec = 10;
        for prec_group in self.ops {
            for op in prec_group {
                let Op {
                    name,
                    fixity,
                    tokens,
                } = op;
                let (left_prec, right_prec) = match fixity {
                    Fixity::Prefix => (None, Some(prec)),
                    Fixity::Suffix => (Some(prec), None),
                    Fixity::InfixL => (Some(prec + 1), Some(prec)),
                    Fixity::InfixR => (Some(prec), Some(prec + 1)),
                };
                let tokens = tokens
                    .into_iter()
                    .map(|c| token_set.insert_constant(c))
                    .collect();
                ops.push(Operator {
                    name,
                    left_prec,
                    right_prec,
                    tokens,
                });
            }
            prec += 10;
        }
        let lexer = token_set.into_lexer(self.whitespace);
        let shunter = Shunter::new(ops, self.juxtapose_prec);
        Parser { lexer, shunter }
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

    fn insert_regex(&mut self, regex: String) -> Token {
        let token = self.new_token();
        self.regexes.push((regex, token));
        token
    }

    fn insert_constant(&mut self, constant: String) -> Token {
        if let Some(token) = self.constants.get(&constant) {
            *token
        } else {
            let token = self.new_token();
            self.constants.insert(constant, token);
            token
        }
    }

    fn into_lexer(self, whitespace: String) -> Lexer<Token> {
        Lexer::new(
            whitespace,
            self.regexes,
            self.constants.into_iter().collect(),
        )
        .unwrap()
    }
}

#[macro_export]
macro_rules! prefix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parser::Op {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parser::Fixity::Prefix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! suffix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parser::Op {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parser::Fixity::Suffix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! infix_l {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parser::Op {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parser::Fixity::InfixL,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! infix_r {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parser::Op {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parser::Fixity::InfixR,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}
