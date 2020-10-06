use crate::lexing::Lexer;
use crate::lexing::Token as TokenTrait;
use crate::shunting::OpSpec as RealOpSpec;
use crate::shunting::{Assoc, Fixity, Shunter, ShunterBuilder};
use std::collections::HashMap;

pub type RegexPattern = String;
type Name = String;

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
    atoms: Vec<(Name, Pattern)>,
    // TODO: Is this split needed?
    ops_by_name: HashMap<Name, OpSpec>,
    ops_by_prec: Vec<(Assoc, Vec<Name>)>,
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
            atoms: Vec::new(),
            ops_by_name: HashMap::new(),
            ops_by_prec: Vec::new(),
        }
    }

    pub fn regex(mut self, name: &str, regex: &str) -> Self {
        let pattern = Pattern::Regex(regex.to_owned());
        self.atoms.push((name.to_owned(), pattern));
        self
    }

    pub fn constant(mut self, name: &str, constant: &str) -> Self {
        let pattern = Pattern::Constant(constant.to_owned());
        self.atoms.push((name.to_owned(), pattern));
        self
    }

    pub fn op_l(self, op: OpSpec) -> Self {
        self.add_op(Assoc::Left, op)
    }

    pub fn op_r(self, op: OpSpec) -> Self {
        self.add_op(Assoc::Right, op)
    }

    pub fn op(self, op: OpSpec) -> Self {
        assert!(op.fixity != Fixity::Infix);
        self.add_op(Assoc::NoAssoc, op)
    }

    fn add_op(mut self, assoc: Assoc, op: OpSpec) -> Self {
        self.ops_by_prec.push((assoc, vec![op.name.clone()]));
        self.ops_by_name.insert(op.name.clone(), op);
        self
    }

    pub fn ops_l(self, ops: Vec<OpSpec>) -> Self {
        self.add_ops(Assoc::Left, ops)
    }

    pub fn ops_r(self, ops: Vec<OpSpec>) -> Self {
        self.add_ops(Assoc::Right, ops)
    }

    pub fn ops(self, ops: Vec<OpSpec>) -> Self {
        let mut lprec_exists = false;
        let mut rprec_exists = false;
        for op in &ops {
            match op.fixity {
                Fixity::Nilfix => (),
                Fixity::Infix => {
                    lprec_exists = true;
                    rprec_exists = true;
                }
                Fixity::Prefix => {
                    rprec_exists = true;
                }
                Fixity::Suffix => {
                    lprec_exists = true;
                }
            }
        }
        if lprec_exists && rprec_exists {
            panic!("Must specify associativity");
        }
        self.add_ops(Assoc::NoAssoc, ops)
    }

    fn add_ops(mut self, assoc: Assoc, ops: Vec<OpSpec>) -> Self {
        self.ops_by_prec
            .push((assoc, ops.iter().map(|r| r.name.clone()).collect()));
        for op in ops {
            self.ops_by_name.insert(op.name.clone(), op);
        }
        self
    }

    // TODO: Errors
    pub fn build(mut self) -> Parser {
        let mut token_set = TokenSet::new();
        let mut shunter_builder = ShunterBuilder::new();
        let mut token_patterns: HashMap<Token, Pattern> = HashMap::new();
        for (name, pattern) in self.atoms {
            let token = match pattern.clone() {
                Pattern::Constant(constant) => token_set.insert_constant(constant),
                Pattern::Regex(regex) => token_set.insert_regex(regex),
            };
            token_patterns.insert(token, pattern);
            let op = RealOpSpec {
                name,
                tokens: vec![token],
                fixity: Fixity::Nilfix,
            };
            shunter_builder = shunter_builder.op(Assoc::NoAssoc, op);
        }
        for (assoc, prec_group) in self.ops_by_prec {
            let mut ops = vec![];
            for op_name in prec_group {
                let op = self.ops_by_name.remove(&op_name).unwrap();
                let mut tokens = Vec::new();
                for constant in op.tokens {
                    let token = token_set.insert_constant(constant.clone());
                    token_patterns.insert(token, Pattern::Constant(constant));
                    tokens.push(token);
                }
                ops.push(RealOpSpec {
                    name: op.name,
                    tokens,
                    fixity: op.fixity,
                });
            }
            shunter_builder = shunter_builder.ops(assoc, ops);
        }
        let lexer = token_set.into_lexer(self.whitespace);
        let shunter = shunter_builder.build();
        Parser {
            lexer,
            shunter,
            token_patterns,
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
macro_rules! prefix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::OpSpec {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Prefix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! suffix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::OpSpec {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Suffix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! infix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::OpSpec {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Infix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! circumfix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::OpSpec {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Nilfix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
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
