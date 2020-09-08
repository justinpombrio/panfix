use crate::lexing::Lexer;
use crate::lexing::Token as TokenTrait;
use crate::shunting::Rule as CompiledRule;
use crate::shunting::{Prec, Shunter};
use std::collections::HashMap;

pub type RegexPattern = String;
type Name = String;

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

pub struct Rule {
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
    juxtapose_prec: Option<(Prec, Prec)>,
    atoms: Vec<(Name, Pattern)>,
    rules_by_name: HashMap<Name, Rule>,
    rules_by_prec: Vec<Vec<Name>>,
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
    pub fn new(whitespace: &str) -> Grammar {
        Grammar {
            whitespace: whitespace.to_string(),
            juxtapose_prec: None,
            atoms: Vec::new(),
            rules_by_name: HashMap::new(),
            rules_by_prec: Vec::new(),
        }
    }

    pub fn juxtapose(mut self, left_prec: Prec, right_prec: Prec) -> Self {
        assert!(self.juxtapose_prec.is_none());
        self.juxtapose_prec = Some((left_prec, right_prec));
        self
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

    pub fn rule(mut self, rule: Rule) -> Self {
        self.rules_by_prec.push(vec![rule.name.clone()]);
        self.rules_by_name.insert(rule.name.clone(), rule);
        self
    }

    pub fn rule_group(mut self, rules: Vec<Rule>) -> Self {
        self.rules_by_prec
            .push(rules.iter().map(|r| r.name.clone()).collect());
        for rule in rules {
            self.rules_by_name.insert(rule.name.clone(), rule);
        }
        self
    }

    // TODO: Errors
    pub fn build(mut self) -> Parser {
        let mut token_set = TokenSet::new();
        let mut rules: Vec<CompiledRule<Token>> = Vec::new();
        let mut token_patterns: HashMap<Token, Pattern> = HashMap::new();
        for (name, pattern) in self.atoms {
            let token = match pattern.clone() {
                Pattern::Constant(constant) => token_set.insert_constant(constant),
                Pattern::Regex(regex) => token_set.insert_regex(regex),
            };
            token_patterns.insert(token, pattern);
            rules.push(CompiledRule {
                name,
                left_prec: None,
                right_prec: None,
                tokens: vec![token],
            });
        }
        let mut prec = 10;
        for prec_group in self.rules_by_prec {
            for rule_name in prec_group {
                let rule = self.rules_by_name.remove(&rule_name).unwrap();
                let Rule {
                    name,
                    fixity,
                    tokens: constants,
                } = rule;
                let (left_prec, right_prec) = match fixity {
                    Fixity::Prefix => (None, Some(prec)),
                    Fixity::Suffix => (Some(prec), None),
                    Fixity::InfixL => (Some(prec + 1), Some(prec)),
                    Fixity::InfixR => (Some(prec), Some(prec + 1)),
                };
                let mut tokens = Vec::new();
                for constant in constants {
                    let token = token_set.insert_constant(constant.clone());
                    token_patterns.insert(token, Pattern::Constant(constant));
                    tokens.push(token);
                }
                rules.push(CompiledRule {
                    name,
                    left_prec,
                    right_prec,
                    tokens,
                });
            }
            prec += 10;
        }
        let lexer = token_set.into_lexer(self.whitespace);
        let shunter = Shunter::new(rules, self.juxtapose_prec);
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
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Prefix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! suffix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Suffix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! infixl {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::InfixL,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! infixr {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::InfixR,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}
