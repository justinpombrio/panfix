use crate::lexing::Lexer;
use crate::lexing::Token as TokenTrait;
use crate::shunting::Rule as CompiledRule;
use crate::shunting::Shunter;
use std::collections::HashMap;

pub type RegexPattern = String;
type Name = String;

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    LexError,
    Juxtapose,
    MissingAtom,
    Normal(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Prefix,
    Suffix,
    Infix,
    Circumfix,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
    NoAssoc,
}

pub struct Grammar {
    whitespace: RegexPattern,
    atoms: Vec<(Name, Pattern)>,
    rules_by_name: HashMap<Name, Rule>,
    rules_by_prec: Vec<(Assoc, Vec<Name>)>,
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

    fn as_usize(self) -> usize {
        use Token::*;
        match self {
            LexError => 0,
            Juxtapose => 1,
            MissingAtom => 2,
            Normal(tok) => tok as usize + 5,
        }
    }
}

impl Grammar {
    pub fn new(whitespace: &str) -> Grammar {
        Grammar {
            whitespace: whitespace.to_string(),
            atoms: Vec::new(),
            rules_by_name: HashMap::new(),
            rules_by_prec: Vec::new(),
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

    pub fn rule_l(self, rule: Rule) -> Self {
        self.add_rule(Assoc::Left, rule)
    }

    pub fn rule_r(self, rule: Rule) -> Self {
        self.add_rule(Assoc::Right, rule)
    }

    pub fn rule(self, rule: Rule) -> Self {
        assert!(rule.fixity != Fixity::Infix);
        self.add_rule(Assoc::NoAssoc, rule)
    }

    fn add_rule(mut self, assoc: Assoc, rule: Rule) -> Self {
        self.rules_by_prec.push((assoc, vec![rule.name.clone()]));
        self.rules_by_name.insert(rule.name.clone(), rule);
        self
    }

    pub fn rules_l(self, rules: Vec<Rule>) -> Self {
        self.add_rules(Assoc::Left, rules)
    }

    pub fn rules_r(self, rules: Vec<Rule>) -> Self {
        self.add_rules(Assoc::Right, rules)
    }

    pub fn rules(self, rules: Vec<Rule>) -> Self {
        let mut lprec_exists = false;
        let mut rprec_exists = false;
        for rule in &rules {
            match rule.fixity {
                Fixity::Circumfix => (),
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
        self.add_rules(Assoc::NoAssoc, rules)
    }

    fn add_rules(mut self, assoc: Assoc, rules: Vec<Rule>) -> Self {
        self.rules_by_prec
            .push((assoc, rules.iter().map(|r| r.name.clone()).collect()));
        for rule in rules {
            self.rules_by_name.insert(rule.name.clone(), rule);
        }
        self
    }

    // TODO: Errors
    pub fn build(mut self) -> Parser {
        use Assoc::*;
        use Fixity::*;

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
        for (assoc, prec_group) in self.rules_by_prec {
            for rule_name in prec_group {
                let rule = self.rules_by_name.remove(&rule_name).unwrap();
                let Rule {
                    name,
                    fixity,
                    tokens: constants,
                } = rule;
                let (left_prec, right_prec) = match (fixity, assoc) {
                    (Prefix, Left) => (None, Some(prec)),
                    (Prefix, Right) => (None, Some(prec + 1)),
                    (Prefix, NoAssoc) => (None, Some(prec)),
                    (Suffix, Left) => (Some(prec + 1), None),
                    (Suffix, Right) => (Some(prec), None),
                    (Suffix, NoAssoc) => (Some(prec), None),
                    (Infix, Left) => (Some(prec + 1), Some(prec)),
                    (Infix, Right) => (Some(prec), Some(prec + 1)),
                    (Circumfix, _) => (None, None),
                    (Infix, NoAssoc) => panic!("Must specify an associativity"),
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
        let shunter = Shunter::new(rules);
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
macro_rules! infix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Infix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! circumfix {
    ( $name:expr, $( $token:expr ),* ) => {
        $crate::parsing::Rule {
            name: ::std::primitive::str::to_owned($name),
            fixity: $crate::parsing::Fixity::Circumfix,
            tokens: vec![$( ::std::primitive::str::to_owned($token) ),*],
        }
    };
}

#[macro_export]
macro_rules! juxtapose {
    () => {
        $crate::parsing::Rule {
            name: "$Juxtapose".to_owned(),
            fixity: $crate::parsing::Fixity::Infix,
            tokens: vec![],
        }
    };
}
