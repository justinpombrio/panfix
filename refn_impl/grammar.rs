use regex::Regex;
use std::collections::HashMap;

pub type Token = usize;
pub type NonTerminal = &'static str;
pub type Prec = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Fixity {
    Nilfix,
    Prefix,
    Suffix,
    Infix,
}

impl Fixity {
    fn arity(&self) -> usize {
        use Fixity::*;
        match self {
            Nilfix => 0,
            Prefix => 1,
            Suffix => 1,
            Infix => 2,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Op {
    pub(super) name: String,
    pub(super) fixity: Fixity,
    pub(super) prec: Prec,
    pub(super) assoc: Assoc,
    pub(super) left_prec: Option<Prec>,
    pub(super) right_prec: Option<Prec>,
    pub(super) token: Option<Token>,
    pub(super) followers: Vec<(NonTerminal, Token)>,
}

impl Op {
    pub fn arity(&self) -> usize {
        if self.name == "$Juxtapose" {
            return 2;
        }
        self.fixity.arity() + self.followers.len()
    }

    pub fn new_atom(name: &str, token: Option<Token>) -> Op {
        Self::new(name, 0, Assoc::Right, token, vec![], Fixity::Nilfix)
    }

    pub fn new(
        name: &str,
        prec: Prec,
        assoc: Assoc,
        token: Option<Token>,
        followers: Vec<(NonTerminal, Token)>,
        fixity: Fixity,
    ) -> Op {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};
        let prec = prec;
        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, _) => (None, None),
            (Prefix, Left) => (None, Some(prec - 1)),
            (Prefix, Right) => (None, Some(prec)),
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Infix, Left) => (Some(prec), Some(prec - 1)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
        };
        Op {
            name: name.to_owned(),
            prec,
            assoc,
            fixity,
            left_prec,
            right_prec,
            token,
            followers,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Grammar {
    // Lexing
    pub(super) whitespace: Regex,
    pub(super) regexes: Vec<(Regex, Token)>,
    pub(super) constants: Vec<(String, Token)>,
    pub(super) token_display: Vec<String>,
    // Parsing
    pub(super) subgrammars: HashMap<String, Subgrammar>,
}

#[derive(Debug, Clone)]
pub struct Subgrammar {
    pub name: String,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub token_to_prefixy_op: Vec<Option<Op>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub token_to_suffixy_op: Vec<Option<Op>>,
    pub missing_atom: Op,
    pub juxtapose: Op,
}
