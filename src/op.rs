use std::collections::HashMap;
use std::fmt;

pub type Prec = u16;
pub type NonTerminal = String;
pub type NonTerminalId = usize;
pub type Token = usize;

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

#[derive(Debug, Clone)]
pub struct Op {
    pub(crate) subgrammar: NonTerminal,
    pub(crate) name: String,
    pub(crate) prec: Prec,
    pub(crate) assoc: Assoc,
    pub(crate) fixity: Fixity,
    pub(crate) first_token: Option<Token>,
    pub(crate) followers: Vec<(NonTerminal, Token, String)>,
}

#[derive(Debug, Clone)]
pub struct CompiledOp {
    pub(crate) subgrammar: NonTerminalId,
    pub(crate) name: String,
    pub(crate) prec: Prec,
    pub(crate) assoc: Assoc,
    pub(crate) fixity: Fixity,
    pub(crate) first_token: Option<Token>,
    pub(crate) followers: Vec<(NonTerminalId, Token, String)>,
    pub(crate) left_prec: Option<Prec>,
    pub(crate) right_prec: Option<Prec>,
    pub(crate) arity: usize,
}

impl Op {
    pub fn new(
        subgrammar: NonTerminal,
        name: String,
        first_token: Token,
        followers: Vec<(NonTerminal, Token, String)>,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
    ) -> Op {
        assert_ne!(name, "$Juxtapose");
        assert_ne!(name, "$MissingAtom");
        Op::new_unchecked(
            subgrammar,
            name,
            Some(first_token),
            followers,
            prec,
            assoc,
            fixity,
        )
    }

    pub(crate) fn new_juxtapose(subgrammar: NonTerminal, prec: Prec) -> Op {
        Op::new_unchecked(
            subgrammar,
            "$Juxtapose".to_string(),
            None,
            vec![],
            prec,
            Assoc::Right,
            Fixity::Infix,
        )
    }

    pub(crate) fn new_missing_atom(subgrammar: NonTerminal) -> Op {
        Op::new_unchecked(
            subgrammar,
            "$MissingAtom".to_string(),
            None,
            vec![],
            0,
            Assoc::Left,
            Fixity::Nilfix,
        )
    }

    pub(crate) fn new_unchecked(
        subgrammar: NonTerminal,
        name: String,
        first_token: Option<Token>,
        followers: Vec<(NonTerminal, Token, String)>,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
    ) -> Op {
        Op {
            subgrammar,
            name,
            prec,
            assoc,
            fixity,
            first_token,
            followers,
        }
    }

    pub(crate) fn token_before_child(&self, child_index: usize) -> Option<Token> {
        use Fixity::*;

        let is_suffixy = match self.fixity {
            Suffix | Infix => true,
            Prefix | Nilfix => false,
        };
        match (child_index, is_suffixy) {
            (0, true) => None,
            (1, true) => self.first_token,
            (n, true) => Some(self.followers[n - 2].1),
            (0, false) => self.first_token,
            (n, false) => Some(self.followers[n - 1].1),
        }
    }

    pub(crate) fn token_after_child(&self, child_index: usize) -> Option<Token> {
        use Fixity::*;

        let is_suffixy = match self.fixity {
            Suffix | Infix => true,
            Prefix | Nilfix => false,
        };
        match (child_index, is_suffixy) {
            (0, true) => self.first_token,
            (n, true) => self.followers.get(n - 1).map(|f| f.1),
            (n, false) => self.followers.get(n).map(|f| f.1),
        }
    }

    pub(crate) fn compile(
        self,
        subgrammar_ids: &HashMap<NonTerminal, NonTerminalId>,
    ) -> CompiledOp {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let subgrammar = self.subgrammar;
        let name = self.name;
        let prec = self.prec;
        let assoc = self.assoc;
        let fixity = self.fixity;
        let first_token = self.first_token;
        let followers = self.followers;

        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, _) => (None, None),
            (Prefix, Left) => (None, Some(prec)),
            (Prefix, Right) => (None, Some(prec)),
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Infix, Left) => (Some(prec), Some(prec)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
        };
        let arity = match fixity {
            Nilfix => followers.len(),
            Prefix => followers.len() + 1,
            Suffix => followers.len() + 1,
            Infix => followers.len() + 2,
        };
        CompiledOp {
            subgrammar: subgrammar_ids[&subgrammar],
            name,
            prec,
            assoc,
            fixity,
            first_token: first_token,
            followers: followers
                .into_iter()
                .map(|(nt, tok, name)| (subgrammar_ids[&nt], tok, name))
                .collect::<Vec<_>>(),
            left_prec,
            right_prec,
            arity,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl CompiledOp {
    pub fn name(&self) -> &str {
        &self.name
    }
}

impl fmt::Display for CompiledOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
