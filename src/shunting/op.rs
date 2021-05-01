use std::fmt::{Debug, Display};
use std::hash::Hash;

pub type Prec = u16;
pub type NT = usize;
pub type Token = usize;
pub const LEX_ERROR: Token = 0;

pub trait OpName: Debug + Display + Clone + Copy + PartialEq + Eq + Hash {
    const MISSING_ATOM: Self;
    const JUXTAPOSE: Self;
}

impl<'s> OpName for &'s str {
    const MISSING_ATOM: &'s str = "MissingAtom";
    const JUXTAPOSE: &'s str = "Juxtapose";
}

impl OpName for usize {
    const MISSING_ATOM: usize = 0;
    const JUXTAPOSE: usize = 1;
}

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
pub struct Op<N: OpName> {
    pub(super) name: N,
    pub(super) prec: Prec,
    pub(super) assoc: Assoc,
    pub(super) fixity: Fixity,
    pub(super) first_token: Option<Token>,
    pub(super) followers: Vec<(NT, Token)>,
    pub(super) subgrammar: NT,
    pub(super) left_prec: Option<Prec>,  // computed
    pub(super) right_prec: Option<Prec>, // computed
    pub(super) arity: usize,             // computed
}

impl<N: OpName> Op<N> {
    pub fn name(&self) -> N {
        self.name
    }

    pub fn arity(&self) -> usize {
        self.arity
    }

    pub fn num_holes(&self) -> usize {
        self.followers.len()
    }

    pub fn fixity(&self) -> Fixity {
        self.fixity
    }

    pub fn first_token(&self) -> Option<Token> {
        self.first_token
    }

    pub fn tokens(&self) -> impl Iterator<Item = Token> + '_ {
        self.first_token
            .iter()
            .copied()
            .chain(self.followers.iter().map(|(_, tok)| *tok))
    }

    pub fn token_before_child(&self, child_index: usize) -> Option<Token> {
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

    pub fn token_after_child(&self, child_index: usize) -> Option<Token> {
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

    pub(super) fn new_juxtapose(subgrammar: NT, prec: Prec) -> Op<N> {
        Op::new(
            N::JUXTAPOSE,
            None,
            vec![],
            prec,
            Assoc::Right,
            Fixity::Infix,
            subgrammar,
        )
    }

    pub(super) fn new_missing_atom(subgrammar: NT) -> Op<N> {
        Op::new(
            N::MISSING_ATOM,
            None,
            vec![],
            0,
            Assoc::Left,
            Fixity::Nilfix,
            subgrammar,
        )
    }

    pub(super) fn new(
        name: N,
        first_token: Option<Token>,
        followers: Vec<(NT, Token)>,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
        subgrammar: NT,
    ) -> Op<N> {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};
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
        Op {
            name,
            prec,
            assoc,
            fixity,
            first_token,
            followers,
            subgrammar,
            left_prec,
            right_prec,
            arity,
        }
    }
}
