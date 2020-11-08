use crate::lexing::Token;

pub type Prec = u16;
pub type NT = u16;

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
pub struct Op<T: Token> {
    pub(super) name: String,
    pub(super) prec: Prec,
    pub(super) assoc: Assoc,
    pub(super) fixity: Fixity,
    pub(super) first_token: Option<T>,
    pub(super) followers: Vec<Follower<T>>,
    pub(super) subgrammar: NT,
    pub(super) left_prec: Option<Prec>,  // computed
    pub(super) right_prec: Option<Prec>, // computed
}

#[derive(Debug, Clone)]
pub(super) struct Follower<T> {
    pub(super) subgrammar_index: NT,
    pub(super) token: T,
}

impl<'g, T: Token> Op<T> {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arity(&self) -> usize {
        let mut arity = self.num_holes();
        if self.left_prec.is_some() {
            arity += 1;
        }
        if self.right_prec.is_some() {
            arity += 1;
        }
        arity
    }

    pub fn num_holes(&self) -> usize {
        self.followers.len()
    }

    pub fn fixity(&self) -> Fixity {
        self.fixity
    }

    pub fn first_token(&self) -> Option<T> {
        self.first_token
    }

    pub fn tokens(&self) -> impl Iterator<Item = T> + '_ {
        self.first_token
            .iter()
            .copied()
            .chain(self.followers.iter().map(|f| f.token))
    }
}

impl<T: Token> Op<T> {
    pub(super) fn new(
        name: String,
        first_token: Option<T>,
        followers: Vec<Follower<T>>,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
        subgrammar: NT,
    ) -> Op<T> {
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
        }
    }
}
