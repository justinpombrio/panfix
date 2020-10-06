use crate::lexing::Token;

pub type Prec = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
    NoAssoc,
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
    pub(super) left_prec: Option<Prec>,
    pub(super) right_prec: Option<Prec>,
    pub(super) tokens: Vec<T>,
}

impl<'g, T: Token> Op<T> {
    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn tokens(&self) -> &[T] {
        &self.tokens
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
        self.tokens.len().saturating_sub(1)
    }

    pub fn fixity(&self) -> Fixity {
        self.fixity
    }
}

impl<T: Token> Op<T> {
    pub fn new(name: String, tokens: Vec<T>, prec: Prec, assoc: Assoc, fixity: Fixity) -> Op<T> {
        use Assoc::{Left, NoAssoc, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};
        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, NoAssoc) => (None, None),
            (Nilfix, Left) | (Nilfix, Right) => {
                panic!("Nilfix op {} can't have an associativity", name)
            }
            (Prefix, Left) => (None, Some(prec)),
            (Prefix, Right) => (None, Some(prec)),
            (Prefix, NoAssoc) => (None, Some(prec)), // allowed if only op in group
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Suffix, NoAssoc) => (Some(prec), None), // allowed if only op in group
            (Infix, Left) => (Some(prec), Some(prec)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
            (Infix, NoAssoc) => panic!("Op {} must have an associativity", name),
        };
        Op {
            name,
            prec,
            assoc,
            fixity,
            left_prec,
            right_prec,
            tokens,
        }
    }
}
