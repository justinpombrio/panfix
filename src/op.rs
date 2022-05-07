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
    pub(crate) name: String,
    pub(crate) fixity: Fixity,
    pub(crate) assoc: Assoc,
    pub(crate) prec: Prec,
    pub(crate) first_token: Option<Token>,
    pub(crate) followers: Vec<(NonTerminalId, Token)>,
    pub(crate) left_prec: Option<Prec>,
    pub(crate) right_prec: Option<Prec>,
    pub(crate) arity: usize,
}

impl Op {
    pub(crate) fn new(
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        first_token: Token,
        followers: Vec<(NonTerminalId, Token)>,
    ) -> Op {
        assert_ne!(name, "$Juxtapose");
        assert_ne!(name, "$MissingAtom");
        Op::new_unchecked(name, fixity, assoc, prec, Some(first_token), followers)
    }

    pub(crate) fn new_atom(name: &str, token: Token) -> Op {
        Op::new_unchecked(name, Fixity::Nilfix, Assoc::Left, 0, Some(token), vec![])
    }

    pub(crate) fn new_missing_atom() -> Op {
        Op::new_unchecked("$MissingAtom", Fixity::Nilfix, Assoc::Left, 0, None, vec![])
    }

    pub(crate) fn new_juxtapose(prec: Prec) -> Op {
        Op::new_unchecked(
            "$Juxtapose",
            Fixity::Infix,
            Assoc::Right,
            prec,
            None,
            vec![],
        )
    }

    fn new_unchecked(
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        first_token: Option<Token>,
        followers: Vec<(NonTerminalId, Token)>,
    ) -> Op {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, _) => (None, None),
            (Prefix, _) => (None, Some(prec)),
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Infix, Left) => (Some(prec), Some(prec)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
        };
        let arity = match fixity {
            Nilfix => followers.len(),
            Prefix | Suffix => followers.len() + 1,
            Infix => followers.len() + 2,
        };
        Op {
            name: name.to_owned(),
            fixity,
            assoc,
            prec,
            first_token,
            followers,
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

impl fmt::Display for Fixity {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Fixity::Nilfix => write!(f, "nilfix"),
            Fixity::Prefix => write!(f, "prefix"),
            Fixity::Suffix => write!(f, "suffix"),
            Fixity::Infix => write!(f, "infix"),
        }
    }
}

impl fmt::Display for Assoc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Assoc::Left => write!(f, "left"),
            Assoc::Right => write!(f, "right"),
        }
    }
}
