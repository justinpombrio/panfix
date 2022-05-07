use crate::lexer::Token;
use std::fmt;

/// Precedence level. Smaller is tighter / wins.
pub type Prec = u16;
/// A category of thing to parse. Every operator is part of a sort. For example, common sorts might
/// include "expression", "statement", "type", and "argument_list".  (If you've heard of
/// non-terminals, a sort is essentially a non-terminal.)
pub type Sort = String;
pub(crate) type SortId = usize;

/// Whether the operator takes an argument on the left, on the right, both, or neither. And
/// furthermore, if the operator takes an argument on both sides, whether
/// For
/// example:
///
/// - `3` takes no arguments, so it is a `Nilfix` operator.
/// - `_ [ _ ]` (indexing) takes an argument only on the left, so it is a `Suffix` operator. (The
/// "internal argument" between the brackets does not count towards the fixity.)
/// - `! _` (not) takes an argument only on the right, so it is a `Prefix` oeprator.
/// - `_ - _` takes an argument on both sides, so it is an infix operator. This leaves the question
/// of whether it is `InfixL` ("left-associative") or `InfixR` ("right-associative"). Subtraction
/// is left-associative (`InfixL`) because `0 - 10 - 1` is equal to `(0 - 10) - 1 = -11` rather
/// than `0 - (10 - 1) = -9`.
/// - `_ . _` (field access) is also `InfixL` (left-associative) because `person.birthdate.year` is
/// equal to `(person.birthdate).year` and not `person.(birthdate.year)`.
/// - Right-associativity is less common, but in languages that allow chained variable declarations,
/// `x = y = 3` is right-associative. If it were left-associative, it would be equal to `(x = y) =
/// 3`, which would attempt to set `x` equal to `y` before `y` had been defined.  Instead it is
/// right-associative (`InfixR`) and thus equal to `x = (y = 3)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Fixity {
    Nilfix,
    Prefix,
    Suffix,
    InfixL,
    InfixR,
}

#[derive(Debug, Clone)]
pub(crate) struct Op {
    pub(crate) name: String,
    pub(crate) fixity: Fixity,
    pub(crate) prec: Prec,
    pub(crate) first_token: Option<Token>,
    pub(crate) followers: Vec<(SortId, Token)>,
    pub(crate) left_prec: Option<Prec>,
    pub(crate) right_prec: Option<Prec>,
    pub(crate) arity: usize,
}

impl Op {
    pub(crate) fn new(
        name: &str,
        fixity: Fixity,
        prec: Prec,
        first_token: Token,
        followers: Vec<(SortId, Token)>,
    ) -> Op {
        assert_ne!(name, "$Juxtapose");
        assert_ne!(name, "$MissingAtom");
        Op::new_unchecked(name, fixity, prec, Some(first_token), followers)
    }

    pub(crate) fn new_atom(name: &str, token: Token) -> Op {
        Op::new_unchecked(name, Fixity::Nilfix, 0, Some(token), vec![])
    }

    pub(crate) fn new_missing_atom() -> Op {
        Op::new_unchecked("$MissingAtom", Fixity::Nilfix, 0, None, vec![])
    }

    pub(crate) fn new_juxtapose(prec: Prec) -> Op {
        Op::new_unchecked("$Juxtapose", Fixity::InfixL, prec, None, vec![])
    }

    fn new_unchecked(
        name: &str,
        fixity: Fixity,
        prec: Prec,
        first_token: Option<Token>,
        followers: Vec<(SortId, Token)>,
    ) -> Op {
        use Fixity::{InfixL, InfixR, Nilfix, Prefix, Suffix};

        let (left_prec, right_prec) = match fixity {
            Nilfix => (None, None),
            Prefix => (None, Some(prec)),
            Suffix => (Some(prec), None),
            InfixL => (Some(prec), Some(prec)),
            InfixR => (Some(prec - 1), Some(prec)),
        };
        let arity = match fixity {
            Nilfix => followers.len(),
            Prefix | Suffix => followers.len() + 1,
            InfixL | InfixR => followers.len() + 2,
        };
        Op {
            name: name.to_owned(),
            fixity,
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
            Fixity::InfixL => write!(f, "infixl"),
            Fixity::InfixR => write!(f, "infixr"),
        }
    }
}
