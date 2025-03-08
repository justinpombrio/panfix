use crate::{TokenId, NAME_BLANK, NAME_ERROR, NAME_JUXTAPOSE};
use std::fmt;

/// Precedence level. Smaller is tighter / wins.
pub type Prec = u16;

/// Whether an operator takes an argument on the left and/or on the right. For example:
///
/// - `3` takes no arguments, so it is a `Nilfix` operator.
/// - `_ [ _ ]` (indexing) takes an argument only on the left, so it is a `Suffix` operator. (The
///   "internal argument" between the brackets does not count towards the fixity.)
/// - `! _` (not) takes an argument only on the right, so it is a `Prefix` oeprator.
/// - `_ - _` takes an argument on both sides, so it is an infix operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Fixity {
    Nilfix,
    Prefix,
    Suffix,
    Infix,
}

/// Whether an operator is left or right associative. For example:
///
/// - Subtraction is left associative because `0 - 10 - 1` is equal to `(0 - 10) - 1 = -11` rather
///   than `0 - (10 - 1) = -9`.
/// - `_ . _` (field access) is also left associative because `person.birthdate.year` is
///   equal to `(person.birthdate).year` and not `person.(birthdate.year)`.
/// - Right associativity is less common, but in languages that allow chained variable declarations,
///   `x = y = 3` is right-associative. If it were left-associative, it would be equal to `(x = y) =
///   3`, which would attempt to set `x` equal to `y` before `y` had been defined.  Instead it is
///   right-associative (`InfixR`) and thus equal to `x = (y = 3)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub(crate) struct Op {
    pub(crate) name: String,
    pub(crate) fixity: Fixity,
    pub(crate) assoc: Assoc,
    pub(crate) prec: Prec,
    pub(crate) tokens: Vec<String>,
    // computed
    pub(crate) arity: usize,
    pub(crate) left_prec: Option<Prec>,
    pub(crate) right_prec: Option<Prec>,
}

impl Op {
    pub(crate) fn new(
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        tokens: Vec<String>,
    ) -> Op {
        assert_ne!(name, NAME_BLANK);
        assert_ne!(name, NAME_JUXTAPOSE);
        Op::new_unchecked(name, fixity, assoc, prec, tokens)
    }

    pub(crate) fn new_atom(name: &str, _token: TokenId) -> Op {
        Op::new_unchecked(name, Fixity::Nilfix, Assoc::Left, 0, vec![name.to_owned()])
    }

    pub(crate) fn new_error() -> Op {
        Op::new_unchecked(
            NAME_ERROR,
            Fixity::Nilfix,
            Assoc::Left,
            0,
            vec!["".to_owned()],
        )
    }

    pub(crate) fn new_blank() -> Op {
        Op::new_unchecked(
            NAME_BLANK,
            Fixity::Nilfix,
            Assoc::Left,
            0,
            vec!["".to_owned()],
        )
    }

    pub(crate) fn new_juxtapose(assoc: Assoc, prec: Prec) -> Op {
        Op::new_unchecked(
            NAME_JUXTAPOSE,
            Fixity::Infix,
            assoc,
            prec,
            vec!["".to_owned()],
        )
    }

    fn new_unchecked(
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        tokens: Vec<String>,
    ) -> Op {
        use Assoc::{Left, Right};
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, _) => (None, None),
            (Prefix, Left) => (None, Some(prec)),
            (Prefix, Right) => (None, Some(prec + 1)),
            (Suffix, Left) => (Some(prec + 1), None),
            (Suffix, Right) => (Some(prec), None),
            (Infix, Left) => (Some(prec + 1), Some(prec)),
            (Infix, Right) => (Some(prec), Some(prec + 1)),
        };
        let arity = match fixity {
            Nilfix => tokens.len() - 1,
            Prefix | Suffix => tokens.len(),
            Infix => tokens.len() + 1,
        };
        Op {
            name: name.to_owned(),
            fixity,
            assoc,
            prec,
            tokens,
            arity,
            left_prec,
            right_prec,
        }
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}
