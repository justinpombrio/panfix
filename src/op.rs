use crate::{Token, TokenId};
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
pub(crate) struct Op<T: Token> {
    pub(crate) token: T,
    pub(crate) fixity: Fixity,
    pub(crate) assoc: Assoc,
    pub(crate) prec: Prec,
    // computed
    pub(crate) arity: usize,
    pub(crate) left_prec: Option<Prec>,
    pub(crate) right_prec: Option<Prec>,
}

impl<T: Token> Op<T> {
    pub(crate) fn new(
        token: T,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        num_tokens: usize,
    ) -> Op<T> {
        assert_ne!(token, T::LEX_ERROR);
        assert_ne!(token, T::BLANK);
        assert_ne!(token, T::JUXTAPOSE);
        Op::new_unchecked(token, fixity, assoc, prec, num_tokens)
    }

    pub(crate) fn new_atom(token: T, _token_id: TokenId) -> Op<T> {
        Op::new_unchecked(token, Fixity::Nilfix, Assoc::Left, 0, 1)
    }

    pub(crate) fn new_error() -> Op<T> {
        Op::new_unchecked(T::LEX_ERROR, Fixity::Nilfix, Assoc::Left, 0, 1)
    }

    pub(crate) fn new_blank() -> Op<T> {
        Op::new_unchecked(T::BLANK, Fixity::Nilfix, Assoc::Left, 0, 1)
    }

    pub(crate) fn new_juxtapose(assoc: Assoc, prec: Prec) -> Op<T> {
        Op::new_unchecked(T::JUXTAPOSE, Fixity::Infix, assoc, prec, 1)
    }

    fn new_unchecked(
        token: T,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        num_tokens: usize,
    ) -> Op<T> {
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
            Nilfix => num_tokens - 1,
            Prefix | Suffix => num_tokens,
            Infix => num_tokens + 1,
        };
        Op {
            token,
            fixity,
            assoc,
            prec,
            arity,
            left_prec,
            right_prec,
        }
    }
}

impl<T: Token> fmt::Display for Op<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.token)
    }
}
