use crate::{Token, NAME_BLANK, NAME_JUXTAPOSE, TOKEN_BLANK, TOKEN_JUXTAPOSE};
use std::fmt;

/// Precedence level. Smaller is tighter / wins.
pub type Prec = u16;

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
    Infix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Clone)]
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

    pub(crate) fn new_atom(name: &str, _token: Token) -> Op {
        Op::new_unchecked(name, Fixity::Nilfix, Assoc::Left, 0, vec![name.to_owned()])
    }

    pub(crate) fn new_blank() -> Op {
        Op::new_unchecked(
            NAME_BLANK,
            Fixity::Nilfix,
            Assoc::Left,
            0,
            vec![NAME_BLANK.to_owned()],
        )
    }

    pub(crate) fn new_juxtapose(assoc: Assoc, prec: Prec) -> Op {
        Op::new_unchecked(
            NAME_JUXTAPOSE,
            Fixity::Infix,
            assoc,
            prec,
            vec![NAME_JUXTAPOSE.to_owned()],
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
            (Prefix, Left) => (None, Some(prec - 1)),
            (Prefix, Right) => (None, Some(prec)),
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Infix, Left) => (Some(prec), Some(prec - 1)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
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

impl fmt::Debug for Op {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}:\t{}\t{}\t{}",
            self.name, self.fixity, self.assoc, self.prec
        )?;
        for token in &self.tokens {
            write!(f, "\t{}", token)?;
        }
        Ok(())
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
