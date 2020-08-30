use crate::Token;

pub type Prec = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Nilfix,
    Prefix(Prec),
    Suffix(Prec),
    Infix(Prec, Prec),
}

#[derive(Debug, Clone)]
pub struct Operator<T: Token> {
    pub name: String,
    pub left_prec: Option<Prec>,
    pub right_prec: Option<Prec>,
    pub tokens: Vec<T>,
}

#[derive(Debug, Clone)]
pub struct Grammar<T: Token> {
    pub(super) token_to_op: Vec<Option<Operator<T>>>,
    pub(super) missing: Operator<T>,
    pub(super) juxtapose: Operator<T>,
}

impl<'g, T: Token> Operator<T> {
    pub fn fixity(&self) -> Fixity {
        match (self.left_prec, self.right_prec) {
            (None, None) => Fixity::Nilfix,
            (Some(i), None) => Fixity::Suffix(i),
            (None, Some(i)) => Fixity::Prefix(i),
            (Some(i), Some(j)) => Fixity::Infix(i, j),
        }
    }

    pub fn arity(&self) -> usize {
        let mut arity = 0;
        if self.left_prec.is_some() {
            arity += 1;
        }
        if self.right_prec.is_some() {
            arity += 1;
        }
        arity
    }
}

impl<'g, T: Token> Grammar<T> {
    pub fn new(ops: Vec<Operator<T>>) -> Grammar<T> {
        let mut largest_token: usize = 0;
        for op in &ops {
            for token in &op.tokens {
                largest_token = largest_token.max(token.as_usize());
            }
        }
        let mut token_to_op = vec![None; largest_token + 1];
        let mut missing = None;
        let mut juxtapose = None;
        for op in ops {
            if op.name == "Missing" {
                assert!(op.left_prec.is_none());
                assert!(op.right_prec.is_none());
                missing = Some(op);
            } else if op.name == "Juxtapose" {
                assert!(op.left_prec.is_some());
                assert!(op.right_prec.is_some());
                juxtapose = Some(op);
            } else if let Some(token) = op.tokens.first() {
                let index = token.as_usize();
                token_to_op[index] = Some(op);
            }
        }
        // TODO: unwrap -> Err
        Grammar {
            token_to_op,
            missing: missing.unwrap().to_owned(),
            juxtapose: juxtapose.unwrap().to_owned(),
        }
    }
}
