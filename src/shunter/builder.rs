use super::grammar::{Grammar, Operator, Prec};
use crate::Token;

#[derive(Debug, Clone)]
pub struct GrammarBuilder<T: Token> {
    ops: Vec<Operator<T>>,
}

impl<T: Token> GrammarBuilder<T> {
    pub fn new() -> GrammarBuilder<T> {
        GrammarBuilder { ops: vec![] }
    }

    pub fn op(
        &mut self,
        name: &str,
        left_prec: Option<Prec>,
        right_prec: Option<Prec>,
        tokens: Vec<T>,
    ) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec,
            right_prec,
            tokens,
        });
        self
    }

    pub fn nilfix(&mut self, name: &str, token: T) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: None,
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn prefix(&mut self, name: &str, token: T, right_prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: None,
            right_prec: Some(right_prec),
            tokens: vec![token],
        });
        self
    }

    pub fn suffix(&mut self, name: &str, token: T, left_prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(left_prec),
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn infixl(&mut self, name: &str, token: T, prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec),
            tokens: vec![token],
        });
        self
    }

    pub fn infixr(&mut self, name: &str, token: T, prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec + 1),
            tokens: vec![token],
        });
        self
    }

    pub fn build(&self) -> Grammar<T> {
        Grammar::new(self.ops.clone())
    }
}
