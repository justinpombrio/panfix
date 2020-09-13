use super::shunter::{Prec, Rule, Shunter};
use crate::lexing::Token;

#[derive(Debug, Clone)]
pub struct ShunterBuilder<T: Token> {
    rules: Vec<Rule<T>>,
}

impl<T: Token> ShunterBuilder<T> {
    pub fn new() -> ShunterBuilder<T> {
        ShunterBuilder { rules: vec![] }
    }

    pub fn juxtapose_prec(mut self, lprec: Prec, rprec: Prec) -> Self {
        self.rules.push(Rule {
            name: "$Juxtapose".to_owned(),
            left_prec: Some(lprec),
            right_prec: Some(rprec),
            tokens: vec![],
        });
        self
    }

    pub fn mixfix(
        mut self,
        name: &str,
        left_prec: Option<Prec>,
        right_prec: Option<Prec>,
        tokens: Vec<T>,
    ) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec,
            right_prec,
            tokens,
        });
        self
    }

    pub fn nilfix(mut self, name: &str, token: T) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec: None,
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn prefix(mut self, name: &str, token: T, right_prec: Prec) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec: None,
            right_prec: Some(right_prec),
            tokens: vec![token],
        });
        self
    }

    pub fn suffix(mut self, name: &str, token: T, left_prec: Prec) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec: Some(left_prec),
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn infixl(mut self, name: &str, token: T, prec: Prec) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec),
            tokens: vec![token],
        });
        self
    }

    pub fn infixr(mut self, name: &str, token: T, prec: Prec) -> Self {
        self.rules.push(Rule {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec + 1),
            tokens: vec![token],
        });
        self
    }

    pub fn build(self) -> Shunter<T> {
        Shunter::new(self.rules)
    }
}

impl<T: Token> Default for ShunterBuilder<T> {
    fn default() -> Self {
        ShunterBuilder::new()
    }
}
