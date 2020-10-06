use super::op::{Assoc, Fixity, Op, Prec};
use super::shunt::Shunter;
use crate::lexing::Token;

#[derive(Debug, Clone)]
pub struct OpSpec<T: Token> {
    pub name: String,
    pub tokens: Vec<T>,
    pub fixity: Fixity,
}

#[derive(Debug, Clone)]
pub struct ShunterBuilder<T: Token> {
    prec: Prec,
    ops: Vec<Op<T>>,
}

impl<T: Token> OpSpec<T> {
    pub fn juxtapose() -> Self {
        OpSpec {
            name: "$Juxtapose".to_owned(),
            tokens: vec![],
            fixity: Fixity::Infix,
        }
    }

    pub fn nilfix(name: String, tokens: Vec<T>) -> Self {
        OpSpec {
            name,
            tokens,
            fixity: Fixity::Nilfix,
        }
    }

    pub fn prefix(name: String, tokens: Vec<T>) -> Self {
        OpSpec {
            name,
            tokens,
            fixity: Fixity::Prefix,
        }
    }

    pub fn suffix(name: String, tokens: Vec<T>) -> Self {
        OpSpec {
            name,
            tokens,
            fixity: Fixity::Suffix,
        }
    }

    pub fn infix(name: String, tokens: Vec<T>) -> Self {
        OpSpec {
            name,
            tokens,
            fixity: Fixity::Infix,
        }
    }

    fn to_op(self, prec: Prec, assoc: Assoc) -> Op<T> {
        Op::new(self.name, self.tokens, prec, assoc, self.fixity)
    }
}

impl<T: Token> ShunterBuilder<T> {
    pub fn new() -> ShunterBuilder<T> {
        ShunterBuilder {
            ops: vec![],
            prec: 2,
        }
    }

    pub fn op(mut self, assoc: Assoc, op: OpSpec<T>) -> Self {
        self.ops.push(op.to_op(self.prec, assoc));
        self.prec += 1;
        self
    }

    pub fn ops(mut self, assoc: Assoc, ops: Vec<OpSpec<T>>) -> Self {
        for op in ops {
            self.ops.push(op.to_op(self.prec, assoc));
        }
        self.prec += 1;
        self
    }

    pub fn build(self) -> Shunter<T> {
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let mut largest_token_index: usize = 0;
        for op in &self.ops {
            for token in &op.tokens {
                largest_token_index = largest_token_index.max(token.as_usize());
            }
        }
        let mut token_to_prefixy_op = vec![None; largest_token_index + 1];
        let mut token_to_suffixy_op = vec![None; largest_token_index + 1];

        let mut juxtapose = None;
        for op in self.ops {
            if &op.name == "$Juxtapose" {
                assert_eq!(op.tokens.len(), 0);
                assert_eq!(op.fixity, Fixity::Infix);
                juxtapose = Some(op);
                continue;
            }
            assert!(!op.tokens.is_empty());
            let token = op.tokens.first().unwrap();
            let index = token.as_usize();
            match op.fixity {
                Prefix | Nilfix => {
                    assert!(
                        token_to_prefixy_op[index].is_none(),
                        "Duplicate first op token: {:?}",
                        token
                    );
                    token_to_prefixy_op[index] = Some(op);
                }
                Suffix | Infix => {
                    assert!(
                        token_to_suffixy_op[index].is_none(),
                        "Duplicate first op token: {:?}",
                        token
                    );
                    token_to_suffixy_op[index] = Some(op);
                }
            }
        }
        let missing_atom = Op::new(
            "$MissingAtom".to_owned(),
            vec![],
            0,
            Assoc::NoAssoc,
            Fixity::Nilfix,
        );
        let juxtapose = juxtapose.unwrap_or(Op::new(
            "$Juxtapose".to_owned(),
            vec![],
            1,
            Assoc::Right,
            Fixity::Infix,
        ));
        // TODO: unwrap -> Err
        Shunter {
            token_to_prefixy_op,
            token_to_suffixy_op,
            missing_atom,
            juxtapose,
        }
    }
}

impl<T: Token> Default for ShunterBuilder<T> {
    fn default() -> Self {
        ShunterBuilder::new()
    }
}
