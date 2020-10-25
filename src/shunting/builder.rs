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
        Self::new("$Juxtapose".to_owned(), vec![], Fixity::Infix)
    }

    pub fn nilfix(name: String, tokens: Vec<T>) -> Self {
        Self::new(name, tokens, Fixity::Nilfix)
    }

    pub fn prefix(name: String, tokens: Vec<T>) -> Self {
        Self::new(name, tokens, Fixity::Prefix)
    }

    pub fn suffix(name: String, tokens: Vec<T>) -> Self {
        Self::new(name, tokens, Fixity::Suffix)
    }

    pub fn infix(name: String, tokens: Vec<T>) -> Self {
        Self::new(name, tokens, Fixity::Infix)
    }

    fn new(name: String, tokens: Vec<T>, fixity: Fixity) -> Self {
        OpSpec {
            name,
            tokens,
            fixity,
        }
    }
}

impl<T: Token> ShunterBuilder<T> {
    pub fn new() -> ShunterBuilder<T> {
        ShunterBuilder {
            ops: vec![],
            prec: 0,
        }
    }

    pub fn op(self, op: OpSpec<T>) -> Self {
        use Fixity::*;
        match op.fixity {
            Nilfix => self.add_nilfix_op(op),
            Prefix | Suffix => self.add_op(Assoc::Left, op),
            Infix => panic!("Must provide associativity for infix operator {}", op.name),
        }
    }

    pub fn op_l(self, op: OpSpec<T>) -> Self {
        use Fixity::*;
        match op.fixity {
            Infix => self.add_op(Assoc::Left, op),
            _ => panic!("The operator {} is at its own precedence level, so it does not need an associativity", op.name),
        }
    }

    pub fn op_r(self, op: OpSpec<T>) -> Self {
        use Fixity::*;
        match op.fixity {
            Infix => self.add_op(Assoc::Right, op),
            _ => panic!("The operator {} is at its own precedence level, so it does not need an associativity", op.name),
        }
    }

    pub fn ops_l(self, ops: Vec<OpSpec<T>>) -> Self {
        self.add_ops(Assoc::Left, ops)
    }

    pub fn ops_r(self, ops: Vec<OpSpec<T>>) -> Self {
        self.add_ops(Assoc::Right, ops)
    }

    fn add_op(mut self, assoc: Assoc, op: OpSpec<T>) -> Self {
        self.prec += 1;
        self.ops
            .push(Op::new(op.name, op.tokens, self.prec, assoc, op.fixity));
        self
    }

    fn add_nilfix_op(mut self, op: OpSpec<T>) -> Self {
        assert_eq!(self.prec, 0, "Since they have the tightest precedence, nilfix operators like {} must be listed first", op.name);
        self.ops
            .push(Op::new(op.name, op.tokens, 0, Assoc::Left, Fixity::Nilfix));
        self
    }

    fn add_ops(mut self, assoc: Assoc, ops: Vec<OpSpec<T>>) -> Self {
        self.prec += 1;
        for op in ops {
            assert!(
                op.fixity != Fixity::Nilfix,
                "The operator {} does not need an associativity",
                op.name
            );
            self.ops
                .push(Op::new(op.name, op.tokens, self.prec, assoc, op.fixity));
        }
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
            Assoc::Left,
            Fixity::Nilfix,
        );
        let juxtapose = juxtapose.unwrap_or_else(|| {
            Op::new(
                "$Juxtapose".to_owned(),
                vec![],
                1,
                Assoc::Right,
                Fixity::Infix,
            )
        });
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
