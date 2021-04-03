use super::op::{Assoc, Fixity, Op, OpName, Prec, NT};
use crate::lexing::Token;
use std::collections::HashMap;
use thiserror::Error;

// TODO: move
#[derive(Debug, Clone)]
pub struct Grammar<T: Token, N: OpName> {
    pub(super) language_name: String,
    pub(super) starting_nonterminal: NT,
    pub(super) subgrammars: Vec<Subgrammar<T, N>>,
}

// TODO: move
#[derive(Debug, Clone)]
pub struct Subgrammar<T: Token, N: OpName> {
    pub(super) name: String,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub(super) token_to_prefixy_op: Vec<Option<Op<T, N>>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub(super) token_to_suffixy_op: Vec<Option<Op<T, N>>>,
    pub(super) missing_atom: Op<T, N>,
    pub(super) juxtapose: Op<T, N>,
}

#[derive(Debug, Clone)]
pub struct GrammarBuilder<T: Token, N: OpName> {
    language_name: String,
    nonterminals: HashMap<String, NT>,
    subgrammars: Vec<Subgrammar<T, N>>,
    starting_nonterminal: Option<NT>,
    current_nonterminal: Option<NT>,
    current_assoc: Option<Assoc>,
    current_prec: Prec,
}

#[derive(Debug, Clone, Error)]
pub enum GrammarBuilderError<T: Token, N: OpName> {
    #[error("The operator {0:?} appeared outside of any subgrammar declaration. But every call to `op()` must be preceded by a call to `subgrammar()`.")]
    OpOutsideSubgrammar(N),
    #[error("The operator {0:?} requires an associativity, but it was declared outside a group. Every call to `op()` with a fixity that is not Nilfix must be preceded by a call to `assoc_l()` or `assoc_r()`, to declare whether it is left-associative or right-associative.")]
    OpRequiresAssoc(N),
    #[error("The operator {0:?} is Nilfix, so it does not require an associativity. For clarity, atoms and Nilfix ops must all be declared before any calls to `assoc_l()` or `assoc_r()`.")]
    OpForbidsAssoc(N),
    #[error("In subgrammar {0}, the token {1:?} was used to start two operators: both {2:?} and {3:?}. To avoid ambiguitiy, all operators must start with unique tokens.")]
    DuplicateOp(String, T, N, N),
    #[error("The `JUXTAPOSE` and `MISSING_ATOM` operator names are reserved by the parser, and cannot be used for user-declared operators. However, you may declare the precedence of juxtaposition with the `juxtapose()` method.")]
    ReservedOpName,
    #[error(
        "Every grammar must have at least one subgrammar, started with the `subgrammar()` method."
    )]
    NoSubgrammars,
}

impl<T: Token, N: OpName> GrammarBuilder<T, N> {
    pub fn new(language_name: &str) -> GrammarBuilder<T, N> {
        GrammarBuilder {
            language_name: language_name.to_owned(),
            nonterminals: HashMap::new(),
            subgrammars: vec![],
            starting_nonterminal: None,
            current_nonterminal: None,
            current_assoc: None,
            current_prec: 0,
        }
    }

    pub fn subgrammar(
        mut self,
        name: &str,
    ) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        let nt = self.insert_nonterminal(name);
        if self.starting_nonterminal.is_none() {
            self.starting_nonterminal = Some(nt);
        }
        self.current_nonterminal = Some(nt);
        Ok(self)
    }

    pub fn assoc_l(mut self) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Left);
        Ok(self)
    }

    pub fn assoc_r(mut self) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Right);
        Ok(self)
    }

    pub fn op(
        mut self,
        name: N,
        token: T,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        self.check_op_name(name)?;
        self.insert_op(name, token, vec![], fixity)
    }

    pub fn op_multi(
        mut self,
        name: N,
        token: T,
        followers: Vec<(&str, T)>,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        self.check_op_name(name)?;
        self.insert_op(name, token, followers, fixity)
    }

    pub fn op_juxtapose(mut self) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        let nt = match self.current_nonterminal {
            None => return Err(GrammarBuilderError::OpOutsideSubgrammar(N::JUXTAPOSE)),
            Some(nt) => nt,
        };
        let op = Op::new_juxtapose(nt, self.current_prec);
        self.subgrammars[nt].juxtapose = op;
        Ok(self)
    }

    pub fn insert_op(
        mut self,
        name: N,
        token: T,
        followers: Vec<(&str, T)>,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<T, N>, GrammarBuilderError<T, N>> {
        use Fixity::*;

        let nt = match self.current_nonterminal {
            None => return Err(GrammarBuilderError::OpOutsideSubgrammar(name)),
            Some(nt) => nt,
        };
        let prec = self.current_prec;
        let assoc = match (fixity, self.current_assoc) {
            (Nilfix, None) => Assoc::Right, // which assoc does not matter
            (_, None) => return Err(GrammarBuilderError::OpRequiresAssoc(name)),
            (Nilfix, Some(_)) => return Err(GrammarBuilderError::OpForbidsAssoc(name)),
            (_, Some(assoc)) => assoc,
        };
        let followers = followers
            .into_iter()
            .map(|(subg_name, tok)| (self.insert_nonterminal(subg_name), tok))
            .collect::<Vec<_>>();
        let op = Op::new(name, Some(token), followers, prec, assoc, fixity, nt);
        let subgrammar = &mut self.subgrammars[nt];
        let op_table = match fixity {
            Prefix | Nilfix => &mut subgrammar.token_to_prefixy_op,
            Suffix | Infix => &mut subgrammar.token_to_suffixy_op,
        };
        if let Some(other_op) = &op_table[token.as_usize()] {
            return Err(GrammarBuilderError::DuplicateOp(
                subgrammar.name.to_owned(),
                token,
                op.name,
                other_op.name,
            ));
        };
        op_table[token.as_usize()] = Some(op);
        Ok(self)
    }

    pub fn finish(self) -> Result<Grammar<T, N>, GrammarBuilderError<T, N>> {
        Ok(Grammar {
            language_name: self.language_name,
            starting_nonterminal: self
                .starting_nonterminal
                .ok_or_else(|| GrammarBuilderError::NoSubgrammars)?,
            subgrammars: self.subgrammars,
        })
    }

    fn check_op_name(&mut self, op_name: N) -> Result<(), GrammarBuilderError<T, N>> {
        if op_name == OpName::MISSING_ATOM || op_name == OpName::JUXTAPOSE {
            Err(GrammarBuilderError::ReservedOpName)
        } else {
            Ok(())
        }
    }

    fn insert_nonterminal(&mut self, name: &str) -> NT {
        match self.nonterminals.get(name) {
            None => {
                let nt = self.subgrammars.len();
                let subgrammar = Subgrammar {
                    name: name.to_owned(),
                    token_to_prefixy_op: vec![],
                    token_to_suffixy_op: vec![],
                    missing_atom: Op::new_missing_atom(nt),
                    juxtapose: Op::new_juxtapose(nt, 1),
                };
                self.subgrammars.push(subgrammar);
                self.nonterminals.insert(name.to_owned(), nt);
                nt
            }
            Some(nt) => *nt,
        }
    }
}
