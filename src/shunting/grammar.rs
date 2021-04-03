use super::op::{Assoc, Fixity, Op, OpName, Prec, Token, NT};
use std::collections::HashMap;
use std::iter;
use thiserror::Error;

#[derive(Debug, Clone)]
pub struct Grammar<N: OpName> {
    pub(super) language_name: String,
    pub(super) starting_nonterminal: NT,
    pub(super) subgrammars: Vec<Subgrammar<N>>,
}

#[derive(Debug, Clone)]
pub struct Subgrammar<N: OpName> {
    pub(super) name: String,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub(super) token_to_prefixy_op: Vec<Option<Op<N>>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub(super) token_to_suffixy_op: Vec<Option<Op<N>>>,
    pub(super) missing_atom: Op<N>,
    pub(super) juxtapose: Op<N>,
    starting_tokens: HashMap<Token, N>,
}

#[derive(Debug, Clone)]
pub struct GrammarBuilder<N: OpName> {
    language_name: String,
    token_count: usize,
    nonterminals: HashMap<String, NT>,
    subgrammars: Vec<Subgrammar<N>>,
    starting_nonterminal: Option<NT>,
    current_nonterminal: Option<NT>,
    current_assoc: Option<Assoc>,
    current_prec: Prec,
}

// TODO: Check that a follower doesn't reference a subgrammar that doesn't exist.
#[derive(Debug, Clone, Error)]
pub enum GrammarBuilderError<N: OpName> {
    #[error("The operator {0:?} appeared outside of any subgrammar declaration. But every call to `op()` must be preceded by a call to `subgrammar()`.")]
    OpOutsideSubgrammar(N),
    #[error("The operator {0:?} requires an associativity, but it was declared outside a group. Every call to `op()` with a fixity that is not Nilfix must be preceded by a call to `assoc_l()` or `assoc_r()`, to declare whether it is left-associative or right-associative.")]
    OpRequiresAssoc(N),
    #[error("The operator {0:?} is Nilfix, so it does not require an associativity. For clarity, atoms and Nilfix ops must all be declared before any calls to `assoc_l()` or `assoc_r()`.")]
    OpForbidsAssoc(N),
    #[error("In subgrammar {subgrammar}, two operators {op_1:?} and {op_2:?} start with the same token. To avoid ambiguitiy, all operators must start with unique tokens.")]
    DuplicateOp {
        subgrammar: String,
        op_1: N,
        op_2: N,
    },
    #[error("The `JUXTAPOSE` and `MISSING_ATOM` operator names are reserved by the parser, and cannot be used for user-declared operators. However, you may declare the precedence of juxtaposition with the `juxtapose()` method.")]
    ReservedOpName,
    #[error("Ambiguous grammar. If argument number {arg_index} of {op:?} contains a {conflicting_op:?}, it could either be parsed as a {conflicting_op:?}, or it could progress the parsing of {op:?}.")]
    AmbiguousFollower {
        op: N,
        arg_index: usize,
        conflicting_op: N,
    },
    #[error(
        "Every grammar must have at least one subgrammar, started with the `subgrammar()` method."
    )]
    NoSubgrammars,
}

impl<N: OpName> GrammarBuilder<N> {
    pub fn new(language_name: &str, token_count: usize) -> GrammarBuilder<N> {
        GrammarBuilder {
            language_name: language_name.to_owned(),
            token_count,
            nonterminals: HashMap::new(),
            subgrammars: vec![],
            starting_nonterminal: None,
            current_nonterminal: None,
            current_assoc: None,
            current_prec: 0,
        }
    }

    pub fn subgrammar(mut self, name: &str) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        let nt = self.insert_nonterminal(name);
        if self.starting_nonterminal.is_none() {
            self.starting_nonterminal = Some(nt);
        }
        self.current_nonterminal = Some(nt);
        Ok(self)
    }

    pub fn assoc_l(mut self) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Left);
        Ok(self)
    }

    pub fn assoc_r(mut self) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Right);
        Ok(self)
    }

    pub fn op(
        mut self,
        name: N,
        token: Token,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        self.check_op_name(name)?;
        self.insert_op(name, token, vec![], fixity)
    }

    pub fn op_multi(
        mut self,
        name: N,
        token: Token,
        followers: Vec<(&str, Token)>,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        self.check_op_name(name)?;
        self.insert_op(name, token, followers, fixity)
    }

    pub fn op_juxtapose(mut self) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
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
        token: Token,
        followers: Vec<(&str, Token)>,
        fixity: Fixity,
    ) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
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
        if let Some(other_op) = &op_table[token] {
            return Err(GrammarBuilderError::DuplicateOp {
                subgrammar: subgrammar.name.to_owned(),
                op_1: op.name,
                op_2: other_op.name,
            });
        };
        op_table[token] = Some(op);
        subgrammar.starting_tokens.insert(token, name);
        Ok(self)
    }

    pub fn finish(self) -> Result<Grammar<N>, GrammarBuilderError<N>> {
        // One last validation check: cannot have a follower (NT, tok),
        // if tok is a starting token for an op in NT.
        for subgrammar in &self.subgrammars {
            let prefixy_ops = subgrammar.token_to_prefixy_op.iter();
            let suffixy_ops = subgrammar.token_to_suffixy_op.iter();
            let all_ops = prefixy_ops
                .chain(suffixy_ops)
                .filter_map(|opt| opt.as_ref());
            for op in all_ops {
                for (i, (nt, tok)) in op.followers.iter().enumerate() {
                    if let Some(conflict) = self.subgrammars[*nt].starting_tokens.get(tok) {
                        let mut arg_index = i;
                        if op.fixity == Fixity::Infix || op.fixity == Fixity::Suffix {
                            arg_index += 1;
                        }
                        return Err(GrammarBuilderError::AmbiguousFollower {
                            op: op.name,
                            arg_index,
                            conflicting_op: *conflict,
                        });
                    }
                }
            }
        }
        Ok(Grammar {
            language_name: self.language_name,
            starting_nonterminal: self
                .starting_nonterminal
                .ok_or_else(|| GrammarBuilderError::NoSubgrammars)?,
            subgrammars: self.subgrammars,
        })
    }

    fn check_op_name(&mut self, op_name: N) -> Result<(), GrammarBuilderError<N>> {
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
                    token_to_prefixy_op: iter::repeat(None).take(self.token_count).collect(),
                    token_to_suffixy_op: iter::repeat(None).take(self.token_count).collect(),
                    missing_atom: Op::new_missing_atom(nt),
                    juxtapose: Op::new_juxtapose(nt, 1),
                    starting_tokens: HashMap::new(),
                };
                self.subgrammars.push(subgrammar);
                self.nonterminals.insert(name.to_owned(), nt);
                nt
            }
            Some(nt) => *nt,
        }
    }
}
