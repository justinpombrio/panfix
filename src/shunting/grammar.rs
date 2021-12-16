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
    nonterminals: HashMap<String, NT>,
    subgrammars: Vec<Subgrammar<N>>,
    max_token: Token,
}

#[derive(Debug, Clone, Error)]
pub enum GrammarBuilderError<N: OpName> {
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
}

impl<N: OpName> GrammarBuilder<N> {
    pub fn new(language_name: &str) -> GrammarBuilder<N> {
        GrammarBuilder {
            language_name: language_name.to_owned(),
            nonterminals: HashMap::new(),
            subgrammars: vec![],
            max_token: 0,
        }
    }

    pub fn op(
        mut self,
        subgrammar: &str,
        name: N,
        prec: Prec,
        assoc: Assoc,
        fixity: Fixity,
        token: Token,
        followers: Vec<(&str, Token)>,
    ) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        use Fixity::*;

        // Error if the op uses a reserved name.
        if name == N::MISSING_ATOM || name == N::JUXTAPOSE {
            return Err(GrammarBuilderError::ReservedOpName);
        }

        // Construct the op, adding any subgrammars it references that don't exist yet.
        self.insert_token(token);
        let nt = self.insert_nonterminal(subgrammar);
        let followers = followers
            .into_iter()
            .map(|(subg_name, tok)| (self.insert_nonterminal(subg_name), self.insert_token(tok)))
            .collect::<Vec<_>>();
        let op = Op::new(name, Some(token), followers, prec, assoc, fixity, nt);
        let subgrammar = &mut self.subgrammars[nt];

        // Insert the op, adding it to various lookup tables.
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

    pub fn op_juxtapose(
        mut self,
        subgrammar: &str,
        prec: Prec,
    ) -> Result<GrammarBuilder<N>, GrammarBuilderError<N>> {
        let nt = self.insert_nonterminal(subgrammar);
        let op = Op::new_juxtapose(nt, prec);
        self.subgrammars[nt].juxtapose = op;
        Ok(self)
    }

    pub fn finish(
        mut self,
        starting_subgrammar: &str,
    ) -> Result<Grammar<N>, GrammarBuilderError<N>> {
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
        let starting_nonterminal = self.insert_nonterminal(starting_subgrammar);
        Ok(Grammar {
            language_name: self.language_name,
            starting_nonterminal,
            subgrammars: self.subgrammars,
        })
    }

    fn insert_nonterminal(&mut self, name: &str) -> NT {
        match self.nonterminals.get(name) {
            None => {
                let nt = self.subgrammars.len();
                let subgrammar = Subgrammar {
                    name: name.to_owned(),
                    token_to_prefixy_op: iter::repeat(None).take(self.max_token).collect(),
                    token_to_suffixy_op: iter::repeat(None).take(self.max_token).collect(),
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

    fn insert_token(&mut self, token: Token) -> Token {
        if token >= self.max_token {
            for subgrammar in &mut self.subgrammars {
                subgrammar.token_to_prefixy_op.resize(token + 1, None);
                subgrammar.token_to_suffixy_op.resize(token + 1, None);
            }
            self.max_token = token + 1;
        }
        token
    }
}
