use super::op::{Assoc, Fixity, Follower, Op, Prec, NT};
use crate::lexing::Token;
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct Grammar<T: Token> {
    pub(super) language_name: String,
    pub(super) starting_nonterminal: NT,
    pub(super) subgrammars: Vec<Subgrammar<T>>,
}

#[derive(Debug, Clone)]
pub struct Subgrammar<T: Token> {
    pub(super) name: String,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub(super) token_to_prefixy_op: Vec<Option<Op<T>>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub(super) token_to_suffixy_op: Vec<Option<Op<T>>>,
    pub(super) missing_atom: Op<T>,
    pub(super) juxtapose: Op<T>,
}

#[derive(Debug, Clone)]
pub struct OpSpec<T: Token> {
    pub nonterminal: String,
    pub name: String,
    pub fixity: Fixity,
    pub assoc: Assoc,
    pub first_token: Option<T>,
    pub followers: Vec<(String, T)>,
    pub prec: Prec,
}

impl<T: Token> OpSpec<T> {
    pub fn juxtapose(nonterminal: &str, prec: Prec) -> OpSpec<T> {
        OpSpec {
            nonterminal: nonterminal.to_owned(),
            name: "$Juxtapose".to_owned(),
            fixity: Fixity::Infix,
            assoc: Assoc::Right,
            first_token: None,
            followers: vec![],
            prec,
        }
    }

    fn validate(&self) {
        if self.name == "$Juxtapose" {
            assert_eq!(self.fixity, Fixity::Infix);
            assert_eq!(self.first_token, None);
            assert!(self.followers.is_empty());
        } else {
            assert!(!self.name.starts_with("$"));
            assert!(self.first_token.is_some());
            if self.fixity == Fixity::Nilfix {
                assert_eq!(self.prec, 0);
            }
        }
    }
}

impl<T: Token> Grammar<T> {
    pub fn new(
        language_name: String,
        starting_nonterminal: String,
        ops: Vec<OpSpec<T>>,
    ) -> Grammar<T> {
        let mut grammar_maker = GrammarMaker::new(language_name, starting_nonterminal);
        for op in ops {
            op.validate();
            // Convert nonterminal names into subgrammar indices
            let nt = grammar_maker.add_subgrammar(&op.nonterminal);
            let followers = op
                .followers
                .into_iter()
                .map(|(nt, token)| Follower {
                    subgrammar_index: grammar_maker.add_subgrammar(&nt),
                    token,
                })
                .collect();
            // Make the real op and add it to the subgrammar
            let op = Op::new(
                op.name,
                op.first_token,
                followers,
                op.prec,
                op.assoc,
                op.fixity,
                nt,
            );
            let subgrammar = &mut grammar_maker.subgrammars[nt as usize];
            if &op.name == "$Juxtapose" {
                subgrammar.juxtapose = Some(op);
            } else {
                subgrammar.ops.push(op);
            }
        }
        grammar_maker.into_grammar()
    }

    // TODO: Method to display the grammar as a pretty table, given a way to display a token
}

struct GrammarMaker<T: Token> {
    language_name: String,
    starting_nonterminal: String,
    subgrammar_name_to_index: HashMap<String, usize>,
    subgrammars: Vec<SubgrammarMaker<T>>,
}

impl<T: Token> GrammarMaker<T> {
    fn new(language_name: String, starting_nonterminal: String) -> GrammarMaker<T> {
        GrammarMaker {
            language_name,
            starting_nonterminal,
            subgrammar_name_to_index: HashMap::new(),
            subgrammars: vec![],
        }
    }

    fn add_subgrammar(&mut self, name: &str) -> NT {
        if let Some(i) = self.subgrammar_name_to_index.get(name) {
            *i as NT
        } else {
            let nt = self.subgrammars.len();
            let subgrammar = SubgrammarMaker::new(name, nt as NT);
            self.subgrammars.push(subgrammar);
            self.subgrammar_name_to_index.insert(name.to_owned(), nt);
            nt as NT
        }
    }

    fn into_grammar(self) -> Grammar<T> {
        let starting_nonterminal = *self
            .subgrammar_name_to_index
            .get(&self.starting_nonterminal)
            .unwrap_or_else(|| panic!("No such nonterminal: {}", self.starting_nonterminal))
            as NT;
        let mut largest_token_index: usize = 0;
        for subgrammar in &self.subgrammars {
            for op in &subgrammar.ops {
                if let Some(token) = op.first_token {
                    largest_token_index = largest_token_index.max(token.as_usize());
                }
                for follower in &op.followers {
                    largest_token_index = largest_token_index.max(follower.token.as_usize());
                }
            }
        }
        let subgrammars = self
            .subgrammars
            .into_iter()
            .map(|g| g.into_subgrammar(largest_token_index))
            .collect::<Vec<_>>();
        for subgrammar in &subgrammars {
            // Can't have a token be both a follower and a start token, for the same nonterminal
            let all_ops = subgrammar
                .token_to_prefixy_op
                .iter()
                .chain(subgrammar.token_to_suffixy_op.iter())
                .filter_map(|op| op.as_ref());
            for op in all_ops {
                for follower in &op.followers {
                    let subgrammar = &subgrammars[follower.subgrammar_index as usize];
                    if let Some(op2) = &subgrammar.token_to_suffixy_op[follower.token.as_usize()] {
                        // TODO: Find a way to name the token?
                        panic!("The first token of operator {} can't also be used as a follower token in operator {}, in the same nonterminal {}. Try making another nonterminal to separate the two uses of the token.", op2.name, op.name, subgrammar.name);
                    }
                }
            }
        }
        Grammar {
            language_name: self.language_name,
            starting_nonterminal,
            subgrammars,
        }
    }
}

pub struct SubgrammarMaker<T: Token> {
    name: String,
    nonterminal: NT,
    ops: Vec<Op<T>>,
    juxtapose: Option<Op<T>>,
}

impl<T: Token> SubgrammarMaker<T> {
    fn new(name: &str, nonterminal: NT) -> SubgrammarMaker<T> {
        SubgrammarMaker {
            name: name.to_owned(),
            nonterminal,
            ops: vec![],
            juxtapose: None,
        }
    }

    fn into_subgrammar(self, largest_token_index: usize) -> Subgrammar<T> {
        use Fixity::*;

        // Autocreate the error-handling ops
        let nt = self.nonterminal;
        let juxtapose = self.juxtapose.unwrap_or_else(|| {
            Op::new(
                "$Juxtapose".to_owned(),
                None,
                vec![],
                1,
                Assoc::Right,
                Fixity::Infix,
                nt,
            )
        });
        let missing_atom = Op::new(
            "$MissingAtom".to_owned(),
            None,
            vec![],
            0,
            Assoc::Left,
            Fixity::Nilfix,
            nt,
        );

        // Initialize maps from token.as_usize() to the op that starts with that token.
        // (Two such maps, depending on whether the op has a left prec or not.)
        let mut token_to_prefixy_op = vec![None; largest_token_index + 1];
        let mut token_to_suffixy_op = vec![None; largest_token_index + 1];
        for op in self.ops {
            let token = op.first_token.unwrap();
            let index = token.as_usize();

            // Can't have two ops that start with the same token, unless exactly one of them has a
            // left_prec.
            match op.fixity {
                Prefix | Nilfix => {
                    assert!(
                        token_to_prefixy_op[index].is_none(),
                        "Duplicate first op token: {:?} in {}",
                        token,
                        self.name,
                    );
                    token_to_prefixy_op[index] = Some(op);
                }
                Suffix | Infix => {
                    assert!(
                        token_to_suffixy_op[index].is_none(),
                        "Duplicate first op token: {:?} in {}",
                        token,
                        self.name,
                    );
                    token_to_suffixy_op[index] = Some(op);
                }
            }
        }

        Subgrammar {
            name: self.name,
            token_to_prefixy_op,
            token_to_suffixy_op,
            juxtapose,
            missing_atom,
        }
    }
}
