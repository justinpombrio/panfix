use crate::lexer::RegexError;
use crate::lexer::{Lexer, LexerBuilder, Token};
use crate::op::{Assoc, Fixity, NonTerminal, NonTerminalId, Op, Prec};
use std::collections::HashMap;
use thiserror::Error;

/// Used to construct a grammar, that you can then use to parse.
#[derive(Debug, Clone)]
pub struct GrammarBuilder {
    starting_subgrammar: NonTerminalId,
    subgrammars: Vec<Subgrammar>,
    subgrammar_indices: HashMap<String, NonTerminalId>,
    token_names: HashMap<Token, String>,
    lexer_builder: LexerBuilder,
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub(crate) starting_subgrammar: NonTerminalId,
    pub(crate) subgrammars: Vec<Subgrammar>,
    pub(crate) lexer: Lexer,
    pub(crate) token_names: HashMap<Token, String>,
}

#[derive(Debug, Clone)]
pub struct Subgrammar {
    pub name: NonTerminal,
    pub id: NonTerminalId,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub token_to_prefixy_op: Vec<Option<Op>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub token_to_suffixy_op: Vec<Option<Op>>,
    pub missing_atom: Op,
    pub juxtapose: Op,
}

#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Duplicate operators. Operators in a subgrammar must start with distinct tokens, unless one is Prefix or Nilfix and the other is Suffix or Infix. This rule was broken by the oeprators {op_1:?} and {op_2:?} in the subgrammar {subgrammar:?}.")]
    DuplicateOp {
        op_1: String,
        op_2: String,
        subgrammar: NonTerminal,
    },
    #[error("{0}")]
    RegexError(RegexError),
}

impl GrammarBuilder {
    pub fn new(
        starting_subgrammar: &str,
        whitespace_regex: &str,
    ) -> Result<GrammarBuilder, GrammarError> {
        let lexer_builder =
            LexerBuilder::new(whitespace_regex).map_err(GrammarError::RegexError)?;
        let mut grammar_builder = GrammarBuilder {
            starting_subgrammar: 0, // temporary
            subgrammars: vec![],
            subgrammar_indices: HashMap::new(),
            token_names: HashMap::new(),
            lexer_builder,
        };
        let id = grammar_builder.insert_subgrammar(starting_subgrammar);
        grammar_builder.starting_subgrammar = id;
        Ok(grammar_builder)
    }

    pub fn add_atom(
        &mut self,
        subgrammar: &str,
        name: &str,
        string_pattern: &str,
    ) -> Result<(), GrammarError> {
        let token = self.add_string_token(string_pattern)?;
        let op = Op::new_atom(name, token);
        let id = self.insert_subgrammar(subgrammar);
        self.subgrammars[id].add_op(op)
    }

    pub fn add_atom_regex(
        &mut self,
        subgrammar: &str,
        name: &str,
        regex_pattern: &str,
    ) -> Result<(), GrammarError> {
        let token = self.add_regex_token(regex_pattern, name)?;
        let op = Op::new_atom(name, token);
        let id = self.insert_subgrammar(subgrammar);
        self.subgrammars[id].add_op(op)
    }

    #[allow(clippy::too_many_arguments)]
    pub fn add_op(
        &mut self,
        subgrammar: &str,
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        first_token: &str,
        followers: Vec<(&str, &str)>,
    ) -> Result<(), GrammarError> {
        let token = self.add_string_token(first_token)?;
        let mut compiled_followers = Vec::<(NonTerminalId, Token)>::new();
        for (subgrammar, pattern) in followers {
            let id = self.insert_subgrammar(subgrammar);
            let token = self.add_string_token(pattern)?;
            compiled_followers.push((id, token));
        }
        let op = Op::new(name, fixity, assoc, prec, token, compiled_followers);
        let id = self.insert_subgrammar(subgrammar);
        self.subgrammars[id].add_op(op)
    }

    pub fn finish(self) -> Result<Grammar, GrammarError> {
        let lexer = self
            .lexer_builder
            .finish()
            .map_err(GrammarError::RegexError)?;
        Ok(Grammar {
            starting_subgrammar: self.starting_subgrammar,
            subgrammars: self.subgrammars,
            lexer,
            token_names: self.token_names,
        })
    }

    fn add_string_token(&mut self, string: &str) -> Result<Token, GrammarError> {
        let token = match self.lexer_builder.string(string) {
            Ok(token) => token,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.token_names.insert(token, format!("'{}'", string));
        Ok(token)
    }

    fn add_regex_token(&mut self, regex_pattern: &str, name: &str) -> Result<Token, GrammarError> {
        let token = match self.lexer_builder.regex(regex_pattern) {
            Ok(token) => token,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.token_names.insert(token, name.to_owned());
        Ok(token)
    }

    fn insert_subgrammar(&mut self, name: &str) -> NonTerminalId {
        if let Some(id) = self.subgrammar_indices.get(name) {
            *id
        } else {
            let id = self.subgrammars.len();
            self.subgrammar_indices.insert(name.to_owned(), id);
            self.subgrammars.push(Subgrammar::new(name, id));
            id
        }
    }
}

impl Subgrammar {
    fn new(name: &str, id: NonTerminalId) -> Subgrammar {
        Subgrammar {
            name: name.to_owned(),
            id,
            token_to_prefixy_op: vec![],
            token_to_suffixy_op: vec![],
            missing_atom: Op::new_missing_atom(),
            juxtapose: Op::new_juxtapose(0),
        }
    }

    fn add_op(&mut self, op: Op) -> Result<(), GrammarError> {
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        if &op.name == "$Juxtapose" {
            self.juxtapose = op;
            Ok(())
        } else if &op.name == "$MissingAtom" {
            self.missing_atom = op;
            Ok(())
        } else {
            let token = op.first_token.unwrap();
            let mapping = match op.fixity {
                Prefix | Nilfix => &mut self.token_to_prefixy_op,
                Suffix | Infix => &mut self.token_to_suffixy_op,
            };
            while token >= mapping.len() {
                mapping.push(None);
            }
            if let Some(existing_op) = &mapping[token] {
                Err(GrammarError::DuplicateOp {
                    op_1: op.name,
                    op_2: existing_op.name.clone(),
                    subgrammar: self.name.clone(),
                })
            } else {
                mapping[token] = Some(op);
                Ok(())
            }
        }
    }
}

impl Grammar {
    pub fn to_table(&self) -> String {
        let mut out = String::new();
        for subgrammar in &self.subgrammars {
            self.show_op(&mut out, &subgrammar.name, &subgrammar.missing_atom);
            self.show_op(&mut out, &subgrammar.name, &subgrammar.juxtapose);
            for op in &subgrammar.token_to_prefixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &subgrammar.name, op);
                }
            }
            for op in &subgrammar.token_to_suffixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &subgrammar.name, op);
                }
            }
        }
        out
    }

    fn show_op(&self, out: &mut String, subgrammar: &str, op: &Op) {
        use std::fmt::Write;

        write!(out, "{:<8}", subgrammar).unwrap();
        write!(out, "{:<8}", op.name).unwrap();
        write!(out, "{:<8}", op.fixity).unwrap();
        write!(out, "{:<8}", op.assoc).unwrap();
        write!(out, "{:<8}", op.prec).unwrap();
        if let Some(token) = op.first_token {
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        } else {
            write!(out, "-       ").unwrap();
        }
        for (id, token) in &op.followers {
            write!(out, "{:<8}", self.subgrammars[*id].name).unwrap();
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        }
    }
}
