use crate::lexer::RegexError;
use crate::lexer::{Lexer, LexerBuilder, Token};
use crate::op::{Assoc, Fixity, Op, Prec, Sort, SortId};
use std::collections::HashMap;
use thiserror::Error;

/// Used to construct a grammar, that you can then use to parse.
// TODO: example
#[derive(Debug, Clone)]
pub struct GrammarBuilder {
    subgrammars: Vec<Subgrammar>,
    sort_ids: HashMap<String, SortId>,
    token_names: HashMap<Token, String>,
    lexer_builder: LexerBuilder,
}

/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Grammar {
    pub(crate) subgrammars: Vec<Subgrammar>,
    pub(crate) sort_ids: HashMap<String, SortId>,
    pub(crate) lexer: Lexer,
    pub(crate) token_names: HashMap<Token, String>,
}

#[derive(Debug, Clone)]
pub(crate) struct Subgrammar {
    sort: Sort,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub(crate) token_to_prefixy_op: Vec<Option<Op>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub(crate) token_to_suffixy_op: Vec<Option<Op>>,
    pub(crate) missing_atom: Op,
    pub(crate) juxtapose: Op,
}

/// An error while constructing a grammar.
#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Duplicate operators. Operators in a sort must start with distinct tokens, unless one is Prefix or Nilfix and the other is Suffix or Infix. This rule was broken by the oeprators {op_1:?} and {op_2:?} in the sort {sort:?}.")]
    DuplicateOp {
        op_1: String,
        op_2: String,
        sort: Sort,
    },
    #[error("{0}")]
    RegexError(RegexError),
}

impl GrammarBuilder {
    /// Start constructing a grammar. `whitespace_regex` is the regex to use to match whitespace,
    /// in the syntax of the `regex` crate.
    pub fn new(whitespace_regex: &str) -> Result<GrammarBuilder, GrammarError> {
        let lexer_builder =
            LexerBuilder::new(whitespace_regex).map_err(GrammarError::RegexError)?;
        Ok(GrammarBuilder {
            subgrammars: vec![],
            sort_ids: HashMap::new(),
            token_names: HashMap::new(),
            lexer_builder,
        })
    }

    /// Extend the grammar: when parsing the given `sort`, if `string_pattern` is found exactly,
    /// parse it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.add_atom("value", "Null" "null")`.
    pub fn add_atom(
        &mut self,
        sort: &str,
        name: &str,
        string_pattern: &str,
    ) -> Result<(), GrammarError> {
        let token = self.add_string_token(string_pattern)?;
        let op = Op::new_atom(name, token);
        let sort_id = self.insert_sort(sort);
        self.subgrammars[sort_id].add_op(op)
    }

    /// Extend the grammar: when parsing the given `sort`, if `regex_pattern` is matched, parse it
    /// as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.add_atom("value", "Number" "[0-9]*")` (though with
    /// a better regex).
    pub fn add_atom_regex(
        &mut self,
        sort: &str,
        name: &str,
        regex_pattern: &str,
    ) -> Result<(), GrammarError> {
        let token = self.add_regex_token(regex_pattern, name)?;
        let op = Op::new_atom(name, token);
        let sort_id = self.insert_sort(sort);
        self.subgrammars[sort_id].add_op(op)
    }

    /// Extend the grammar. When parsing the given `sort`, if `first_token` is found exactly, parse
    /// it as an operator with the given fixity, associativity, precedence, and followers. For
    /// details on what all of those mean, see the [module level docs](`crate`).
    ///
    /// For example, a JSON grammar might have:
    /// ```
    /// .add_op("members", "Comma", Fixity::Infix, Assoc::Left, 20, ",", vec![])
    /// .add_op("members", "Colon", Fixity::Infix, Assoc::Left, 10, ":", vec![])
    /// ```
    #[allow(clippy::too_many_arguments)]
    pub fn add_op(
        &mut self,
        sort: &str,
        name: &str,
        fixity: Fixity,
        assoc: Assoc,
        prec: Prec,
        first_token: &str,
        followers: Vec<(&str, &str)>,
    ) -> Result<(), GrammarError> {
        let token = self.add_string_token(first_token)?;
        let mut compiled_followers = Vec::<(SortId, Token)>::new();
        for (sort, pattern) in followers {
            let sort_id = self.insert_sort(sort);
            let token = self.add_string_token(pattern)?;
            compiled_followers.push((sort_id, token));
        }
        let op = Op::new(name, fixity, assoc, prec, token, compiled_followers);
        let sort_id = self.insert_sort(sort);
        self.subgrammars[sort_id].add_op(op)
    }

    /// Finish constructing the grammar.
    pub fn finish(self) -> Result<Grammar, GrammarError> {
        let lexer = self
            .lexer_builder
            .finish()
            .map_err(GrammarError::RegexError)?;
        Ok(Grammar {
            subgrammars: self.subgrammars,
            sort_ids: self.sort_ids,
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

    fn insert_sort(&mut self, sort: &str) -> SortId {
        if let Some(sort_id) = self.sort_ids.get(sort) {
            *sort_id
        } else {
            let sort_id = self.subgrammars.len();
            self.sort_ids.insert(sort.to_owned(), sort_id);
            self.subgrammars.push(Subgrammar::new(sort));
            sort_id
        }
    }
}

impl Subgrammar {
    fn new(sort: &str) -> Subgrammar {
        Subgrammar {
            sort: sort.to_owned(),
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
                    sort: self.sort.clone(),
                })
            } else {
                mapping[token] = Some(op);
                Ok(())
            }
        }
    }
}

impl Grammar {
    /// Display the grammar in a table.
    // TODO: This is the inverse of from_table.
    pub fn to_table(&self) -> String {
        let mut out = String::new();
        for subgrammar in &self.subgrammars {
            self.show_op(&mut out, &subgrammar.sort, &subgrammar.missing_atom);
            self.show_op(&mut out, &subgrammar.sort, &subgrammar.juxtapose);
            for op in &subgrammar.token_to_prefixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &subgrammar.sort, op);
                }
            }
            for op in &subgrammar.token_to_suffixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &subgrammar.sort, op);
                }
            }
        }
        out
    }

    fn show_op(&self, out: &mut String, sort: &str, op: &Op) {
        use std::fmt::Write;

        write!(out, "{:<8}", sort).unwrap();
        write!(out, "{:<8}", op.name).unwrap();
        write!(out, "{:<8}", op.fixity).unwrap();
        write!(out, "{:<8}", op.assoc).unwrap();
        write!(out, "{:<8}", op.prec).unwrap();
        if let Some(token) = op.first_token {
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        } else {
            write!(out, "-       ").unwrap();
        }
        for (sort_id, token) in &op.followers {
            write!(out, "{:<8}", self.subgrammars[*sort_id].sort).unwrap();
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        }
    }
}
