use crate::lexer::{LexerBuilder, RegexError, Token, UNICODE_WHITESPACE_REGEX};
use crate::op::{Assoc, Fixity, Op, Prec, Sort, SortId};
use crate::parser::{Parser, SortTable};
use std::collections::HashMap;
use thiserror::Error;

/// A grammar for a language. Add operators until the grammar is complete, then call `.finish()` to
/// construct a `Parser` you can use to parse.
// TODO: example
#[derive(Debug, Clone)]
pub struct Grammar {
    sort_tables: Vec<SortTable>,
    sort_ids: HashMap<String, SortId>,
    token_names: HashMap<Token, String>,
    largest_token: Token,
    lexer_builder: LexerBuilder,
    current_sort: Option<SortId>,
    current_prec: Prec,
    current_assoc: Assoc,
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
    #[error("Regex error in grammar. {0}")]
    RegexError(RegexError),
    #[error("Grammar error: you must set the `sort` before adding atoms or operators.")]
    SortNotSet,
    #[error("Grammar error: you must call `group()` before adding operators.")]
    PrecNotSet,
    #[error("Grammar error: sort '{0}' not found.")]
    NoSuchSort(Sort),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Pattern<'a> {
    pub fixity: Fixity,
    pub first_token: &'a str,
    pub followers: Vec<(&'a str, &'a str)>,
}

impl Grammar {
    /// An empty grammar, that uses Unicode's Pattern_White_Space for whitespace.
    pub fn new_with_unicode_whitespace() -> Result<Grammar, GrammarError> {
        Grammar::new(UNICODE_WHITESPACE_REGEX)
    }

    /// An empty grammar. `whitespace_regex` is the regex to use to match whitespace, in the syntax
    /// of the `regex` crate.
    pub fn new(whitespace_regex: &str) -> Result<Grammar, GrammarError> {
        let lexer_builder =
            LexerBuilder::new(whitespace_regex).map_err(GrammarError::RegexError)?;
        Ok(Grammar {
            sort_tables: vec![],
            sort_ids: HashMap::new(),
            token_names: HashMap::new(),
            largest_token: 0,
            lexer_builder,
            current_sort: None,
            current_prec: 0,
            current_assoc: Assoc::Left,
        })
    }

    /// Set the sort, used when adding ops.
    pub fn sort(&mut self, sort: &str) {
        let sort_id = self.insert_sort(sort);
        self.current_sort = Some(sort_id);
    }

    /// Add a new group of operators in the current sort. They will have higher precedence (i.e.
    /// bind _looser_) than any of the groups added so far. Any infix operators in this group will
    /// be _left associative_.
    pub fn lgroup(&mut self) {
        self.current_prec += 10;
        self.current_assoc = Assoc::Left;
    }

    /// Add a new group of operators in the current sort. They will have higher precedence (i.e.
    /// bind _looser_) than any of the groups added so far. Any infix operators in this group will
    /// be _left associative_.
    pub fn rgroup(&mut self) {
        self.current_prec += 1;
        self.current_assoc = Assoc::Right;
    }

    /// Extend the grammar with an atom: when parsing the given `sort`, if `string_pattern` is
    /// found exactly, parse it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.atom_string("value", "Null" "null")`.
    pub fn string(&mut self, name: &str, string_pattern: &str) -> Result<(), GrammarError> {
        let token = self.add_string_token(string_pattern)?;
        let sort_table = self.get_sort_table()?;
        sort_table.add_op(Op::new_atom(name, token))
    }

    /// Extend the grammar with an atom: when parsing the given `sort`, if `regex_pattern` is
    /// matched, parse it as an operator that takes no arguments.
    ///
    /// For example, a JSON grammar might have `.atom_regex("value", "Number" "[0-9]*")` (though with
    /// a better regex).
    pub fn regex(&mut self, name: &str, regex_pattern: &str) -> Result<(), GrammarError> {
        let token = self.add_regex_token(regex_pattern, name)?;
        let sort_table = self.get_sort_table()?;
        sort_table.add_op(Op::new_atom(name, token))
    }

    pub fn juxtapose(&mut self) -> Result<(), GrammarError> {
        let (prec, assoc) = self.get_prec_and_assoc()?;
        let sort_table = self.get_sort_table()?;
        sort_table.add_op(Op::new_juxtapose(assoc, prec))
    }

    /// Extend the grammar with an operator. When parsing the given `sort`, if
    /// `pattern.first_token` is found exactly, parse it as an operator with the given fixity,
    /// precedence, and followers.  For details on what all of those mean, see the [module level
    /// docs](`crate`).
    ///
    /// For example, a JSON grammar might have:
    /// ```no_run
    /// # use panfix::{Grammar, Fixity, pattern};
    /// # let mut grammar = Grammar::new("").unwrap();
    /// grammar.sort("Members");
    /// grammar.lgroup();
    /// grammar.op("comma", pattern!(_ "," _));
    /// grammar.lgroup();
    /// grammar.op("colon", pattern!(_ ":" _));
    /// ```
    #[allow(clippy::too_many_arguments)]
    pub fn op(&mut self, name: &str, pattern: Pattern) -> Result<(), GrammarError> {
        let sort_id = self.get_sort_id()?;
        if pattern.fixity == Fixity::Nilfix {
            self.insert_op(sort_id, name, Assoc::Left, 0, pattern)
        } else {
            let (prec, assoc) = self.get_prec_and_assoc()?;
            self.insert_op(sort_id, name, assoc, prec, pattern)
        }
    }

    /// Ignore the builder pattern and nice abstractions that `Grammar` otherwise uses, and insert
    /// an op into the table exactly as specified.
    pub fn insert_raw_op(
        &mut self,
        sort: &str,
        name: &str,
        assoc: Assoc,
        prec: Prec,
        pattern: Pattern,
    ) -> Result<(), GrammarError> {
        let sort_id = self.insert_sort(sort);
        self.insert_op(sort_id, name, assoc, prec, pattern)
    }

    fn insert_op(
        &mut self,
        sort_id: SortId,
        name: &str,
        assoc: Assoc,
        prec: Prec,
        pattern: Pattern,
    ) -> Result<(), GrammarError> {
        let token = self.add_string_token(pattern.first_token)?;
        let mut compiled_followers = Vec::<(SortId, Token)>::new();
        for (sort, tok_patt) in pattern.followers {
            let sort_id = self.insert_sort(sort);
            let token = self.add_string_token(tok_patt)?;
            compiled_followers.push((sort_id, token));
        }
        let op = Op::new(name, pattern.fixity, assoc, prec, token, compiled_followers);
        self.sort_tables[sort_id].add_op(op)
    }

    /// Declare the grammar complete, and construct a parser from it.
    pub fn finish(mut self) -> Result<Parser, GrammarError> {
        // Build the lexer
        let lexer = self
            .lexer_builder
            .finish()
            .map_err(GrammarError::RegexError)?;
        // Extend sort_tables to have entries for every possible token
        let required_len = self.largest_token + 1;
        for sort_table in &mut self.sort_tables {
            while sort_table.token_to_prefixy_op.len() < required_len {
                sort_table.token_to_prefixy_op.push(None);
            }
            while sort_table.token_to_suffixy_op.len() < required_len {
                sort_table.token_to_suffixy_op.push(None);
            }
        }
        Ok(Parser {
            sort_tables: self.sort_tables,
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
        self.token_names.insert(token, format!("{}", string));
        self.largest_token = token;
        Ok(token)
    }

    fn add_regex_token(&mut self, regex_pattern: &str, name: &str) -> Result<Token, GrammarError> {
        let token = match self.lexer_builder.regex(regex_pattern) {
            Ok(token) => token,
            Err(err) => return Err(GrammarError::RegexError(err)),
        };
        self.token_names.insert(token, name.to_owned());
        self.largest_token = token;
        Ok(token)
    }

    fn get_prec_and_assoc(&self) -> Result<(Prec, Assoc), GrammarError> {
        if self.current_prec > 0 {
            Ok((self.current_prec, self.current_assoc))
        } else {
            Err(GrammarError::PrecNotSet)
        }
    }

    fn get_sort_table(&mut self) -> Result<&mut SortTable, GrammarError> {
        let sort_id = self.get_sort_id()?;
        Ok(&mut self.sort_tables[sort_id])
    }

    fn get_sort_id(&mut self) -> Result<SortId, GrammarError> {
        if let Some(sort_id) = self.current_sort {
            Ok(sort_id)
        } else {
            Err(GrammarError::SortNotSet)
        }
    }

    fn insert_sort(&mut self, sort: &str) -> SortId {
        if let Some(sort_id) = self.sort_ids.get(sort) {
            *sort_id
        } else {
            let sort_id = self.sort_tables.len();
            self.sort_ids.insert(sort.to_owned(), sort_id);
            self.sort_tables.push(SortTable::new(sort));
            sort_id
        }
    }
}

impl SortTable {
    fn new(sort: &str) -> SortTable {
        SortTable {
            sort: sort.to_owned(),
            token_to_prefixy_op: vec![],
            token_to_suffixy_op: vec![],
            blank: Op::new_blank(),
            juxtapose: Op::new_juxtapose(Assoc::Left, 1),
        }
    }

    fn add_op(&mut self, op: Op) -> Result<(), GrammarError> {
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        if &op.name == "$Juxtapose" {
            self.juxtapose = op;
            Ok(())
        } else if &op.name == "$Blank" {
            self.blank = op;
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
    pub fn show_table(&self) -> String {
        let mut out = String::new();
        for sort_table in &self.sort_tables {
            self.show_op(&mut out, &sort_table.sort, &sort_table.blank);
            self.show_op(&mut out, &sort_table.sort, &sort_table.juxtapose);
            for op in &sort_table.token_to_prefixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &sort_table.sort, op);
                }
            }
            for op in &sort_table.token_to_suffixy_op {
                if let Some(op) = op {
                    self.show_op(&mut out, &sort_table.sort, op);
                }
            }
        }
        out
    }

    fn show_op(&self, out: &mut String, sort: &str, op: &Op) {
        use std::fmt::Write;

        write!(out, "{:<8}", sort).unwrap();
        write!(out, "{:<16}", op.name).unwrap();
        write!(out, "{:<8}", format!("{}", op.fixity)).unwrap();
        write!(out, "{:<8}", format!("{}", op.assoc)).unwrap();
        write!(out, "{:<8}", op.prec).unwrap();
        if let Some(token) = op.first_token {
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        } else {
            write!(out, "-       ").unwrap();
        }
        for (sort_id, token) in &op.followers {
            write!(out, "{:<8}", self.sort_tables[*sort_id].sort).unwrap();
            write!(out, "{:<8}", self.token_names[&token]).unwrap();
        }
        write!(out, "\n").unwrap();
    }
}
