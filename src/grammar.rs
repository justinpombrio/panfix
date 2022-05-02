use super::op::{CompiledOp, Fixity, NonTerminal, NonTerminalId, Op};
use std::collections::HashMap;
use thiserror::Error;

// TODO:
// - Method to display the grammar as a pretty table, given a way to display a token
// - Think about what happens if a starting token is also a follower token in the same subgrammar.

#[derive(Debug, Clone)]
pub struct GrammarBuilder {
    language_name: String,
    starting_subgrammar: NonTerminal,
    subgrammars: HashMap<NonTerminal, SubgrammarBuilder>,
}

#[derive(Debug, Clone)]
pub struct SubgrammarBuilder {
    name: NonTerminal,
    ops: Vec<Op>,
    missing_atom: Op,
    juxtapose: Op,
}

#[derive(Debug, Clone)]
pub struct Grammar {
    pub(crate) language_name: String,
    pub(crate) starting_subgrammar: NonTerminalId,
    pub(crate) subgrammars: Vec<Subgrammar>,
}

#[derive(Debug, Clone)]
pub struct Subgrammar {
    pub name: NonTerminal,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub token_to_prefixy_op: Vec<Option<CompiledOp>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub token_to_suffixy_op: Vec<Option<CompiledOp>>,
    pub missing_atom: CompiledOp,
    pub juxtapose: CompiledOp,
}

#[derive(Error, Debug)]
pub enum GrammarError {
    #[error("Duplicate operators. Operators in a subgrammar must start with distinct tokens, unless one is Prefix or Nilfix and the other is Suffix or Infix. This rule was broken by the oeprators {op_1:?} and {op_2:?} in the subgrammar {subgrammar:?}.")]
    DuplicateOp {
        op_1: String,
        op_2: String,
        subgrammar: NonTerminal,
    },
}

impl SubgrammarBuilder {
    fn new(name: NonTerminal) -> SubgrammarBuilder {
        SubgrammarBuilder {
            name: name.clone(),
            ops: vec![],
            // Defaults: can be overridden
            missing_atom: Op::new_missing_atom(name.clone()),
            juxtapose: Op::new_juxtapose(name, 0),
        }
    }

    fn add_op(&mut self, op: Op) -> Result<(), GrammarError> {
        if &op.name == "$Juxtapose" {
            self.juxtapose = op;
        } else if &op.name == "$MissingAtom" {
            self.missing_atom = op;
        } else {
            self.ops.push(op);
        }
        Ok(())
    }

    fn finish(
        self,
        subgrammar_ids: &HashMap<NonTerminal, NonTerminalId>,
    ) -> Result<Subgrammar, GrammarError> {
        use Fixity::{Infix, Nilfix, Prefix, Suffix};

        let mut token_to_prefixy_op = Vec::<Option<CompiledOp>>::new();
        let mut token_to_suffixy_op = Vec::<Option<CompiledOp>>::new();

        for op in self.ops {
            let token = op.first_token.unwrap();
            let mapping = match op.fixity {
                Prefix | Nilfix => &mut token_to_prefixy_op,
                Suffix | Infix => &mut token_to_suffixy_op,
            };
            while token >= mapping.len() {
                mapping.push(None);
            }
            if let Some(existing_op) = &mapping[token] {
                return Err(GrammarError::DuplicateOp {
                    op_1: op.name,
                    op_2: existing_op.name.clone(),
                    subgrammar: self.name.clone(),
                });
            } else {
                mapping[token] = Some(op.compile(&subgrammar_ids));
            }
        }
        let missing_atom = self.missing_atom.compile(&subgrammar_ids);
        let juxtapose = self.juxtapose.compile(&subgrammar_ids);
        Ok(Subgrammar {
            name: self.name,
            token_to_prefixy_op,
            token_to_suffixy_op,
            missing_atom,
            juxtapose,
        })
    }
}

impl GrammarBuilder {
    pub fn new(
        language_name: String,
        starting_subgrammar: NonTerminal,
    ) -> Result<GrammarBuilder, GrammarError> {
        Ok(GrammarBuilder {
            language_name,
            starting_subgrammar,
            subgrammars: HashMap::new(),
        })
    }

    pub fn add_op(&mut self, op: Op) -> Result<(), GrammarError> {
        self.get_subgrammar_mut(&op.subgrammar).add_op(op)
    }

    pub fn name(&self) -> &str {
        &self.language_name
    }

    pub fn finish(self) -> Result<Grammar, GrammarError> {
        // TODO: don't panic on missing subgrammar; implicitly create it instead
        let subgrammar_ids = self
            .subgrammars
            .iter()
            .enumerate()
            .map(|(i, (_, subgrammar))| (subgrammar.name.clone(), i))
            .collect::<HashMap<NonTerminal, NonTerminalId>>();
        let mut subgrammars = Vec::<Subgrammar>::new();
        for (_, subgrammar) in self.subgrammars {
            subgrammars.push(subgrammar.finish(&subgrammar_ids)?);
        }
        Ok(Grammar {
            language_name: self.language_name,
            starting_subgrammar: subgrammar_ids[&self.starting_subgrammar],
            subgrammars,
        })
    }

    fn get_subgrammar_mut<'l>(&'l mut self, name: &str) -> &'l mut SubgrammarBuilder {
        self.subgrammars
            .entry(name.to_owned())
            .or_insert_with(|| SubgrammarBuilder::new(name.to_owned()))
    }
}

/*
impl PartialEq for Subgrammar {
    fn eq(&self, other: &Subgrammar) -> bool {
        self.name == other.name
    }
}
impl Eq for Subgrammar {}
impl Hash for Subgrammar {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        self.name.hash(hasher);
    }
}
*/
