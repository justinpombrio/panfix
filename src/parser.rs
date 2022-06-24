use crate::lexer::Lexer;
use crate::op::Op;
use crate::parse_tree::ParseTree;
use crate::resolver::resolve;
use crate::shunter::shunt;
use crate::source::Source;
use crate::tree_visitor::Forest;
use crate::{Prec, Token};
use std::collections::HashMap;

type OpToken = Token;

/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    token_names: HashMap<Token, String>,
    prefixy_tokens: Vec<Option<(OpToken, bool)>>,
    suffixy_tokens: Vec<Option<(OpToken, bool)>>,
    prec_table: Vec<(Prec, Prec)>,
    ops: Vec<Option<Op>>,
}

impl Parser {
    /// Parse `source`. Runs in linear time.
    pub fn parse<'s, 'g>(&'g self, source: &'s Source) -> Result<ParseTree<'s, 'g>, ()> {
        use std::iter::FromIterator;

        let lexeme_stream = self.lexer.lex(source.source());
        let resolved_stream = resolve(&self.prefixy_tokens, &self.suffixy_tokens, lexeme_stream);
        let shunted_stream = shunt(&self.prec_table, resolved_stream);
        let forest = Forest::from_iter(shunted_stream);
        Ok(ParseTree::new(source, forest))
    }
}
