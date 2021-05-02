use super::op::Op;
use super::parser::Parser;
use super::tokenset::TokenSet;
use crate::lexing::LexerBuilderError;
use crate::shunting::{Fixity, GrammarBuilder, GrammarBuilderError, OpName, Token};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParserBuilderError<N: OpName + 'static> {
    #[error("{0}")]
    Shunter(#[from] GrammarBuilderError<N>),
    #[error("{0}")]
    Lexer(#[from] LexerBuilderError),
}

pub struct ParserBuilder<N: OpName> {
    shunter: GrammarBuilder<N>,
    tokenset: TokenSet,
}

impl<N: OpName> ParserBuilder<N> {
    pub fn new(language_name: &str) -> ParserBuilder<N> {
        ParserBuilder {
            shunter: GrammarBuilder::new(language_name),
            tokenset: TokenSet::new(),
        }
    }

    pub fn subgrammar(mut self, name: &str) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.shunter = self.shunter.subgrammar(name)?;
        Ok(self)
    }

    pub fn assoc_l(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.shunter = self.shunter.assoc_l()?;
        Ok(self)
    }

    pub fn assoc_r(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.shunter = self.shunter.assoc_r()?;
        Ok(self)
    }

    pub fn whitespace(
        mut self,
        whitespace_regex: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        // TODO
        assert!(self.shunter.current_nonterminal.is_none());
        self.tokenset.insert_whitespace(&whitespace_regex);
        Ok(self)
    }

    pub fn regex_literal(
        mut self,
        name: N,
        regex: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        // TODO
        assert!(self.shunter.current_nonterminal.is_none());
        let token = self.tokenset.regex(regex);
        self.shunter = self.shunter.op(name, token, Fixity::Nilfix)?;
        Ok(self)
    }

    pub fn string_literal(
        mut self,
        name: N,
        string: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        // TODO
        assert!(self.shunter.current_nonterminal.is_none());
        let token = self.tokenset.string(string);
        self.shunter = self.shunter.op(name, token, Fixity::Nilfix)?;
        Ok(self)
    }

    pub fn op(mut self, op: Op<N>) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        let mut followers: Vec<(&str, Token)> = vec![];
        for (nt, lit) in &op.followers {
            let token = self.tokenset.string(lit);
            followers.push((nt, token));
        }
        let token = self.tokenset.string(&op.first_token);
        self.shunter = self
            .shunter
            .op_multi(op.name, token, followers, op.fixity)?;
        Ok(self)
    }

    pub fn op_juxtapose(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.shunter = self.shunter.op_juxtapose()?;
        Ok(self)
    }

    pub fn finish(self) -> Result<Parser<N>, ParserBuilderError<N>> {
        let shunter = self.shunter.finish()?;
        let lexer = self.tokenset.into_lexer()?;
        Ok(Parser { shunter, lexer })
    }
}
