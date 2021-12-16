use super::op::Op;
use super::parser::Parser;
use super::tokenset::TokenSet;
use crate::lexing::LexerBuilderError;
use crate::shunting::{Assoc, Fixity, GrammarBuilder, GrammarBuilderError, OpName, Prec, Token};
use thiserror::Error;

#[derive(Debug, Clone, Error)]
pub enum ParserBuilderError<N: OpName + 'static> {
    #[error("{0}")]
    Shunter(#[from] GrammarBuilderError<N>),
    #[error("{0}")]
    Lexer(#[from] LexerBuilderError),
    #[error("Please specify whitespace before any calls to `.subgrammar()`, as it is not tied to any particular subgrammar.")]
    WhitespaceInsideSubgrammar,
    #[error("Operators are part of a subgrammar, so you must call `.subgrammar(name)` before any calls to `.op()`.")]
    OpOutsideSubgrammar,
    #[error("Regex and string literals are part of a subgrammar, so you must call `.subgrammar(name)` before any calls to `.regex()` or `.string()`.")]
    LiteralOutsideSubgrammar,
    #[error("Regex and string literals do not have an associativity, so they must be declared after a call to `.subgrammar()`, but before a call to `assoc_l` or `assoc_r`.")]
    LiteralInsideAssoc,
    #[error("The operator {0} requires an associativity, but it was declared outside a group. Every call to `op()` with a fixity that is not Nilfix must be preceded by a call to `assoc_l()` or `assoc_r()`, to declare whether it is left-associative or right-associative.")]
    OpRequiresAssoc(N),
    #[error("The operator {0} is Nilfix, so it does not require an associativity. For clarity, and Nilfix ops must all be declared before any calls to `assoc_l()` or `assoc_r()`.")]
    OpForbidsAssoc(N),
    #[error(
        "Every grammar must have at least one subgrammar, started with the `subgrammar()` method."
    )]
    NoSubgrammars,
}

pub struct ParserBuilder<N: OpName> {
    shunter: GrammarBuilder<N>,
    tokenset: TokenSet,
    starting_subgrammar: Option<String>,
    current_subgrammar: Option<String>,
    current_assoc: Option<Assoc>,
    current_prec: Prec,
}

impl<N: OpName> ParserBuilder<N> {
    pub fn new(language_name: &str) -> ParserBuilder<N> {
        ParserBuilder {
            shunter: GrammarBuilder::new(language_name),
            tokenset: TokenSet::new(),
            starting_subgrammar: None,
            current_subgrammar: None,
            current_assoc: None,
            current_prec: 0,
        }
    }

    pub fn subgrammar(mut self, name: &str) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.current_subgrammar = Some(name.to_owned());
        if self.starting_subgrammar.is_none() {
            self.starting_subgrammar = Some(name.to_owned());
        }
        Ok(self)
    }

    pub fn assoc_l(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Left);
        Ok(self)
    }

    pub fn assoc_r(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        self.current_prec += 1;
        self.current_assoc = Some(Assoc::Right);
        Ok(self)
    }

    pub fn whitespace(
        mut self,
        whitespace_regex: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        if self.current_subgrammar.is_some() {
            return Err(ParserBuilderError::WhitespaceInsideSubgrammar);
        }
        self.tokenset.insert_whitespace(&whitespace_regex);
        Ok(self)
    }

    pub fn regex_literal(
        mut self,
        name: N,
        regex: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        let subgrammar = match self.current_subgrammar.as_ref() {
            Some(subgrammar) => subgrammar,
            None => return Err(ParserBuilderError::LiteralOutsideSubgrammar),
        };
        if self.current_assoc.is_some() {
            return Err(ParserBuilderError::LiteralInsideAssoc);
        }
        let token = self.tokenset.regex(regex);
        self.shunter = self.shunter.op(
            subgrammar,
            name,
            0,
            Assoc::Right, // doesn't matter
            Fixity::Nilfix,
            token,
            vec![],
        )?;
        Ok(self)
    }

    pub fn string_literal(
        mut self,
        name: N,
        string: &str,
    ) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        let subgrammar = match self.current_subgrammar.as_ref() {
            Some(subgrammar) => subgrammar,
            None => return Err(ParserBuilderError::LiteralOutsideSubgrammar),
        };
        if self.current_assoc.is_some() {
            return Err(ParserBuilderError::LiteralInsideAssoc);
        }
        let token = self.tokenset.string(string);
        self.shunter = self.shunter.op(
            subgrammar,
            name,
            0,
            Assoc::Right, // doesn't matter
            Fixity::Nilfix,
            token,
            vec![],
        )?;
        Ok(self)
    }

    pub fn op(mut self, op: Op<N>) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        let subgrammar = match self.current_subgrammar.as_ref() {
            Some(subgrammar) => subgrammar,
            None => return Err(ParserBuilderError::OpOutsideSubgrammar),
        };
        let assoc = match (self.current_assoc, op.fixity) {
            (None, Fixity::Nilfix) => Assoc::Right, // doesn't matter
            (Some(_), Fixity::Nilfix) => return Err(ParserBuilderError::OpForbidsAssoc(op.name)),
            (None, _) => return Err(ParserBuilderError::OpRequiresAssoc(op.name)),
            (Some(assoc), _) => assoc,
        };
        let prec = self.current_prec;
        let token = self.tokenset.string(&op.first_token);
        let mut followers: Vec<(&str, Token)> = vec![];
        for (nt, lit) in &op.followers {
            let token = self.tokenset.string(lit);
            followers.push((nt, token));
        }
        self.shunter = self.shunter.op(
            subgrammar, op.name, prec, assoc, op.fixity, token, followers,
        )?;
        Ok(self)
    }

    pub fn op_juxtapose(mut self) -> Result<ParserBuilder<N>, ParserBuilderError<N>> {
        let subgrammar = match self.current_subgrammar.as_ref() {
            Some(subgrammar) => subgrammar,
            None => return Err(ParserBuilderError::OpOutsideSubgrammar),
        };
        // TODO: can this mix with other ops in an op group? Can if have diff assocs?
        let prec = self.current_prec;
        self.shunter = self.shunter.op_juxtapose(subgrammar, prec)?;
        Ok(self)
    }

    pub fn finish(self) -> Result<Parser<N>, ParserBuilderError<N>> {
        let starting_subgrammar = match self.starting_subgrammar.as_ref() {
            None => return Err(ParserBuilderError::NoSubgrammars),
            Some(starting_subgrammar) => starting_subgrammar,
        };
        let shunter = self.shunter.finish(starting_subgrammar)?;
        let (lexer, token_names) = self.tokenset.into_lexer()?;
        Ok(Parser {
            shunter,
            lexer,
            token_names,
        })
    }
}
