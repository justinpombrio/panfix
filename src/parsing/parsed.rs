use super::parser::{Fixity, Parser, Pattern, Token};
use crate::lexing::Span;
use crate::rpn_visitor::Stack as RpnStack;
use crate::rpn_visitor::Visitor as RpnVisitor;
use crate::rpn_visitor::VisitorIter as RpnVisitorIter;
use crate::shunting::{Node, ShuntError};
use std::error::Error;
use std::fmt;

// TODO: Get line&col nums
#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Parsed<'a> {
    source: &'a str,
    stack: RpnStack<Node<'a, Token>>,
}

pub struct Visitor<'a> {
    source: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError {
        lexeme: String,
        pos: Position,
    },
    ExtraSeparator {
        separator: String,
        pos: Position,
    },
    MissingSeparator {
        rule_name: String,
        separator: String,
        pos: Position,
    },
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;

        match self {
            LexError{lexeme, pos} => write!(
                f,
                "Lexing failed. It did not recognize the characters '{}'. Line {} ({}:{})",
                lexeme, pos.line, pos.line, pos.column
            ),
            ExtraSeparator{separator, pos} => write!(
               f,
               "Parsing failed. It did not expect to find '{}' on its own. Line {} ({}:{})",
               separator, pos.line, pos.line, pos.column
            ),
            MissingSeparator{rule_name, separator, pos} => write!(
            f,
            "Parsing failed. It expected to find '{}' as part of {}, but could not. Line {} ({}:{})",
            rule_name, separator, pos.line, pos.line, pos.column
            ),
        }
    }
}

impl Parser {
    pub fn parse<'s>(&'s self, source: &'s str) -> Result<Parsed<'s>, ParseError> {
        let tokens = self.lexer.lex(source);
        let rpn = self.shunter.shunt(tokens);
        let mut stack = RpnStack::new();
        for node in rpn {
            match node {
                Err(ShuntError::LexError(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let lexeme = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::LexError { lexeme, pos });
                }
                Err(ShuntError::ExtraSep(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let separator = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::ExtraSeparator { separator, pos });
                }
                Err(ShuntError::MissingSep {
                    rule_name,
                    span,
                    token,
                }) => {
                    let pos = Position {
                        line: 0,
                        column: span.0 + 1,
                    };
                    let separator = match self.token_patterns.get(&token).unwrap() {
                        Pattern::Constant(constant) => format!("{}", constant),
                        Pattern::Regex(regex) => format!("/{}/", regex),
                    };
                    return Err(ParseError::MissingSeparator {
                        rule_name,
                        separator,
                        pos,
                    });
                }
                Ok(node) => stack.push(node),
            }
        }
        Ok(Parsed { source, stack })
    }
}

impl<'a> Parsed<'a> {
    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn groups(&self) -> VisitorIter {
        VisitorIter {
            source: self.source,
            iter: self.stack.groups(),
        }
    }
}

impl<'a> Visitor<'a> {
    pub fn name(&self) -> &'a str {
        &self.visitor.node().rule.name
    }

    pub fn fixity(&self) -> Option<Fixity> {
        use Fixity::*;

        let rule = self.visitor.node().rule;
        match (
            rule.tokens.len(),
            rule.left_prec.is_some(),
            rule.right_prec.is_some(),
        ) {
            (1, false, false) => None,
            (_, false, false) => Some(Circumfix),
            (_, false, true) => Some(Prefix),
            (_, true, false) => Some(Suffix),
            (_, true, true) => Some(Infix),
        }
    }

    pub fn rule_patterns<'p>(&self, parser: &'p Parser) -> Vec<Option<&'p Pattern>> {
        self.visitor
            .node()
            .rule
            .tokens
            .iter()
            .map(|tok| parser.token_patterns.get(tok))
            .collect()
    }

    pub fn span(&self) -> Span {
        self.visitor.node().span
    }

    pub fn arity(&self) -> usize {
        self.visitor.node().arity()
    }

    pub fn text(&self) -> &'a str {
        self.visitor.node().text(self.source)
    }

    pub fn children(&self) -> VisitorIter {
        VisitorIter {
            source: self.source,
            iter: self.visitor.children(),
        }
    }
}

pub struct VisitorIter<'a> {
    source: &'a str,
    iter: RpnVisitorIter<'a, Node<'a, Token>>,
}

impl<'a> Iterator for VisitorIter<'a> {
    type Item = Visitor<'a>;
    fn next(&mut self) -> Option<Visitor<'a>> {
        match self.iter.next() {
            None => None,
            Some(v) => Some(Visitor {
                source: self.source,
                visitor: v,
            }),
        }
    }
}

impl<'a> ExactSizeIterator for VisitorIter<'a> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}
