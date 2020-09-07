use super::parser::{Fixity, Parser, Pattern, Token};
use crate::lexing::Span;
use crate::rpn_visitor::Stack as RpnStack;
use crate::rpn_visitor::Visitor as RpnVisitor;
use crate::rpn_visitor::VisitorIter as RpnVisitorIter;
use crate::shunting::Node;

pub struct Parsed<'a> {
    source: &'a str,
    stack: RpnStack<Node<'a, Token>>,
}

pub struct Visitor<'a> {
    source: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
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
        match (rule.left_prec.is_some(), rule.right_prec.is_some()) {
            (false, false) => None,
            (false, true) => Some(Prefix),
            (true, false) => Some(Suffix),
            (true, true) => Some(Infix),
        }
    }

    pub fn rule_tokens(&self) -> &[Token] {
        &self.visitor.node().rule.tokens
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

impl Parser {
    pub fn parse<'s>(&'s self, source: &'s str) -> Parsed<'s> {
        let tokens = self.lexer.lex(source);
        let rpn = self.shunter.shunt(tokens);
        let stack = RpnStack::from_iter(rpn);
        Parsed { source, stack }
    }

    pub fn token_pattern(&self, token: Token) -> Option<&Pattern> {
        self.token_patterns.get(&token)
    }
}
