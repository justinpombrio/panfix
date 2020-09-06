use super::grammar::{Parser, Token};
use crate::lexer::Span;
use crate::rpn_visitor::Stack as RpnStack;
use crate::rpn_visitor::Visitor as RpnVisitor;
use crate::rpn_visitor::VisitorIter as RpnVisitorIter;
use crate::shunter::{Node, Operator};

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
    pub fn op(&self) -> &'a Operator<Token> {
        self.visitor.node().op
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

    pub fn children<'s>(&'s self) -> VisitorIter {
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
}
