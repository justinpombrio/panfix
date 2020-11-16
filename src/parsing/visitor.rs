use super::grammar::{Parser, Token};
use crate::lexing::{Pattern, Span};
use crate::rpn_visitor::Stack as RpnStack;
use crate::rpn_visitor::Visitor as RpnVisitor;
use crate::rpn_visitor::VisitorIter as RpnVisitorIter;
use crate::shunting::{Fixity, Node};

#[derive(Debug)]
pub struct Parsed<'a> {
    pub(super) source: &'a str,
    pub(super) stack: RpnStack<Node<'a, Token>>,
}

impl<'a> Parsed<'a> {
    pub fn source(&self) -> &'a str {
        self.source
    }

    pub fn root_nodes(&self) -> impl ExactSizeIterator<Item = OpChain> {
        // TODO: Is it actually possible for this to have more than one element?
        assert_eq!(self.stack.groups().len(), 1);
        OpChainIter {
            source: self.source,
            iter: self.stack.groups(),
        }
    }
}

#[derive(Debug)]
struct OpChainIter<'a> {
    source: &'a str,
    iter: RpnVisitorIter<'a, Node<'a, Token>>,
}

impl<'a> Iterator for OpChainIter<'a> {
    type Item = OpChain<'a>;
    fn next(&mut self) -> Option<OpChain<'a>> {
        match self.iter.next() {
            None => None,
            Some(v) => Some(OpChain {
                source: self.source,
                visitor: v,
            }),
        }
    }
}

impl<'a> ExactSizeIterator for OpChainIter<'a> {
    fn len(&self) -> usize {
        self.iter.len()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct OpChain<'a> {
    source: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
}

impl<'a> OpChain<'a> {
    pub fn op_group_name(self) -> &'a str {
        self.visitor.node().op.op_group_name()
    }

    pub fn links(self) -> impl Iterator<Item = Link<'a>> {
        LinkIter {
            source: self.source,
            op_group_name: self.op_group_name(),
            visitor: self.visitor,
        }
    }

    pub fn fold<T>(
        self,
        f_elem: impl Fn(Option<OpChain<'a>>) -> T,
        f_fold: impl Fn(T, &'a str, T) -> T,
    ) -> T {
        let first_arg = self.links().next().unwrap().left_arg();
        let mut accum = f_elem(first_arg);
        for link in self.links() {
            accum = f_fold(accum, link.op_name(), f_elem(link.right_arg()));
        }
        accum
    }

    // TODO: Fixing the impedence mismatch between this code and rpn_visitor should eliminate the
    // allocation here.
    pub fn rfold<T>(
        self,
        f_elem: impl Fn(Option<OpChain<'a>>) -> T,
        f_fold: impl Fn(T, &'a str, T) -> T,
    ) -> T {
        let links = self.links().collect::<Vec<_>>();
        let last_arg = links.last().unwrap().right_arg();
        let mut accum = f_elem(last_arg);
        for link in links.iter().rev() {
            accum = f_fold(f_elem(link.left_arg()), link.op_name(), accum);
        }
        accum
    }
}

#[derive(Debug, Clone, Copy)]
struct LinkIter<'a> {
    source: &'a str,
    op_group_name: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
}

impl<'a> Iterator for LinkIter<'a> {
    type Item = Link<'a>;
    fn next(&mut self) -> Option<Link<'a>> {
        if self.visitor.node().op.op_group_name() == self.op_group_name {
            Some(Link {
                source: self.source,
                visitor: self.visitor,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Link<'a> {
    source: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
}

impl<'a> Link<'a> {
    pub fn op_name(self) -> &'a str {
        self.visitor.node().op.name()
    }

    pub fn fixity(self) -> Fixity {
        self.visitor.node().op.fixity()
    }

    pub fn op_patterns(self, parser: &Parser) -> Vec<Option<&Pattern>> {
        self.visitor
            .node()
            .op
            .tokens()
            .map(|tok| parser.lexer.get_token_pattern(tok))
            .collect()
    }

    pub fn span(self) -> Span {
        self.visitor.node().span
    }

    pub fn arity(self) -> usize {
        self.visitor.node().arity()
    }

    pub fn text(self) -> &'a str {
        self.visitor.node().text(self.source)
    }

    pub fn left_arg(self) -> Option<OpChain<'a>> {
        if self.visitor.node().op.fixity().has_left_arg() {
            Some(OpChain {
                source: self.source,
                visitor: self.visitor.children().next().unwrap(),
            })
        } else {
            None
        }
    }

    // TODO: This is horrible. Due to mismatch between rpn_visitor and this link iterator. Bring
    // the visitor in line with how we're actually visiting things.
    pub fn right_arg(self) -> Option<OpChain<'a>> {
        if !self.visitor.node().op.fixity().has_right_arg() {
            return None;
        }
        let right_side = self.visitor.children().last().unwrap();
        if right_side.node().op.op_group_name() != self.visitor.node().op.op_group_name() {
            return Some(OpChain {
                source: self.source,
                visitor: right_side,
            });
        }
        if !right_side.node().op.fixity().has_left_arg() {
            return None;
        }
        Some(OpChain {
            source: self.source,
            visitor: right_side.children().next().unwrap(),
        })
    }

    pub fn enclosed_children(self) -> impl Iterator<Item = OpChain<'a>> {
        let children = self.visitor.children();
        let len = children.len();
        let (start, end) = match self.visitor.node().op.fixity() {
            Fixity::Infix => (1, len - 1),
            Fixity::Nilfix => (0, len),
            Fixity::Prefix => (0, len - 1),
            Fixity::Suffix => (1, len),
        };
        EnclosedChildrenIter {
            source: self.source,
            visitor: self.visitor.children(),
            index: start,
            end,
        }
    }
}

#[derive(Debug)]
pub struct EnclosedChildrenIter<'a> {
    source: &'a str,
    visitor: RpnVisitorIter<'a, Node<'a, Token>>,
    index: usize,
    end: usize,
}

impl<'a> Iterator for EnclosedChildrenIter<'a> {
    type Item = OpChain<'a>;
    fn next(&mut self) -> Option<OpChain<'a>> {
        if self.index == self.end {
            None
        } else {
            Some(OpChain {
                source: self.source,
                visitor: self.visitor.next().unwrap(),
            })
        }
    }
}
