use crate::lexer::Position;
use crate::op::{Assoc, Fixity, Op, Prec};
use crate::rpn_visitor::{RpnNode, RpnStack, RpnVisitor, RpnVisitorIter};

pub struct ParseTree<'s, 'g>(RpnStack<Node<'s, 'g>>);

impl<'s, 'g> ParseTree<'s, 'g> {
    pub(crate) fn new(stack: RpnStack<Node<'s, 'g>>) -> ParseTree<'s, 'g> {
        ParseTree(stack)
    }

    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'g, 't> {
        Visitor {
            // Parser guarantees there's at least one node
            node: self.0.last_group().unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Node<'s, 'g> {
    pub op: &'g Op,
    pub source: &'s str,
    pub start: Position,
    pub end: Position,
}

impl<'s, 'g> RpnNode for Node<'s, 'g> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Visitor<'s, 'g, 't> {
    node: RpnVisitor<'t, Node<'s, 'g>>,
}

impl<'s, 'g, 't> Visitor<'s, 'g, 't> {
    pub fn name(&self) -> &'g str {
        &self.node.op.name
    }

    pub fn fixity(&self) -> Fixity {
        self.node.op.fixity
    }

    pub fn assoc(&self) -> Assoc {
        self.node.op.assoc
    }

    pub fn prec(&self) -> Prec {
        self.node.op.prec
    }

    pub fn source(&self) -> &'s str {
        self.node.source
    }

    pub fn arity(&self) -> usize {
        self.node.op.arity
    }

    pub fn children(&self) -> impl Iterator<Item = Visitor<'s, 'g, 't>> {
        VisitorIter {
            node: self.node.children(),
        }
    }
}

#[derive(Debug)]
pub struct VisitorIter<'s, 'g, 't> {
    node: RpnVisitorIter<'t, Node<'s, 'g>>,
}

impl<'s, 'g, 't> Iterator for VisitorIter<'s, 'g, 't> {
    type Item = Visitor<'s, 'g, 't>;

    fn next(&mut self) -> Option<Visitor<'s, 'g, 't>> {
        let node = self.node.next()?;
        Some(Visitor { node })
    }
}
