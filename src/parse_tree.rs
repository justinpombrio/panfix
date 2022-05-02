use crate::lexing::Position;
use crate::op::CompiledOp;
use crate::rpn_visitor;
use crate::rpn_visitor::{RpnNode, RpnStack};

pub struct ParseTree<'s, 'g>(RpnStack<Node<'s, 'g>>);

#[derive(Debug, Clone)]
pub struct Node<'s, 'g> {
    pub op: &'g CompiledOp,
    pub source: &'s str,
    pub start: Position,
    pub end: Position,
}

#[derive(Debug)]
pub struct Visitor<'s, 'g, 't> {
    stack: &'t rpn_visitor::RpnStack<Node<'s, 'g>>,
    node: rpn_visitor::Visitor<'t, Node<'s, 'g>>,
}

impl<'s, 'g, 't> Clone for Visitor<'s, 'g, 't> {
    fn clone(&self) -> Visitor<'s, 'g, 't> {
        Visitor {
            stack: self.stack,
            node: self.node,
        }
    }
}
impl<'s, 'g, 't> Copy for Visitor<'s, 'g, 't> {}

impl<'s, 'g> RpnNode for Node<'s, 'g> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

impl<'s, 'g> ParseTree<'s, 'g> {
    pub(crate) fn new(stack: RpnStack<Node<'s, 'g>>) -> ParseTree<'s, 'g> {
        ParseTree(stack)
    }
}

impl<'s, 'g, 't> Visitor<'s, 'g, 't> {
    pub fn name(self) -> &'g str {
        self.node.op.name()
    }
}
