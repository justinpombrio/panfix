use crate::lexer::Position;
use crate::op::{Fixity, Op, Prec};
use crate::rpn_visitor::{RpnNode, RpnStack, RpnVisitor, RpnVisitorIter};

/// The result of parsing a source string. Call `.visitor()` to walk it.
///
/// To minimize allocations, this contains references into both the source text and the grammar, so
/// it cannot outlive either.
pub struct ParseTree<'s, 'g>(RpnStack<Node<'s, 'g>>);

impl<'s, 'g> ParseTree<'s, 'g> {
    pub(crate) fn new(stack: RpnStack<Node<'s, 'g>>) -> ParseTree<'s, 'g> {
        ParseTree(stack)
    }

    /// Obtains a "visitor" that can walk the source tree.
    ///
    /// (This is not the visitor pattern: you're not supplying a function to run on each node.
    /// Instead you can inspect the nodes and do whatever you want yourself.)
    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'g, 't> {
        Visitor {
            // Parser guarantees there's at least one node
            node: self.0.last_group().unwrap(),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Node<'s, 'g> {
    pub(crate) op: &'g Op,
    pub(crate) source: &'s str,
    pub(crate) start: Position,
    pub(crate) end: Position,
}

impl<'s, 'g> RpnNode for Node<'s, 'g> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

/// One node in the parse tree. Allows you to inspect this node and its children, but not its
/// parent.
#[derive(Debug, Clone, Copy)]
pub struct Visitor<'s, 'g, 't> {
    node: RpnVisitor<'t, Node<'s, 'g>>,
}

impl<'s, 'g, 't> Visitor<'s, 'g, 't> {
    /// The name of the operator at this node.
    pub fn op(&self) -> &'g str {
        &self.node.op.name
    }

    /// The position just to the left of this operator and its arguments (if any).
    pub fn start(&self) -> Position {
        self.node.start
    }

    /// The position just to the right of this operator and its arguments (if any).
    pub fn end(&self) -> Position {
        self.node.end
    }

    /// The source text between the `start()` and `end()` position.
    pub fn source(&self) -> &'s str {
        self.node.source
    }

    /// The fixity of this node's operator.
    pub fn fixity(&self) -> Fixity {
        self.node.op.fixity
    }

    /// The precedence of this node's operator.
    pub fn prec(&self) -> Prec {
        self.node.op.prec
    }

    /// The number of children this node has.
    pub fn arity(&self) -> usize {
        self.node.op.arity
    }

    /// Iterate over this node's children.
    pub fn children(&self) -> impl Iterator<Item = Visitor<'s, 'g, 't>> {
        VisitorIter {
            node: self.node.children(),
        }
    }
}

#[derive(Debug)]
struct VisitorIter<'s, 'g, 't> {
    node: RpnVisitorIter<'t, Node<'s, 'g>>,
}

impl<'s, 'g, 't> Iterator for VisitorIter<'s, 'g, 't> {
    type Item = Visitor<'s, 'g, 't>;

    fn next(&mut self) -> Option<Visitor<'s, 'g, 't>> {
        let node = self.node.next()?;
        Some(Visitor { node })
    }
}
