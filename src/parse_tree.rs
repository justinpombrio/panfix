use crate::op::{Fixity, Op, Prec};
use crate::parse_error::ParseError;
use crate::rpn_visitor::{RpnNode, RpnStack, RpnVisitor, RpnVisitorIter};
use crate::{Source, Span};
use std::fmt;

/// The result of parsing a source string. Call `.visitor()` to walk it.
///
/// To minimize allocations, this contains references into both the source text and the grammar, so
/// it cannot outlive either.
#[derive(Debug)]
pub struct ParseTree<'s, 'g> {
    source: &'s Source,
    stack: RpnStack<Node<'s, 'g>>,
}

impl<'s, 'g> ParseTree<'s, 'g> {
    pub(crate) fn new(source: &'s Source, stack: RpnStack<Node<'s, 'g>>) -> ParseTree<'s, 'g> {
        ParseTree { source, stack }
    }

    /// Obtains a "visitor" that can walk the source tree.
    ///
    /// (This is not the visitor pattern: you're not supplying a function to run on each node.
    /// Instead you can inspect the nodes and do whatever you want yourself.)
    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'g, 't> {
        Visitor {
            // Parser guarantees there's at least one node
            node: self.stack.last_group().unwrap(),
        }
    }

    /// The filename of the source text.
    pub fn filename(&self) -> &'s str {
        self.source.filename()
    }

    /// The entire source text.
    pub fn source(&self) -> &'s str {
        self.source.source()
    }

    /// Create a custom parse error at the given location.
    pub fn error(&self, span: Span, message: &str) -> ParseError<'s> {
        ParseError::custom_error(self.source, message, span)
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Node<'s, 'g> {
    pub(crate) op: &'g Op,
    pub(crate) op_span: Span,
    pub(crate) slice: &'s str,
    pub(crate) span: Span,
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

    /// The span that includes only the initial lexeme of this operator.
    pub fn op_span(&self) -> Span {
        self.node.op_span
    }

    /// The span that includes this operator and all of its arguments.
    pub fn span(&self) -> Span {
        self.node.span
    }

    /// The source text covered by `span()`.
    pub fn source(&self) -> &'s str {
        self.node.slice
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
    pub fn num_children(&self) -> usize {
        self.node.op.arity
    }

    /// Iterate over this node's children.
    pub fn children(&self) -> impl Iterator<Item = Visitor<'s, 'g, 't>> {
        VisitorIter {
            node: self.node.children(),
        }
    }

    /// Extract this visitor's children into an array.
    ///
    /// # Panics
    ///
    /// Panics if `N` does not match the number of children. Note that the number of children is
    /// not dynamic: you can tell how many there will be from the grammar. Even if a child is
    /// "missing", it will actually be represented as $blank.
    pub fn expect_children<const N: usize>(&self) -> [Visitor<'s, 'g, 't>; N] {
        let mut children = self.children();
        let mut array = [*self; N]; // dummy value
        for i in 0..N {
            array[i] = children
                .next()
                .expect("Visitor::expect_children -- there are not as many children as declared");
        }
        assert!(
            children.next().is_none(),
            "Visitor::expect_children -- there are more children than were declared"
        );
        array
    }
}

impl<'s, 'g, 't> fmt::Display for Visitor<'s, 'g, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.op() == "$Blank" {
            write!(f, "_")
        } else if self.num_children() == 0 {
            write!(f, "{}", self.source())
        } else {
            write!(f, "(")?;
            if self.op() == "$Juxtapose" {
                write!(f, "_")?;
            } else {
                write!(f, "{}", self.op())?;
            }
            for child in self.children() {
                write!(f, " {}", child)?;
            }
            write!(f, ")")
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
