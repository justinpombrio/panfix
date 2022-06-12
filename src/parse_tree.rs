use crate::op::{Fixity, Op, Prec};
use crate::parse_error::ParseError;
use crate::tree_visitor::{Arity, Forest, Visitor as RpnVisitor};
use crate::{Source, Span};
use std::fmt;

/// The result of parsing a source string. Call `.visitor()` to walk it.
///
/// To minimize allocations, this contains references into both the source text and the grammar, so
/// it cannot outlive either.
#[derive(Debug)]
pub struct ParseTree<'s, 'g> {
    source: &'s Source,
    stack: Forest<Node<'s, 'g>>,
}

impl<'s, 'g> ParseTree<'s, 'g> {
    pub(crate) fn new(source: &'s Source, stack: Forest<Node<'s, 'g>>) -> ParseTree<'s, 'g> {
        ParseTree { source, stack }
    }

    /// Obtains a "visitor" that can walk the source tree.
    ///
    /// (This is not the visitor pattern: you're not supplying a function to run on each node.
    /// Instead you can navigate the nodes and do whatever you want yourself.)
    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'g, 't> {
        Visitor {
            // Parser guarantees there's at least one node
            node: self.stack.tree(0).unwrap(),
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

#[derive(Debug, Clone, Copy)]
pub(crate) struct Node<'s, 'g> {
    pub(crate) op: &'g Op,
    pub(crate) op_span: Span,
    pub(crate) slice: &'s str,
    pub(crate) span: Span,
}

impl<'s, 'g> Arity for Node<'s, 'g> {
    fn arity(&self) -> usize {
        unimplemented!()
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
        &self.node.item().op.name
    }

    /// How many children this node has
    pub fn num_children(&self) -> usize {
        unimplemented!()
    }

    /// The span that includes only the initial lexeme of this operator.
    pub fn op_span(&self) -> Span {
        self.node.item().op_span
    }

    /// The span that includes this operator and all of its arguments.
    pub fn span(&self) -> Span {
        self.node.item().span
    }

    /// The source text covered by `span()`.
    pub fn source(&self) -> &'s str {
        self.node.item().slice
    }

    /// The fixity of this node's operator.
    pub fn fixity(&self) -> Fixity {
        self.node.item().op.fixity
    }

    /// The precedence of this node's operator.
    pub fn prec(&self) -> Prec {
        self.node.item().op.prec
    }

    /// Get this node's `n`th child.
    ///
    /// # Panics if there aren't at least `n` children.
    pub fn child(&self, n: usize) -> Visitor<'s, 'g, 't> {
        Visitor {
            node: self.node.child(n).unwrap_or_else(|| {
                panic!(
                    "Visitor: child index '{}' out of bound for op '{}'",
                    n,
                    self.node.item().op.name
                )
            }),
        }
    }

    /// Iterate over this node's children.
    pub fn children(&self) -> impl Iterator<Item = Visitor<'s, 'g, 't>> {
        VisitorIter {
            node: self.node,
            index: 0,
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
        let mut array = [*self; N]; // dummy value
        if N != self.num_children() {
            panic!(
                "Visitor::expect_children -- expected {} children but found {}",
                N,
                self.num_children()
            );
        }
        for i in 0..N {
            array[i] = Visitor {
                node: self.node.child(i).unwrap(),
            };
        }
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
            for i in 0..self.num_children() {
                write!(f, " {}", self.child(i))?;
            }
            write!(f, ")")
        }
    }
}

#[derive(Debug)]
struct VisitorIter<'s, 'g, 't> {
    node: RpnVisitor<'t, Node<'s, 'g>>,
    index: usize,
}

impl<'s, 'g, 't> Iterator for VisitorIter<'s, 'g, 't> {
    type Item = Visitor<'s, 'g, 't>;

    fn next(&mut self) -> Option<Visitor<'s, 'g, 't>> {
        let num_children = unimplemented!();
        if self.index < num_children {
            self.index += 1;
            Some(Visitor {
                node: self.node.child(self.index).unwrap(),
            })
        } else {
            None
        }
    }
}
