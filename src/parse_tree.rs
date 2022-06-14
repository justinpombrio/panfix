use crate::op::{Fixity, Op, Prec};
use crate::parse_error::ParseError;
use crate::source::Source;
use crate::tree_visitor::{Arity, Forest, Visitor as ForestVisitor};
use crate::{Lexeme, Parser, Span, NAME_BLANK, NAME_JUXTAPOSE};
use std::fmt;

/// The result of parsing a source string. Call `.visitor()` to walk it.
///
/// To minimize allocations, this contains references into both the source text and the grammar, so
/// it cannot outlive either.
#[derive(Debug)]
pub struct ParseTree<'s, 'p> {
    source: &'s Source,
    parser: &'p Parser,
    forest: Forest<Item<'p>>,
}

#[derive(Debug, Clone, Copy)]
pub(crate) struct Item<'p> {
    pub(crate) op: &'p Op,
    pub(crate) span: Span,
}

impl<'s, 'p> ParseTree<'s, 'p> {
    pub(crate) fn new(
        source: &'s Source,
        parser: &'p Parser,
        forest: Forest<Item<'p>>,
    ) -> ParseTree<'s, 'p> {
        ParseTree {
            source,
            parser,
            forest,
        }
    }

    /// Obtains a "visitor" that can walk the source tree.
    ///
    /// (This is not the visitor pattern: you're not supplying a function to run on each node.
    /// Instead you can navigate the nodes and do whatever you want yourself.)
    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'p, 't> {
        Visitor {
            source: self.source,
            parser: self.parser,
            // Parser guarantees there's at least one node
            node: self.forest.tree(0).unwrap(),
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

impl<'p> Arity for Item<'p> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

/// One node in the parse tree. Allows you to inspect this node and its children, but not its
/// parent.
#[derive(Debug, Clone, Copy)]
pub struct Visitor<'s, 'p, 't> {
    source: &'s Source,
    parser: &'p Parser,
    node: ForestVisitor<'t, Item<'p>>,
}

impl<'s, 'p, 't> Visitor<'s, 'p, 't> {
    /// The name of the op at this node.
    pub fn name(&self) -> &'p str {
        &self.item().op.name
    }

    /// The span of this node's first token.
    pub fn span(&self) -> Span {
        self.item().span
    }

    /// The source text covered by `span()`.
    pub fn source(&self) -> &'s str {
        self.source.substr(self.item().span)
    }

    /// The fixity of this node's operator.
    pub fn fixity(&self) -> Fixity {
        self.node.item().op.fixity
    }

    /// The precedence of this node's operator.
    pub fn prec(&self) -> Prec {
        self.node.item().op.prec
    }

    /// The number of children this node has (which is determined by its operator).
    pub fn arity(&self) -> usize {
        self.node.item().op.arity
    }

    /// Get this node's `n`th child.
    ///
    /// # Panics if there aren't at least `n` children.
    pub fn child(&self, n: usize) -> Visitor<'s, 'p, 't> {
        Visitor {
            source: self.source,
            parser: self.parser,
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
    pub fn children(&self) -> impl Iterator<Item = Visitor<'s, 'p, 't>> {
        VisitorIter {
            source: self.source,
            parser: self.parser,
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
    /// "missing", it will actually be represented as blank.
    pub fn expect_children<const N: usize>(&self) -> [Visitor<'s, 'p, 't>; N] {
        let mut array = [*self; N]; // dummy value
        if N != self.arity() {
            panic!(
                "Visitor::expect_children -- expected {} children but found {}",
                N,
                self.arity()
            );
        }
        for (i, child) in array.iter_mut().enumerate() {
            *child = Visitor {
                source: self.source,
                parser: self.parser,
                node: self.node.child(i).unwrap(),
            };
        }
        array
    }

    fn item(&self) -> Item<'p> {
        *self.node.item()
    }
}

impl<'s, 'p, 't> fmt::Display for Visitor<'s, 'p, 't> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.name() == NAME_BLANK {
            write!(f, "_")
        } else if self.arity() == 0 {
            write!(f, "{}", self.source())
        } else {
            write!(f, "(")?;
            if self.name() == NAME_JUXTAPOSE {
                write!(f, "_")?;
            } else {
                write!(f, "{}", self.name())?;
            }
            for i in 0..self.arity() {
                write!(f, " {}", self.child(i))?;
            }
            write!(f, ")")
        }
    }
}

#[derive(Debug)]
struct VisitorIter<'s, 'p, 't> {
    source: &'s Source,
    parser: &'p Parser,
    node: ForestVisitor<'t, Item<'p>>,
    index: usize,
}

impl<'s, 'p, 't> Iterator for VisitorIter<'s, 'p, 't> {
    type Item = Visitor<'s, 'p, 't>;

    fn next(&mut self) -> Option<Visitor<'s, 'p, 't>> {
        if self.index < self.node.arity() {
            self.index += 1;
            Some(Visitor {
                source: self.source,
                parser: self.parser,
                node: self.node.child(self.index).unwrap(),
            })
        } else {
            None
        }
    }
}
