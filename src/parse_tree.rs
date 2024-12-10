use crate::op::{Assoc, Fixity, Op, Prec};
use crate::parse_error::ParseError;
use crate::source::Source;
use crate::tree_visitor::{Arity, Forest, Visitor as ForestVisitor};
use crate::{Parser, Position, Span, NAME_BLANK, NAME_JUXTAPOSE};
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

    pub fn source(&self) -> &'s Source {
        self.source
    }
}

impl<'p> Arity for Item<'p> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

/// One node in a parse tree. Allows you to inspect the node and its children, but not its parent.
#[derive(Debug, Clone, Copy)]
pub struct Visitor<'s, 'p, 't> {
    source: &'s Source,
    parser: &'p Parser,
    node: ForestVisitor<'t, Item<'p>>,
}

impl<'s, 'p, 't> Visitor<'s, 'p, 't> {
    /// The name of the op at this node.
    pub fn name(&self) -> &'p str {
        &self.node.item().op.name
    }

    /// The span of this node and it's children.
    pub fn span(&self) -> Span {
        Span {
            start: self.start(),
            end: self.end(),
        }
    }

    fn start(&self) -> Position {
        match self.fixity() {
            Fixity::Infix | Fixity::Suffix => self.child(0).start(),
            Fixity::Nilfix | Fixity::Prefix => self.node.item().span.start,
        }
    }

    fn end(&self) -> Position {
        match self.fixity() {
            Fixity::Infix | Fixity::Prefix => self.child(self.num_children() - 1).end(),
            Fixity::Nilfix | Fixity::Suffix => self.node.item().span.end,
        }
    }

    /// The span of this node's first token.
    pub fn token_span(&self) -> Span {
        self.node.item().span
    }

    /// The source text covered by `.token_span()`.
    pub fn token_source(&self) -> &'s str {
        self.source.substr(self.span())
    }

    /// The source text covered by `.span()`.
    pub fn source(&self) -> &'s str {
        self.source.substr(self.span())
    }

    /// The fixity of this node's operator.
    pub fn fixity(&self) -> Fixity {
        self.node.item().op.fixity
    }

    /// The precedence of this node's operator.
    pub fn prec(&self) -> Prec {
        self.node.item().op.prec
    }

    /// The associativity of this node's operator.
    pub fn assoc(&self) -> Assoc {
        self.node.item().op.assoc
    }

    /// The tokens of this node's operator.
    pub fn tokens(&self) -> &[String] {
        &self.node.item().op.tokens
    }

    /// The number of children this node has (which is determined by its operator).
    pub fn num_children(&self) -> usize {
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

    /// Extract this visitor's children into an array.
    ///
    /// # Panics
    ///
    /// Panics if `N` does not match the number of children. Note that the number of children is
    /// not dynamic: you can tell how many there will be from the grammar. Even if a child is
    /// "missing", it will actually be represented as blank.
    pub fn children<const N: usize>(&self) -> [Visitor<'s, 'p, 't>; N] {
        let mut array = [*self; N]; // dummy value
        if N != self.num_children() {
            panic!(
                "Visitor::expect_children -- expected {} children but found {}",
                N,
                self.num_children()
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

    /// Create a custom parsing error with the given message at the location `self.span()`.
    pub fn error(&self, short_message: &str, message: &str) -> ParseError<'s> {
        ParseError::custom_error(self.source, short_message, message, self.span())
    }

    /// Create a custom parsing error with the given message at the location `self.token_span()`.
    pub fn error_at_token(&self, short_message: &str, message: &str) -> ParseError<'s> {
        ParseError::custom_error(self.source, short_message, message, self.token_span())
    }
}

impl<'s, 'p, 't> fmt::Display for Visitor<'s, 'p, 't> {
    /// Display this node as an s-expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.name() == NAME_BLANK {
            write!(f, "_")
        } else if self.num_children() == 0 {
            write!(f, "{}", self.source())
        } else {
            write!(f, "(")?;
            if self.name() == NAME_JUXTAPOSE {
                write!(f, "_")?;
            } else {
                write!(f, "{}", self.name())?;
            }
            for i in 0..self.num_children() {
                write!(f, " {}", self.child(i))?;
            }
            write!(f, ")")
        }
    }
}

impl<'s, 'p> fmt::Display for ParseTree<'s, 'p> {
    /// Display this tree as an s-expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.visitor())
    }
}
