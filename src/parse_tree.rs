use crate::op::{Assoc, Fixity, Op, Prec};
use crate::parse_error::ParseError;
use crate::source::Source;
use crate::tree_visitor::{Arity, Forest, Visitor as ForestVisitor};
use crate::{Parser, Position, Span, Token};
use std::fmt;

/// The result of parsing a source string. Call `.visitor()` to walk it.
///
/// To minimize allocations, this contains references into both the source text and the grammar, so
/// it cannot outlive either.
#[derive(Debug)]
pub struct ParseTree<'s, 'p, T: Token> {
    source: &'s Source,
    parser: &'p Parser<T>,
    forest: Forest<Item<'p, T>>,
}

#[derive(Debug, Clone)]
pub(crate) struct Item<'p, T: Token> {
    pub(crate) op: &'p Op<T>,
    pub(crate) span: Span,
}

impl<T: Token> Copy for Item<'_, T> {}

impl<'s, 'p, T: Token> ParseTree<'s, 'p, T> {
    pub(crate) fn new(
        source: &'s Source,
        parser: &'p Parser<T>,
        forest: Forest<Item<'p, T>>,
    ) -> ParseTree<'s, 'p, T> {
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
    pub fn visitor<'t>(&'t self) -> Visitor<'s, 'p, 't, T> {
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

impl<T: Token> Arity for Item<'_, T> {
    fn arity(&self) -> usize {
        self.op.arity
    }
}

/// One node in a parse tree. Allows you to inspect the node and its children, but not its parent.
#[derive(Debug, Clone)]
pub struct Visitor<'s, 'p, 't, T: Token> {
    source: &'s Source,
    parser: &'p Parser<T>,
    node: ForestVisitor<'t, Item<'p, T>>,
}

impl<T: Token> Copy for Visitor<'_, '_, '_, T> {}

impl<'s, 'p, 't, T: Token> Visitor<'s, 'p, 't, T> {
    /// The token for the op at this node.
    pub fn token(&self) -> T {
        self.node.item().op.token.clone()
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

    /// The number of children this node has (which is determined by its operator).
    pub fn num_children(&self) -> usize {
        self.node.item().op.arity
    }

    /// Get this node's `n`th child.
    ///
    /// # Panics if there aren't at least `n` children.
    #[track_caller]
    pub fn child(&self, n: usize) -> Visitor<'s, 'p, 't, T> {
        match self.node.child(n) {
            Some(node) => Visitor {
                source: self.source,
                parser: self.parser,
                node,
            },
            None => panic!(
                "Visitor: child index '{}' out of bound for op '{}'",
                n,
                self.node.item().op.token
            ),
        }
    }

    /// Extract this visitor's children into an array.
    ///
    /// # Panics
    ///
    /// Panics if `N` does not match the number of children. Note that the number of children is
    /// not dynamic: you can tell how many there will be from the grammar. Even if a child is
    /// "missing", it will actually be represented as blank.
    #[track_caller]
    pub fn children<const N: usize>(&self) -> [Visitor<'s, 'p, 't, T>; N] {
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

impl<T: Token> fmt::Display for Visitor<'_, '_, '_, T> {
    /// Display this node as an s-expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.token() == T::BLANK {
            write!(f, "_")
        } else if self.num_children() == 0 {
            write!(f, "{}", self.source())
        } else {
            write!(f, "(")?;
            if self.token() == T::JUXTAPOSE {
                write!(f, "_")?;
            } else {
                write!(f, "{}", self.token())?;
            }
            for i in 0..self.num_children() {
                write!(f, " {}", self.child(i))?;
            }
            write!(f, ")")
        }
    }
}

impl<T: Token> fmt::Display for ParseTree<'_, '_, T> {
    /// Display this tree as an s-expression.
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.visitor())
    }
}
