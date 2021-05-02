use crate::lexing;
use crate::lexing::Lexer;
use crate::line_and_col_indexer::LineAndColIndexer;
use crate::rpn_forest::{RpnForest, RpnNode, RpnTree};
use crate::shunting;
use crate::shunting::{Fixity, Grammar, Lexeme, Node, OpName, ShuntError, Span, Token};

pub struct Parser<N: OpName> {
    pub(super) shunter: Grammar<N>,
    pub(super) lexer: Lexer<Token>,
}

#[derive(Debug)]
pub struct Parsed<'s, N: OpName> {
    source: &'s str,
    indexer: LineAndColIndexer<'s>,
    stack: RpnForest<Node<'s, N>>,
}

#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

/**********
 * Errors *
 **********/

pub struct ParseError<'s, N: OpName> {
    inner: ParseErrorInner<N>,
    span: (Position, Position),
    contents_of_lines: &'s str,
}

// TODO: impl Error
pub enum ParseErrorInner<N: OpName> {
    LexError(Lexeme),
    UnexpectedToken(Lexeme),
    MissingFollower {
        op: N,
        expected: Token,
        found: Option<Lexeme>,
    },
    MissingRequiredNode {
        parent: N,
        preceding_token: Option<Token>,
        child_index: usize,
    },
}

impl<'s, N: OpName> ParseError<'s, N> {
    fn new(
        indexer: &LineAndColIndexer<'s>,
        span: Span,
        inner: ParseErrorInner<N>,
    ) -> ParseError<'s, N> {
        ParseError {
            inner,
            span: span_to_positions(indexer, span),
            contents_of_lines: contents_of_span(indexer, span),
        }
    }

    fn from_shunt_error(
        indexer: &LineAndColIndexer<'s>,
        error: ShuntError<N>,
    ) -> ParseError<'s, N> {
        use ParseErrorInner::*;

        let (span, inner) = match error {
            ShuntError::LexError(lexeme) => (lexeme.span, LexError(lexeme)),
            ShuntError::UnexpectedToken(lexeme) => (lexeme.span, UnexpectedToken(lexeme)),
            ShuntError::MissingFollower(op, expected, found) => {
                let inner = MissingFollower {
                    op,
                    expected,
                    found,
                };
                let span = match found {
                    Some(lexeme) => lexeme.span,
                    None => (indexer.source().len(), indexer.source().len()),
                };
                (span, inner)
            }
        };
        ParseError::new(indexer, span, inner)
    }
}

fn span_to_positions(indexer: &LineAndColIndexer, span: Span) -> (Position, Position) {
    let (start, end) = span;
    let (start_line, start_col) = indexer.line_col(start);
    let (end_line, end_col) = indexer.line_col(end);
    let start_pos = Position {
        line: start_line,
        column: start_col,
    };
    let end_pos = Position {
        line: end_line,
        column: end_col,
    };
    (start_pos, end_pos)
}

fn contents_of_span<'s>(indexer: &LineAndColIndexer<'s>, span: Span) -> &'s str {
    let (start_line, _) = indexer.line_col(span.0);
    let (end_line, _) = indexer.line_col(span.1);
    let (start_of_first_line, _) = indexer.line_span(start_line);
    let (_, end_of_last_line) = indexer.line_span(end_line);
    &indexer.source()[start_of_first_line..end_of_last_line]
}

impl<'s, N: OpName> Parsed<'s, N> {}

/***********
 * Parsing *
 ***********/

impl<N: OpName + 'static> Parser<N> {
    pub fn parse<'s>(&'s self, source: &'s str) -> Result<Parsed<'s, N>, ParseError<N>> {
        let indexer = LineAndColIndexer::new(source);
        let lexemes = self.lexer.lex(source).map(|lexeme| lexeme.into());
        let rpn = self.shunter.shunt(lexemes);
        let mut stack = RpnForest::new();
        for node in rpn {
            match node {
                Ok(node) => stack.push(node),
                Err(err) => return Err(ParseError::from_shunt_error(&indexer, err)),
            };
        }
        Ok(Parsed {
            source,
            indexer,
            stack,
        })
    }
}

/*************
 * Glue Code *
 *************/

impl<'a, N: OpName> RpnNode for Node<'a, N> {
    fn arity(&self) -> usize {
        self.op().arity()
    }
}

impl From<lexing::Lexeme<Token>> for shunting::Lexeme {
    fn from(lexeme: lexing::Lexeme<Token>) -> shunting::Lexeme {
        shunting::Lexeme {
            token: lexeme.token,
            span: lexeme.span,
        }
    }
}

/***************
 * Parse Trees *
 ***************/

#[derive(Debug)]
pub struct ParseTree<'s, 'p, N: OpName> {
    parsed: &'p Parsed<'s, N>,
    tree: RpnTree<'s, Node<'s, N>>,
}

impl<'s, 'p, N: OpName> ParseTree<'s, 'p, N> {
    pub fn name(&self) -> N {
        self.tree.node().op().name()
    }

    pub fn arity(&self) -> usize {
        self.tree.node().op().arity()
    }

    pub fn fixity(&self) -> Fixity {
        self.tree.node().op().fixity()
    }

    pub fn span(&self) -> (Position, Position) {
        span_to_positions(&self.parsed.indexer, self.tree.node().span())
    }

    pub fn contents_of_lines(&self) -> &'s str {
        contents_of_span(&self.parsed.indexer, self.tree.node().span())
    }

    // TODO: doc. Panics.
    pub fn opt_child(&self, index: usize) -> Self {
        let tree = self.tree.children().nth(index).unwrap_or_else(|| {
            panic!("req_child: invalid index {} for op {}", index, self.name());
        });
        ParseTree {
            parsed: self.parsed,
            tree,
        }
    }

    // TODO: doc. Panics.
    pub fn child(&self, index: usize) -> Result<Self, ParseError<'s, N>> {
        let child = self.opt_child(index);
        if child.name() == N::MISSING_ATOM {
            let span = child.tree.node().span();
            let preceding_token = child.tree.node().op().token_before_child(index);
            let inner = ParseErrorInner::MissingRequiredNode {
                parent: self.name(),
                child_index: index,
                preceding_token,
            };
            Err(ParseError::new(&self.parsed.indexer, span, inner))
        } else {
            Ok(child)
        }
    }
}

/*
use super::grammar::{Parser, Token};
use crate::lexing::{Pattern, Span};
use crate::rpn_visitor::Stack as RpnStack;
use crate::rpn_visitor::Visitor as RpnVisitor;
use crate::rpn_visitor::VisitorIter as RpnVisitorIter;
use crate::shunting::{Assoc, Fixity, Node, ShuntError};
use std::error::Error;
use std::fmt;

// TODO: Get line&col nums
#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Parsed<'a> {
    source: &'a str,
    stack: RpnStack<Node<'a, Token>>,
}

#[derive(Debug, Clone, Copy)]
pub struct Visitor<'a> {
    source: &'a str,
    visitor: RpnVisitor<'a, Node<'a, Token>>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError {
        lexeme: String,
        pos: Position,
    },
    ExtraSeparator {
        separator: String,
        pos: Position,
    },
    MissingSeparator {
        op_name: String,
        separator: String,
        pos: Position,
    },
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;

        match self {
            LexError{lexeme, pos} => write!(
                f,
                "Lexing failed. It did not recognize the characters '{}'. Line {} ({}:{})",
                lexeme, pos.line, pos.line, pos.column
            ),
            ExtraSeparator{separator, pos} => write!(
               f,
               "Parsing failed. It did not expect to find '{}' on its own. Line {} ({}:{})",
               separator, pos.line, pos.line, pos.column
            ),
            MissingSeparator{op_name, separator, pos} => write!(
            f,
            "Parsing failed. It expected to find '{}' as part of {}, but could not. Line {} ({}:{})",
            op_name, separator, pos.line, pos.line, pos.column
            ),
        }
    }
}

impl Parser {
    pub fn parse<'s>(&'s self, source: &'s str) -> Result<Parsed<'s>, ParseError> {
        let tokens = self.lexer.lex(source);
        let rpn = self.shunter.shunt(tokens);
        let mut stack = RpnStack::new();
        for node in rpn {
            match node {
                Err(ShuntError::LexError(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let lexeme = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::LexError { lexeme, pos });
                }
                Err(ShuntError::ExtraSep(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let separator = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::ExtraSeparator { separator, pos });
                }
                Err(ShuntError::MissingSep {
                    op_name,
                    span,
                    token,
                }) => {
                    let pos = Position {
                        line: 0,
                        column: span.0 + 1,
                    };
                    let separator = match self.lexer.get_token_pattern(token).unwrap() {
                        Pattern::Constant(constant) => constant.to_string(),
                        Pattern::Regex { name, .. } => format!("{}", name),
                    };
                    return Err(ParseError::MissingSeparator {
                        op_name,
                        separator,
                        pos,
                    });
                }
                Ok(node) => stack.push(node),
            }
        }
        Ok(Parsed { source, stack })
    }
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
    pub fn name(self) -> &'a str {
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

    pub fn children(self) -> VisitorIter<'a> {
        VisitorIter {
            source: self.source,
            iter: self.visitor.children(),
        }
    }

    pub fn expect_1_child(self) -> Visitor<'a> {
        let mut children = self.children();
        assert_eq!(
            children.len(),
            1,
            "Visitor.expect_1_child: there wasn't 1 child"
        );
        children.next().unwrap()
    }

    pub fn expect_2_children(self) -> (Visitor<'a>, Visitor<'a>) {
        let mut children = self.children();
        assert_eq!(
            children.len(),
            2,
            "Visitor.expect_2_children: there weren't 2 children"
        );
        let child_1 = children.next().unwrap();
        let child_2 = children.next().unwrap();
        (child_1, child_2)
    }

    pub fn expect_3_children(self) -> (Visitor<'a>, Visitor<'a>, Visitor<'a>) {
        let mut children = self.children();
        assert_eq!(
            children.len(),
            3,
            "Visitor.expect_3_children: there weren't 3 children"
        );
        let child_1 = children.next().unwrap();
        let child_2 = children.next().unwrap();
        let child_3 = children.next().unwrap();
        (child_1, child_2, child_3)
    }

    pub fn expect_4_children(self) -> (Visitor<'a>, Visitor<'a>, Visitor<'a>, Visitor<'a>) {
        let mut children = self.children();
        assert_eq!(
            children.len(),
            4,
            "Visitor.expect_4_children: there weren't 4 children"
        );
        let child_1 = children.next().unwrap();
        let child_2 = children.next().unwrap();
        let child_3 = children.next().unwrap();
        let child_4 = children.next().unwrap();
        (child_1, child_2, child_3, child_4)
    }

    /// Iterate over the elements of a right-associative list.
    /// For example, if "Comma" is a right-associative infix operator and `self` is the tree:
    ///
    /// ```(Comma A (Comma B (Comma C D)))```
    ///
    /// then iterator over A, B, C, D.
    pub fn iter_right(self, op: &'static str) -> InfixIter<'a> {
        InfixIter {
            visitor: Some(self),
            op,
            assoc: Assoc::Right,
        }
    }

    /// Iterate over the elements of a left-associative list, in reverse order.
    /// For example, if "Plus" is left-associative infix operator and `self` is the tree:
    ///
    /// ```(Plus (Plus (Plus A B) C) D)```
    ///
    /// then iterator over D, C, B, A.
    ///
    /// This method exists because iterating in order requires allocation, and this library prides
    /// itself on avoiding allocations. To get the elements in order, you can use [Visitor::iter_left_vec].
    pub fn iter_left_rev(self, op: &'static str) -> InfixIter<'a> {
        InfixIter {
            visitor: Some(self),
            op,
            assoc: Assoc::Left,
        }
    }

    /// Like [Visitor::iter_left_rev], but in order and allocated in a `Vec`.
    pub fn iter_left_vec(self, op: &'static str) -> Vec<Visitor<'a>> {
        let mut vec = self.iter_left_rev(op).collect::<Vec<_>>();
        vec.reverse();
        vec
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

pub struct InfixIter<'a> {
    visitor: Option<Visitor<'a>>,
    op: &'a str,
    assoc: Assoc,
}

impl<'a> Iterator for InfixIter<'a> {
    type Item = Visitor<'a>;
    fn next(&mut self) -> Option<Visitor<'a>> {
        self.visitor.map(|visitor| {
            if visitor.name() == self.op {
                let (left, right) = visitor.expect_2_children();
                match self.assoc {
                    Assoc::Left => {
                        self.visitor = Some(left);
                        right
                    }
                    Assoc::Right => {
                        self.visitor = Some(right);
                        left
                    }
                }
            } else {
                self.visitor = None;
                visitor
            }
        })
    }
}
*/
