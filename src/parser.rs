use crate::lexer::{Lexeme, Lexer, Position, Span, Token};
use crate::op::{Op, Prec, Sort, SortId};
use crate::parse_tree::{Node, ParseTree};
use crate::rpn_visitor::RpnStack;
use std::collections::HashMap;
use std::fmt;
use std::iter::Peekable;
use thiserror::Error;

use std::sync::atomic::{AtomicUsize, Ordering};
thread_local! {
    static DEBUG_NEST_LEVEL: AtomicUsize = AtomicUsize::new(0);
}

macro_rules! debug_begin {
    ($msg:literal $(, $arg:expr)*) => {
        let depth = DEBUG_NEST_LEVEL.with(|n| n.fetch_add(1, Ordering::SeqCst));
        for _ in 0..depth {
            print!("    ");
        }
        println!($msg $(, $arg)*);
    }
}
macro_rules! debug_end {
    ($msg:literal $(, $arg:expr)*) => {
        let depth = DEBUG_NEST_LEVEL.with(|n| n.fetch_sub(1, Ordering::SeqCst));
        for _ in 0..depth-1 {
            print!("    ");
        }
        println!($msg $(, $arg)*);
    }
}
macro_rules! debug {
    ($msg:literal $(, $arg:expr)*) => {
        let depth = DEBUG_NEST_LEVEL.with(|n| n.load(Ordering::SeqCst));
        for _ in 0..depth {
            print!("    ");
        }
        println!($msg $(, $arg)*);
    }
}

/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Parser {
    pub(crate) sort_tables: Vec<SortTable>,
    pub(crate) sort_ids: HashMap<String, SortId>,
    pub(crate) lexer: Lexer,
    pub(crate) token_names: HashMap<Token, String>,
}

#[derive(Debug, Clone)]
pub(crate) struct SortTable {
    pub(crate) sort: Sort,
    // Map from the first token in a Prefix or Nilfix op, to that op.
    pub(crate) token_to_prefixy_op: Vec<Option<Op>>,
    // Map from the first token in a Suffix or Infix op, to that op.
    pub(crate) token_to_suffixy_op: Vec<Option<Op>>,
    pub(crate) blank: Op,
    pub(crate) juxtapose: Op,
}

impl Parser {
    /// Parse the `source` text as the given `sort`. Runs in linear time.
    pub fn parse<'s, 'g>(
        &'g self,
        sort: &str,
        source: &'s str,
    ) -> Result<ParseTree<'s, 'g>, ParseError<'s, 'g>> {
        let lexeme_stream = self.lexer.lex(source);
        ParseState::new(source, self, lexeme_stream).parse(sort)
    }
}

#[derive(Debug, Error)]
pub struct ParseError<'s, 'g> {
    cause: ParseErrorCause<'s, 'g>,
    span: Option<Span>,
}

impl<'s, 'g> ParseError<'s, 'g> {
    pub fn custom_error(message: &str, span: Option<Span>) -> ParseError<'s, 'g> {
        ParseError {
            cause: ParseErrorCause::Custom(message.to_owned()),
            span,
        }
    }
}

impl<'s, 'g> fmt::Display for ParseError<'s, 'g> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Parse error: {}", self.cause)?;
        if let Some(span) = self.span {
            write!(f, "\nat {}", span)?;
        }
        Ok(())
    }
}

#[derive(Debug, Error)]
enum ParseErrorCause<'s, 'g> {
    #[error("{0}")]
    Custom(String),
    #[error("Unexpected token '{0}'.")]
    UnexpectedLexeme(Lexeme<'s>),
    #[error("While parsing '{}', expected '{}' but found '{}'.",
            op_name, expected, found.lexeme)]
    MissingSep {
        op_name: &'g str,
        expected: &'g str,
        found: Lexeme<'s>,
    },
    #[error(
        "While parsing '{}', expected '{}' but found end of file.",
        op_name,
        expected
    )]
    MissingSepEof { op_name: &'g str, expected: &'g str },
    #[error("Sort '{0}' is not defined in the grammar.")]
    NoSuchSort(String),
}

struct ParseState<'s, 'g, I: Iterator<Item = Lexeme<'s>>> {
    source: &'s str,
    parser: &'g Parser,
    lexemes: Peekable<I>,
    last_pos: Position,
    output: RpnStack<Node<'s, 'g>>,
}

impl<'s, 'g, I: Iterator<Item = Lexeme<'s>>> ParseState<'s, 'g, I> {
    fn new(source: &'s str, parser: &'g Parser, lexemes: I) -> ParseState<'s, 'g, I> {
        ParseState {
            source,
            parser,
            lexemes: lexemes.peekable(),
            last_pos: Position::start(),
            output: RpnStack::new(),
        }
    }

    fn parse(mut self, sort: &str) -> Result<ParseTree<'s, 'g>, ParseError<'s, 'g>> {
        let sort_table = if let Some(sort_id) = self.parser.sort_ids.get(sort) {
            &self.parser.sort_tables[*sort_id]
        } else {
            return Err(ParseError {
                cause: ParseErrorCause::NoSuchSort(sort.to_owned()),
                span: None,
            });
        };
        self.parse_expr(sort_table, Prec::MAX, None)?;
        if let Some(lexeme) = self.lexemes.next() {
            return Err(self.error_unexpected_lexeme(lexeme));
        }
        Ok(ParseTree::new(self.output))
    }

    fn parse_expr(
        &mut self,
        sort_table: &'g SortTable,
        prec: Prec,
        awaiting_token: Option<Token>,
    ) -> Result<(), ParseError<'s, 'g>> {
        debug_begin!("<expr> {}", prec);
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = &sort_table.token_to_prefixy_op[lexeme.token] {
                self.consume_lexeme();
                self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    self.parse_expr(sort_table, rprec, awaiting_token)?;
                }
                debug!("-> '{}'", lexeme);
                self.output_op(lexeme, op);
            } else {
                // E.g. "2 + *" at the "*"
                debug!("-> blank");
                self.output_blank(sort_table);
            }
        } else {
            // E.g. the entire file is "2 +"
            debug!("-> blank");
            self.output_blank(sort_table);
        };
        self.parse_suffix(sort_table, prec, awaiting_token)?;
        debug_end!("</expr>");
        Ok(())
    }

    fn parse_suffix(
        &mut self,
        sort_table: &'g SortTable,
        prec: Prec,
        awaiting_token: Option<Token>,
    ) -> Result<(), ParseError<'s, 'g>> {
        debug_begin!("<suff> {}", prec);
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if awaiting_token == Some(lexeme.token) {
                ()
            } else if let Some(op) = sort_table.token_to_suffixy_op[lexeme.token].as_ref() {
                if op.left_prec.unwrap() <= prec {
                    self.consume_lexeme();
                    self.parse_followers(op)?;
                    if let Some(rprec) = op.right_prec {
                        self.parse_expr(sort_table, rprec, awaiting_token)?;
                    }
                    debug!("-> '{}'", lexeme);
                    self.output_op(lexeme, op);
                    self.parse_suffix(sort_table, prec, awaiting_token)?;
                }
            } else if sort_table.token_to_prefixy_op[lexeme.token].is_some() {
                // E.g. you're looking to extend the expr "2 * 3" and find the next token is "4"
                let op = &sort_table.juxtapose;
                if op.left_prec.unwrap() <= prec {
                    self.parse_expr(sort_table, op.right_prec.unwrap(), awaiting_token)?;
                    debug!("-> '{}'", lexeme);
                    self.output_op(lexeme, op);
                    self.parse_suffix(sort_table, prec, awaiting_token)?;
                }
            }
        }
        debug_end!("</suff>");
        Ok(())
    }

    fn parse_followers(&mut self, op: &'g Op) -> Result<(), ParseError<'s, 'g>> {
        for (sort_id, expected_token) in &op.followers {
            let sort_table = &self.parser.sort_tables[*sort_id];
            self.parse_expr(sort_table, Prec::MAX, Some(*expected_token))?;
            let lexeme = self.lexemes.next();
            if lexeme.map(|l| l.token) != Some(*expected_token) {
                let expected_token_name = self.parser.token_names.get(expected_token).unwrap();
                return Err(self.error_missing_follower(&op.name, expected_token_name, lexeme));
            }
        }
        Ok(())
    }

    fn output_op(&mut self, lexeme: Lexeme<'s>, op: &'g Op) {
        let start = lexeme.span.start;
        self.output.push(Node {
            op,
            span: Span {
                start,
                end: self.last_pos,
            },
            source: &self.source[start.offset..self.last_pos.offset],
        });
    }

    fn output_blank(&mut self, sort_table: &'g SortTable) {
        let start = self.last_pos;
        let end = self.last_pos;
        self.output.push(Node {
            op: &sort_table.blank,
            span: Span { start, end },
            source: &self.source[start.offset..end.offset],
        });
    }

    fn error_missing_follower(
        &self,
        op_name: &'g str,
        expected: &'g str,
        found: Option<Lexeme<'s>>,
    ) -> ParseError<'s, 'g> {
        if let Some(found) = found {
            ParseError {
                cause: ParseErrorCause::MissingSep {
                    op_name,
                    expected,
                    found,
                },
                span: Some(found.span),
            }
        } else {
            ParseError {
                cause: ParseErrorCause::MissingSepEof { op_name, expected },
                span: Some(Span {
                    start: self.last_pos,
                    end: self.last_pos,
                }),
            }
        }
    }

    fn error_unexpected_lexeme(&self, lexeme: Lexeme<'s>) -> ParseError<'s, 'g> {
        ParseError {
            cause: ParseErrorCause::UnexpectedLexeme(lexeme),
            span: Some(lexeme.span),
        }
    }

    fn consume_lexeme(&mut self) -> Lexeme<'s> {
        let lexeme = self.lexemes.next().unwrap();
        self.last_pos = lexeme.span.end;
        lexeme
    }
}
