/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Parser {
    pub(crate) lexer: Lexer,

    pub(crate) sort_tables: Vec<SortTable>,
    pub(crate) sort_ids: HashMap<String, SortId>,
    pub(crate) token_names: HashMap<Token, String>,
}

impl Parser {
    /// Parse `source`. Runs in linear time.
    pub fn parse<'s, 'g>(
        &'g self,
        source: &'s Source,
    ) -> Result<ParseTree<'s, 'g>, ParseError<'s>> {
        let lexeme_stream = self.lexer.lex(source.source());
        ParseState::new(source, self, lexeme_stream).parse(sort)
    }
}

use crate::lexer::Lexer;
use crate::op::{Op, Prec};
use crate::parse_error::ParseError;
use crate::parse_tree::{Node, ParseTree};
use crate::tree_visitor::Forest;
use crate::{Lexeme, Source};
use crate::{Position, Span, Token};
use std::collections::HashMap;
use std::iter::Peekable;

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
        source: &'s Source,
        sort: &str,
    ) -> Result<ParseTree<'s, 'g>, ParseError<'s>> {
        let lexeme_stream = self.lexer.lex(source.source());
        ParseState::new(source, self, lexeme_stream).parse(sort)
    }
}

struct ParseState<'s, 'g, I: Iterator<Item = Lexeme<'s>>> {
    source: &'s Source,
    parser: &'g Parser,
    lexemes: Peekable<I>,
    last_pos: Position,
    output: Forest<Node<'s, 'g>>,
}

impl<'s, 'g, I: Iterator<Item = Lexeme<'s>>> ParseState<'s, 'g, I> {
    fn new(source: &'s Source, parser: &'g Parser, lexemes: I) -> ParseState<'s, 'g, I> {
        ParseState {
            source,
            parser,
            lexemes: lexemes.peekable(),
            last_pos: Position::start(),
            output: Forest::new(),
        }
    }

    fn parse(mut self, sort: &str) -> Result<ParseTree<'s, 'g>, ParseError<'s>> {
        let sort_table = if let Some(sort_id) = self.parser.sort_ids.get(sort) {
            &self.parser.sort_tables[*sort_id]
        } else {
            return Err(ParseError::no_such_sort(self.source, sort));
        };
        self.parse_expr(sort_table, Prec::MAX, None)?;
        if let Some(lexeme) = self.lexemes.next() {
            return Err(ParseError::unexpected_lexeme(self.source, lexeme));
        }
        Ok(ParseTree::new(self.source, self.output))
    }

    fn parse_expr(
        &mut self,
        sort_table: &'g SortTable,
        prec: Prec,
        awaiting_token: Option<Token>,
    ) -> Result<(), ParseError<'s>> {
        let mut start = self.last_pos;
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = &sort_table.token_to_prefixy_op[lexeme.token] {
                start = lexeme.span.start;
                self.consume_lexeme();
                self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    self.parse_expr(sort_table, rprec, awaiting_token)?;
                }
                self.output_op(lexeme, op, start, self.last_pos);
            } else {
                // E.g. "2 + *" at the "*"
                self.output_blank(sort_table);
            }
        } else {
            // E.g. the entire file is "2 +"
            self.output_blank(sort_table);
        };
        self.parse_suffix(sort_table, prec, start, awaiting_token)?;
        Ok(())
    }

    fn parse_suffix(
        &mut self,
        sort_table: &'g SortTable,
        prec: Prec,
        start: Position,
        awaiting_token: Option<Token>,
    ) -> Result<(), ParseError<'s>> {
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
                    self.output_op(lexeme, op, start, self.last_pos);
                    self.parse_suffix(sort_table, prec, start, awaiting_token)?;
                }
            } else if sort_table.token_to_prefixy_op[lexeme.token].is_some() {
                // E.g. you're looking to extend the expr "2 * 3" and find the next token is "4"
                let op = &sort_table.juxtapose;
                if op.left_prec.unwrap() <= prec {
                    self.parse_expr(sort_table, op.right_prec.unwrap(), awaiting_token)?;
                    self.output_op(lexeme, op, start, self.last_pos);
                    self.parse_suffix(sort_table, prec, start, awaiting_token)?;
                }
            }
        }
        Ok(())
    }

    fn parse_followers(&mut self, op: &'g Op) -> Result<(), ParseError<'s>> {
        for (sort_id, expected_token) in &op.followers {
            let sort_table = &self.parser.sort_tables[*sort_id];
            self.parse_expr(sort_table, Prec::MAX, Some(*expected_token))?;
            let lexeme = self.try_consume_lexeme();
            if let Some(lexeme) = lexeme {
                if lexeme.token != *expected_token {
                    return Err(ParseError::missing_sep(
                        self.source,
                        &op.name,
                        self.parser.token_names.get(expected_token).unwrap(),
                        lexeme,
                    ));
                }
            } else {
                let span = Span {
                    start: self.last_pos,
                    end: self.last_pos,
                };
                return Err(ParseError::missing_sep_eof(
                    self.source,
                    &op.name,
                    self.parser.token_names.get(expected_token).unwrap(),
                    span,
                ));
            }
        }
        Ok(())
    }

    fn output_op(&mut self, lexeme: Lexeme<'s>, op: &'g Op, start: Position, end: Position) {
        self.output.push(Node {
            op,
            op_span: lexeme.span,
            span: Span { start, end },
            slice: self.source.substr(Span { start, end }),
        });
    }

    fn output_blank(&mut self, sort_table: &'g SortTable) {
        let span = Span {
            start: self.last_pos,
            end: self.last_pos,
        };
        self.output.push(Node {
            op: &sort_table.blank,
            op_span: span,
            span,
            slice: self.source.substr(Span {
                start: self.last_pos,
                end: self.last_pos,
            }),
        });
    }

    fn consume_lexeme(&mut self) -> Lexeme<'s> {
        let lexeme = self.lexemes.next().unwrap();
        self.last_pos = lexeme.span.end;
        lexeme
    }

    fn try_consume_lexeme(&mut self) -> Option<Lexeme<'s>> {
        if let Some(lexeme) = self.lexemes.next() {
            self.last_pos = lexeme.span.end;
            Some(lexeme)
        } else {
            None
        }
    }
}
