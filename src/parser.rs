use crate::grammar::{Grammar, Subgrammar};
use crate::lexer::{Lexeme, Position};
use crate::op::{Op, Prec, Sort};
use crate::parse_tree::{Node, ParseTree};
use crate::rpn_visitor::RpnStack;
use std::iter::Peekable;
use thiserror::Error;

impl Grammar {
    /// Parse the `source` text as the given `sort`. Runs in linear time.
    pub fn parse<'s, 'g>(
        &'g self,
        sort: &str,
        source: &'s str,
    ) -> Result<ParseTree<'s, 'g>, ParseError<'s, 'g>> {
        let lexeme_stream = self.lexer.lex(source);
        let parser = Parser::new(source, self, lexeme_stream);
        parser.parse(sort)
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError<'s, 'g> {
    #[error("Parse Error: unexpected token '{}'.\nAt {}", lexeme.lexeme, lexeme.span)]
    UnexpectedLexeme { lexeme: Lexeme<'s> },
    #[error("Parse Error: {} expected {}, but found '{}'.\nAt {}",
            op_name, expected, found.lexeme, found.span)]
    MissingSep {
        op_name: &'g str,
        expected: &'g str,
        found: Lexeme<'s>,
    },
    #[error(
        "Parse Error: {} expected {}, but found end of file.\nAt {}",
        op_name,
        expected,
        pos
    )]
    MissingSepEof {
        op_name: &'g str,
        expected: &'g str,
        pos: Position,
    },
    #[error("Tried to parse a '{0}', but the grammar does not contain that sort.")]
    NoSuchSort(Sort),
}

struct Parser<'s, 'g, I: Iterator<Item = Lexeme<'s>>> {
    source: &'s str,
    grammar: &'g Grammar,
    lexemes: Peekable<I>,
    last_pos: Position,
    output: RpnStack<Node<'s, 'g>>,
}

impl<'s, 'g, I: Iterator<Item = Lexeme<'s>>> Parser<'s, 'g, I> {
    fn new(source: &'s str, grammar: &'g Grammar, lexemes: I) -> Parser<'s, 'g, I> {
        Parser {
            source,
            grammar,
            lexemes: lexemes.peekable(),
            last_pos: Position::start(),
            output: RpnStack::new(),
        }
    }

    fn parse(mut self, sort: &str) -> Result<ParseTree<'s, 'g>, ParseError<'s, 'g>> {
        let subgrammar = if let Some(sort_id) = self.grammar.sort_ids.get(sort) {
            &self.grammar.subgrammars[*sort_id]
        } else {
            return Err(ParseError::NoSuchSort(sort.to_owned()));
        };
        self.parse_expr(subgrammar, Prec::MAX)?;
        if let Some(lexeme) = self.lexemes.next() {
            return Err(self.error_unexpected_lexeme(lexeme));
        }
        Ok(ParseTree::new(self.output))
    }

    fn parse_expr(
        &mut self,
        subgrammar: &'g Subgrammar,
        prec: Prec,
    ) -> Result<(), ParseError<'s, 'g>> {
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = &subgrammar.token_to_prefixy_op[lexeme.token] {
                self.consume_lexeme();
                self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    self.parse_expr(subgrammar, rprec)?;
                }
                self.output_op(lexeme, op);
            } else {
                // E.g. "2 + *"
                self.output_blank(subgrammar);
            }
        } else {
            // E.g. the entire file is "2 +"
            self.output_blank(subgrammar);
        };
        self.parse_suffix(subgrammar, prec)
    }

    fn parse_suffix(
        &mut self,
        subgrammar: &'g Subgrammar,
        prec: Prec,
    ) -> Result<(), ParseError<'s, 'g>> {
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = subgrammar.token_to_suffixy_op[lexeme.token].as_ref() {
                if op.left_prec.unwrap() <= prec {
                    self.consume_lexeme();
                    self.parse_followers(op)?;
                    if let Some(rprec) = op.right_prec {
                        self.parse_expr(subgrammar, rprec)?;
                    }
                    self.parse_suffix(subgrammar, prec)?;
                    self.output_op(lexeme, op);
                    Ok(())
                } else {
                    // Precedence is too high, this token is not for us
                    Ok(())
                }
            } else if subgrammar.token_to_prefixy_op[lexeme.token].is_some() {
                // E.g. you're looking to extend the expr "2 * 3" and find the next token is "4"
                let op = &subgrammar.juxtapose;
                if op.left_prec.unwrap() <= prec {
                    self.parse_expr(subgrammar, op.right_prec.unwrap())?;
                    self.parse_suffix(subgrammar, prec)?;
                    self.output_op(lexeme, op);
                    Ok(())
                } else {
                    // Precedence is too high, this token is not for us
                    Ok(())
                }
            } else {
                // Unknown token. Someone else will deal with it.
                Ok(())
            }
        } else {
            // EOF (totally expected)
            Ok(())
        }
    }

    fn parse_followers(&mut self, op: &'g Op) -> Result<(), ParseError<'s, 'g>> {
        for (nonterminal, expected_token) in &op.followers {
            let subgrammar = &self.grammar.subgrammars[*nonterminal];
            self.parse_expr(subgrammar, Prec::MAX)?;
            let lexeme = self.lexemes.next();
            if lexeme.map(|l| l.token) != Some(*expected_token) {
                let expected_token_name = self.grammar.token_names.get(expected_token).unwrap();
                return Err(self.error_missing_follower(&op.name, expected_token_name, lexeme));
            }
        }
        Ok(())
    }

    fn output_op(&mut self, lexeme: Lexeme<'s>, op: &'g Op) {
        let start = lexeme.span.start;
        self.output.push(Node {
            op,
            start,
            end: self.last_pos,
            source: &self.source[start.offset..self.last_pos.offset],
        });
    }

    fn output_blank(&mut self, subgrammar: &'g Subgrammar) {
        let start = self.last_pos;
        let end = self.last_pos;
        self.output.push(Node {
            op: &subgrammar.blank,
            start,
            end,
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
            ParseError::MissingSep {
                op_name,
                expected,
                found,
            }
        } else {
            ParseError::MissingSepEof {
                op_name,
                expected,
                pos: self.last_pos,
            }
        }
    }

    fn error_unexpected_lexeme(&self, lexeme: Lexeme<'s>) -> ParseError<'s, 'g> {
        ParseError::UnexpectedLexeme { lexeme }
    }

    fn consume_lexeme(&mut self) -> Lexeme<'s> {
        let lexeme = self.lexemes.next().unwrap();
        self.last_pos = lexeme.span.end;
        lexeme
    }
}
