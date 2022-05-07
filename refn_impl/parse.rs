use super::grammar::{Fixity, Grammar, Op, Prec, Subgrammar, Token};
use super::lex::{lex, Lexeme, Span};
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, Clone, Copy)]
pub struct Node<'g> {
    pub op: &'g Op,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct ParseTree<'s> {
    source: &'s str,
    op: &'s Op,
    span: Span,
    children: Vec<ParseTree<'s>>,
}

impl<'s> ParseTree<'s> {
    fn new_missing_atom(source: &'s str, g: &'s Subgrammar, loc: usize) -> ParseTree<'s> {
        ParseTree {
            source,
            op: &g.missing_atom,
            span: (loc, loc),
            children: vec![],
        }
    }

    fn new(source: &'s str, op: &'s Op, span: Span, children: Vec<ParseTree<'s>>) -> ParseTree<'s> {
        ParseTree {
            source,
            op,
            span,
            children,
        }
    }

    pub fn name(&self) -> &str {
        &self.op.name
    }

    pub fn arity(&self) -> usize {
        self.op.arity()
    }

    pub fn fixity(&self) -> Fixity {
        self.op.fixity
    }

    pub fn text(&self) -> &str {
        &self.source[self.span.0..self.span.1]
    }

    pub fn children(&self) -> impl Iterator<Item = &ParseTree<'s>> {
        self.children.iter()
    }
}

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("did not expect {0}")]
    UnexpectedToken(String),
    #[error("expected {expected}, but found {found}")]
    ExpectedOneThingFoundAnother { expected: String, found: String },
    #[error("expected {expected}, but found end of file")]
    ExpectedOneThingFoundEof { expected: String },
}

pub fn parse<'s>(grammar: &'s Grammar, source: &'s str) -> Result<ParseTree<'s>, ParseError> {
    let lexemes = lex(grammar, source);
    let mut parser = Parser {
        source,
        grammar,
        lexemes: lexemes.peekable(),
    };
    parser.parse(&grammar.subgrammars["main"])
}

struct Parser<'s, I: Iterator<Item = Lexeme>> {
    source: &'s str,
    grammar: &'s Grammar,
    lexemes: Peekable<I>,
}

impl<'s, I: Iterator<Item = Lexeme>> Parser<'s, I> {
    fn parse(&mut self, grammar: &'s Subgrammar) -> Result<ParseTree<'s>, ParseError> {
        let tree = self.parse_expr(grammar, Prec::MAX)?;
        if let Some(lexeme) = self.lexemes.next() {
            return Err(self.err_unexpected_token(lexeme));
        }
        Ok(tree)
    }

    fn parse_expr(&mut self, g: &'s Subgrammar, prec: Prec) -> Result<ParseTree<'s>, ParseError> {
        let tree = if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = g.token_to_prefixy_op[lexeme.token].as_ref() {
                self.lexemes.next();
                let mut children = self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    let last_child = self.parse_expr(g, rprec)?;
                    children.push(last_child);
                }
                ParseTree::new(self.source, op, lexeme.span, children)
            } else {
                // E.g. "2 + *".
                ParseTree::new_missing_atom(self.source, g, lexeme.span.0)
            }
        } else {
            // E.g. the entire file is "2 +".
            ParseTree::new_missing_atom(self.source, g, self.source.len())
        };
        self.parse_suffix(g, tree, prec)
    }

    fn parse_suffix(
        &mut self,
        g: &'s Subgrammar,
        expr: ParseTree<'s>,
        prec: Prec,
    ) -> Result<ParseTree<'s>, ParseError> {
        if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = g.token_to_suffixy_op[lexeme.token].as_ref() {
                if op.left_prec.unwrap() <= prec {
                    self.lexemes.next();
                    let mut children = vec![expr];
                    let followers = self.parse_followers(op)?;
                    children.extend(followers);
                    if let Some(rprec) = op.right_prec {
                        let last_child = self.parse_expr(g, rprec)?;
                        children.push(last_child);
                    }
                    let tree = ParseTree::new(self.source, op, lexeme.span, children);
                    self.parse_suffix(g, tree, prec)
                } else {
                    // Precedence is too high, this token is not for us
                    Ok(expr)
                }
            } else if g.token_to_prefixy_op[lexeme.token].is_some() {
                // E.g. you're looking to extend the expr "2 * 3" and find the next token is "4"
                let op = &g.juxtapose;
                if op.left_prec.unwrap() <= prec {
                    let last_child = self.parse_expr(g, op.right_prec.unwrap())?;
                    let children = vec![expr, last_child];
                    let tree = ParseTree::new(self.source, op, lexeme.span, children);
                    self.parse_suffix(g, tree, prec)
                } else {
                    // Precedence is too high, this token is not for us
                    Ok(expr)
                }
            } else {
                // Unknown token. Someone else will deal with it.
                Ok(expr)
            }
        } else {
            // EOF (totally expected)
            Ok(expr)
        }
    }

    fn parse_followers(&mut self, op: &'s Op) -> Result<Vec<ParseTree<'s>>, ParseError> {
        let mut children = vec![];
        for (nonterminal, expected_token) in &op.followers {
            let subgrammar = &self.grammar.subgrammars[*nonterminal];
            let tree = self.parse_expr(subgrammar, Prec::MAX)?;
            let lexeme = self.lexemes.next();
            if lexeme.map(|l| l.token) != Some(*expected_token) {
                return Err(self.err_waiting_for(*expected_token, lexeme));
            }
            children.push(tree);
        }
        Ok(children)
    }

    fn err_unexpected_token(&self, lexeme: Lexeme) -> ParseError {
        ParseError::UnexpectedToken(lexeme.text(self.source).to_owned())
    }

    fn err_waiting_for(&self, expected: Token, found: Option<Lexeme>) -> ParseError {
        if let Some(found) = found {
            ParseError::ExpectedOneThingFoundAnother {
                expected: self.grammar.token_display[expected].to_owned(),
                found: found.text(self.source).to_owned(),
            }
        } else {
            ParseError::ExpectedOneThingFoundEof {
                expected: self.grammar.token_display[expected].to_owned(),
            }
        }
    }
}
