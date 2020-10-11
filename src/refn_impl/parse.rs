use super::grammar::{Assoc, Fixity, Grammar, Op, Prec, Subgrammar, Token};
use super::lex::{Lexeme, Span};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy)]
pub struct Node<'g> {
    pub op: &'g Op,
    pub span: Span,
}

pub struct ParseTree<'g> {
    op: &'g Op,
    span: Span,
    children: Vec<ParseTree<'g>>,
}

impl<'g> ParseTree<'g> {
    fn new_missing_atom(g: &'g Subgrammar, loc: usize) -> ParseTree<'g> {
        ParseTree {
            op: &g.missing_atom,
            span: (loc, loc),
            children: vec![],
        }
    }

    fn new(op: &'g Op, span: Span, children: Vec<ParseTree<'g>>) -> ParseTree<'g> {
        ParseTree { op, span, children }
    }
}

pub enum ParseError {
    UnexpectedToken(String),
    WaitingFor {
        expected: String,
        found: Option<String>,
    },
}

impl Grammar {
    pub fn parse<'g, I>(&'g self, source: &'g str, lexemes: I) -> Result<ParseTree<'g>, ParseError>
    where
        I: Iterator<Item = Lexeme>,
    {
        let mut parser = Parser {
            source,
            grammar: self,
            lexemes: lexemes.peekable(),
        };
        parser.parse(&self.subgrammars["main"])
    }
}

struct Parser<'g, I: Iterator<Item = Lexeme>> {
    source: &'g str,
    grammar: &'g Grammar,
    lexemes: Peekable<I>,
}

impl<'g, I: Iterator<Item = Lexeme>> Parser<'g, I> {
    fn parse(&mut self, grammar: &'g Subgrammar) -> Result<ParseTree<'g>, ParseError> {
        let tree = self.parse_expr(grammar, Prec::MAX)?;
        if let Some(lexeme) = self.lexemes.next() {
            return Err(self.err_unexpected_token(lexeme));
        }
        Ok(tree)
    }

    fn parse_expr(&mut self, g: &'g Subgrammar, prec: Prec) -> Result<ParseTree<'g>, ParseError> {
        let tree = if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = g.token_to_prefixy_op[lexeme.token].as_ref() {
                self.lexemes.next();
                let mut children = self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    let last_child = self.parse_expr(g, rprec)?;
                    children.push(last_child);
                }
                ParseTree::new(op, lexeme.span, children)
            } else {
                // E.g. "2 + *".
                ParseTree::new_missing_atom(g, lexeme.span.0)
            }
        } else {
            // E.g. the entire file is "2 +".
            ParseTree::new_missing_atom(g, self.source.len())
        };
        self.parse_suffix(g, tree, prec)
    }

    fn parse_suffix(
        &mut self,
        g: &'g Subgrammar,
        expr: ParseTree<'g>,
        prec: Prec,
    ) -> Result<ParseTree<'g>, ParseError> {
        if let Some(lexeme) = self.lexemes.peek().copied() {
            let op = if let Some(op) = g.token_to_suffixy_op[lexeme.token].as_ref() {
                op
            } else if g.token_to_prefixy_op[lexeme.token].is_some() {
                // E.g. you're looking to extend the expr "2 * 3" and find the next token is "4"
                &g.juxtapose
            } else {
                // Unknown token. Someone else will deal with it.
                return Ok(expr);
            };
            if op.left_prec.unwrap() <= prec {
                self.lexemes.next();
                let mut children = self.parse_followers(op)?;
                if let Some(rprec) = op.right_prec {
                    let last_child = self.parse_expr(g, rprec)?;
                    children.push(last_child);
                }
                let tree = ParseTree::new(op, lexeme.span, children);
                self.parse_suffix(g, tree, prec)
            } else {
                // Precedence is too high, this token is not for us
                Ok(expr)
            }
        } else {
            // EOF (totally expected)
            Ok(expr)
        }
    }

    fn parse_followers(&mut self, op: &'g Op) -> Result<Vec<ParseTree<'g>>, ParseError> {
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
        ParseError::WaitingFor {
            expected: self.grammar.token_display[expected].to_owned(),
            found: found.map(|l| l.text(self.source).to_owned()),
        }
    }
}
