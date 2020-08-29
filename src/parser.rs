use crate::{Lexeme, Token};
use std::fmt;

// TODO:
// - Multifix
// - Builder, and ParsrConstructionError

type Prec = u32;

#[derive(Debug, Clone)]
struct Operator<T: Token> {
    name: String,
    left_prec: Option<Prec>,
    right_prec: Option<Prec>,
    tokens: Vec<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Fixity {
    Nilfix,
    Prefix(Prec),
    Suffix(Prec),
    Infix(Prec, Prec),
}

#[derive(Debug, Clone, Copy)]
struct Node<'o, T: Token> {
    op: &'o Operator<T>,
    span: (usize, usize),
}

#[derive(Debug)]
struct Parser<'o, T: Token> {
    token_to_op: Vec<Option<&'o Operator<T>>>,
    missing: &'o Operator<T>,
    juxtapose: &'o Operator<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

/*
#[derive(Debug)]
struct Parse<'p, 's, 'o, T: Token> {
    parser: &'p Parser<'o, T>,
    mode: Mode,
    op_stack: Vec<Node<'o, T>>,
    rpn: Vec<Node<'o, T>>,
    spans: Vec<(usize, usize)>,
    last_pos: usize,
}

impl<'p, 's, 'o, T: Token> Parse<'p, 's, 'o, T> {
    fn push(&mut self, op: &'o Operator<T>, span: (usize, usize)) {
        self.rpn.push(Node { op, span });
    }
}
*/

impl<'o, T: Token> Operator<T> {
    fn fixity(&self) -> Fixity {
        match (self.left_prec, self.right_prec) {
            (None, None) => Fixity::Nilfix,
            (Some(i), None) => Fixity::Suffix(i),
            (None, Some(i)) => Fixity::Prefix(i),
            (Some(i), Some(j)) => Fixity::Infix(i, j),
        }
    }
}

impl<'o, T: Token> Node<'o, T> {
    fn fixity(self) -> Fixity {
        self.op.fixity()
    }
}

impl<'o, T: Token> Parser<'o, T> {
    fn new(ops: &'o [Operator<T>]) -> Parser<'o, T> {
        let mut largest_token: usize = 0;
        for op in ops {
            for token in &op.tokens {
                largest_token = largest_token.max(token.as_usize());
            }
        }
        let mut token_to_op = vec![None; largest_token + 1];
        let mut missing = None;
        let mut juxtapose = None;
        for op in ops {
            if let Some(token) = op.tokens.first() {
                token_to_op[token.as_usize()] = Some(op);
            }
            if op.name == "Missing" {
                assert!(op.left_prec.is_none());
                assert!(op.right_prec.is_none());
                missing = Some(op);
            } else if op.name == "Juxtapose" {
                assert!(op.left_prec.is_some());
                assert!(op.right_prec.is_some());
                juxtapose = Some(op);
            }
        }
        // TODO: unwrap -> Err
        Parser {
            token_to_op,
            missing: missing.unwrap(),
            juxtapose: juxtapose.unwrap(),
        }
    }

    fn parse<'s, I>(&self, lexemes: I) -> Vec<Node<'o, T>>
    where
        I: IntoIterator<Item = Lexeme<T>>,
    {
        use Fixity::*;
        let mut mode = Mode::Expr;
        let mut op_stack: Vec<Node<'o, T>> = vec![];
        let mut rpn: Vec<Node<'o, T>> = vec![];
        let mut spans: Vec<(usize, usize)> = vec![];
        let juxt_prec: Prec = self.juxtapose.left_prec.unwrap();
        let mut last_pos: usize = 0;
        let mut lexemes = lexemes.into_iter().peekable();
        let mut push_nilfix = |op, span| {
            rpn.push(Node { op, span });
            spans.push(span);
            mode = Mode::Suffix;
        };
        while let Some(lexeme) = lexemes.peek() {
            let lexeme = *lexeme;
            last_pos = lexeme.span.1;
            let op = self.token_to_op[lexeme.token.as_usize()].unwrap_or_else(|| {
                panic!("No operator provided for token {:?}", lexeme.token);
            });
            let node = Node {
                op,
                span: lexeme.span,
            };
            match mode {
                Mode::Expr => match op.fixity() {
                    // Rule 1.
                    Nilfix => {
                        lexemes.next();
                        rpn.push(node);
                        mode = Mode::Suffix;
                    }
                    // Rule 2.
                    Prefix(_) => {
                        lexemes.next();
                        op_stack.push(node);
                    }
                    // Rule 9, then Rule 1.
                    Suffix(_) | Infix(_, _) => {
                        rpn.push(Node {
                            span: (lexeme.span.0, lexeme.span.0),
                            op: self.missing,
                        });
                        mode = Mode::Suffix;
                    }
                },
                Mode::Suffix => {
                    // Using Prec::MAX handles Rules 6 and 7.
                    let prec = op_stack
                        .last()
                        .map(|node| node.op.right_prec.unwrap())
                        .unwrap_or(Prec::MAX);
                    match op.fixity() {
                        // Rule 11 or 12, followed by 3.
                        Nilfix | Prefix(_) if prec < juxt_prec => {
                            let last_op = op_stack.pop().unwrap();
                            rpn.push(last_op);
                        }
                        // Rule 11 or 12, followed by 4.
                        Nilfix | Prefix(_) => {
                            op_stack.push(Node {
                                span: (lexeme.span.0, lexeme.span.0),
                                op: self.juxtapose,
                            });
                            mode = Mode::Expr;
                        }
                        // Rule 3.
                        Infix(l, _) | Suffix(l) if prec < l => {
                            let last_op = op_stack.pop().unwrap();
                            rpn.push(last_op);
                        }
                        // Rule 4.
                        Infix(_, _) => {
                            lexemes.next();
                            op_stack.push(node);
                            mode = Mode::Expr;
                        }
                        // Rule 5.
                        Suffix(_) => {
                            lexemes.next();
                            op_stack.push(node);
                        }
                    }
                }
            }
        }
        // Rule 10, then Rule 1.
        if mode == Mode::Expr {
            rpn.push(Node {
                span: (last_pos, last_pos),
                op: self.missing,
            });
        }
        // Rule 8.
        while let Some(op) = op_stack.pop() {
            rpn.push(op);
        }
        rpn
    }
}
