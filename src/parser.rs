use crate::rpn_visitor::Node as RpnNode;
use crate::rpn_visitor::Stack;
use crate::{Lexeme, Span, Token};
use std::fmt;

// TODO:
// - Multifix
// - ParsrConstructionError
// - Handle lexing errors
// - Proper Source data structure, with line numbers

type Prec = u32;

#[derive(Debug, Clone)]
pub struct Operator<T: Token> {
    pub name: String,
    pub left_prec: Option<Prec>,
    pub right_prec: Option<Prec>,
    pub tokens: Vec<T>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Fixity {
    Nilfix,
    Prefix(Prec),
    Suffix(Prec),
    Infix(Prec, Prec),
}

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, T: Token> {
    pub op: &'g Operator<T>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct Grammar<T: Token> {
    token_to_op: Vec<Option<Operator<T>>>,
    missing: Operator<T>,
    juxtapose: Operator<T>,
}

#[derive(Debug, Clone)]
pub struct GrammarBuilder<T: Token> {
    ops: Vec<Operator<T>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

impl<'g, T: Token> RpnNode for Node<'g, T> {
    fn arity(&self) -> usize {
        Node::arity(*self)
    }
}

impl<T: Token> GrammarBuilder<T> {
    pub fn new() -> GrammarBuilder<T> {
        GrammarBuilder { ops: vec![] }
    }

    pub fn nilfix(&mut self, name: &str, token: T) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: None,
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn prefix(&mut self, name: &str, token: T, right_prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: None,
            right_prec: Some(right_prec),
            tokens: vec![token],
        });
        self
    }

    pub fn suffix(&mut self, name: &str, token: T, left_prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(left_prec),
            right_prec: None,
            tokens: vec![token],
        });
        self
    }

    pub fn infixl(&mut self, name: &str, token: T, prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec),
            tokens: vec![token],
        });
        self
    }

    pub fn infixr(&mut self, name: &str, token: T, prec: Prec) -> &mut Self {
        self.ops.push(Operator {
            name: name.to_owned(),
            left_prec: Some(prec),
            right_prec: Some(prec + 1),
            tokens: vec![token],
        });
        self
    }

    pub fn build(&self) -> Grammar<T> {
        Grammar::new(self.ops.clone())
    }
}

#[derive(Debug)]
struct Parser<'s, 'g, T: Token> {
    source: &'s str,
    grammar: &'g Grammar<T>,
    // Mode::Expr: looking for an expr.
    // Mode::Suffix: looking to extend an existing expr with a suffix.
    mode: Mode,
    // Operators that are still waiting for args. The span is the span of the operator itself.
    op_stack: Vec<(&'g Operator<T>, Span)>,
    // TODO: impl Iterator instead
    // The operators in RPN order. This is the final output.
    rpn: Stack<Node<'g, T>>,
    // The full spans of the groups currently on the rpn stack.
    spans: Vec<Span>,
    // The left position of the last seen token.
    last_pos: usize,
}

impl<'s, 'g, T: Token> fmt::Display for Parser<'s, 'g, T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for node in self.rpn.nodes() {
            write!(f, "{} ", &self.source[node.span.0..node.span.1])?;
        }
        write!(f, "| ")?;
        for (_, span) in &self.op_stack {
            write!(f, "{} ", &self.source[span.0..span.1])?;
        }
        match self.mode {
            Mode::Expr => write!(f, "|e")?,
            Mode::Suffix => write!(f, "|s")?,
        }
        Ok(())
    }
}

pub fn parse<'s, 'g, I, T: Token>(
    grammar: &'g Grammar<T>,
    source: &'s str,
    lexemes: I,
) -> Stack<Node<'g, T>>
where
    I: IntoIterator<Item = Lexeme<T>>,
{
    let mut parser = Parser::new(source, grammar);
    parser.parse(lexemes);
    parser.finish()
}

impl<'s, 'g, T: Token> Parser<'s, 'g, T> {
    fn new(source: &'s str, grammar: &'g Grammar<T>) -> Parser<'s, 'g, T> {
        Parser {
            source,
            grammar,
            mode: Mode::Expr,
            op_stack: vec![],
            rpn: Stack::new(),
            spans: vec![],
            last_pos: 0,
        }
    }

    fn push(&mut self, op: &'g Operator<T>, op_span: Span) {
        use Fixity::*;
        match op.fixity() {
            Nilfix => self.push_nilfix(op, op_span),
            Prefix(_) => self.push_prefix(op, op_span),
            Suffix(_) => self.push_suffix(op, op_span),
            Infix(_, _) => self.push_infix(op, op_span),
        }
    }

    fn push_nilfix(&mut self, op: &'g Operator<T>, op_span: Span) {
        self.spans.push(op_span);
        self.rpn.push(Node { op, span: op_span });
    }

    fn push_prefix(&mut self, op: &'g Operator<T>, op_span: Span) {
        let arg_span = self.spans.pop().unwrap();
        let span = (op_span.0, arg_span.1);
        self.spans.push(span);
        self.rpn.push(Node { op, span });
    }

    fn push_suffix(&mut self, op: &'g Operator<T>, op_span: Span) {
        let arg_span = self.spans.pop().unwrap();
        let span = (arg_span.0, op_span.1);
        self.spans.push(span);
        self.rpn.push(Node { op, span });
    }

    fn push_infix(&mut self, op: &'g Operator<T>, _op_span: Span) {
        let arg_2_span = self.spans.pop().unwrap();
        let arg_1_span = self.spans.pop().unwrap();
        let span = (arg_1_span.0, arg_2_span.1);
        self.spans.push(span);
        self.rpn.push(Node { op, span });
    }

    fn parse<I>(&mut self, lexemes: I)
    where
        I: IntoIterator<Item = Lexeme<T>>,
    {
        use Fixity::*;

        let juxt_prec: Prec = self.grammar.juxtapose.left_prec.unwrap();
        let mut lexemes = lexemes.into_iter().peekable();
        while let Some(lexeme) = lexemes.peek() {
            let lexeme = *lexeme;
            let op = self.grammar.token_to_op[lexeme.token.as_usize()]
                .as_ref()
                .unwrap_or_else(|| {
                    panic!("No operator provided for token {:?}", lexeme.token);
                });
            match self.mode {
                Mode::Expr => match op.fixity() {
                    // Rule 1.
                    Nilfix => {
                        lexemes.next();
                        self.push_nilfix(op, lexeme.span);
                        self.mode = Mode::Suffix;
                    }
                    // Rule 2.
                    Prefix(_) => {
                        lexemes.next();
                        self.op_stack.push((op, lexeme.span));
                    }
                    // Rule 9, then Rule 1.
                    Suffix(_) | Infix(_, _) => {
                        self.push_nilfix(&self.grammar.missing, (self.last_pos, self.last_pos));
                        self.mode = Mode::Suffix;
                    }
                },
                Mode::Suffix => {
                    // Using Prec::MAX handles Rules 6 and 7.
                    let prec = self
                        .op_stack
                        .last()
                        .map(|(op, _)| op.right_prec.unwrap())
                        .unwrap_or(Prec::MAX);
                    match op.fixity() {
                        // Rule 11 or 12, followed by 3.
                        Nilfix | Prefix(_) if prec <= juxt_prec => {
                            let (last_op, span) = self.op_stack.pop().unwrap();
                            self.push(last_op, span);
                        }
                        // Rule 11 or 12, followed by 4.
                        Nilfix | Prefix(_) => {
                            let span = (lexeme.span.0, lexeme.span.0);
                            self.op_stack.push((&self.grammar.juxtapose, span));
                            self.mode = Mode::Expr;
                        }
                        // Rule 3.
                        Infix(l, _) | Suffix(l) if prec <= l => {
                            let (last_op, span) = self.op_stack.pop().unwrap();
                            self.push(last_op, span);
                        }
                        // Rule 4.
                        Infix(_, _) => {
                            lexemes.next();
                            self.op_stack.push((op, lexeme.span));
                            self.mode = Mode::Expr;
                        }
                        // Rule 5.
                        Suffix(_) => {
                            lexemes.next();
                            self.push_suffix(op, lexeme.span);
                        }
                    }
                }
            }
            println!("{}", self);
            self.last_pos = lexeme.span.1;
        }
    }

    fn finish(mut self) -> Stack<Node<'g, T>> {
        // Rule 10, then Rule 1.
        if self.mode == Mode::Expr {
            self.push_nilfix(&self.grammar.missing, (self.last_pos, self.last_pos));
        }
        // Rule 8.
        while let Some((op, span)) = self.op_stack.pop() {
            self.push(op, span);
        }
        self.rpn
    }
}

impl<'g, T: Token> Operator<T> {
    fn fixity(&self) -> Fixity {
        match (self.left_prec, self.right_prec) {
            (None, None) => Fixity::Nilfix,
            (Some(i), None) => Fixity::Suffix(i),
            (None, Some(i)) => Fixity::Prefix(i),
            (Some(i), Some(j)) => Fixity::Infix(i, j),
        }
    }

    fn arity(&self) -> usize {
        let mut arity = 0;
        if self.left_prec.is_some() {
            arity += 1;
        }
        if self.right_prec.is_some() {
            arity += 1;
        }
        arity
    }
}

impl<'g, T: Token> Node<'g, T> {
    pub fn fixity(self) -> Fixity {
        self.op.fixity()
    }

    pub fn arity(self) -> usize {
        self.op.arity()
    }

    pub fn text(self, source: &str) -> &str {
        &source[self.span.0..self.span.1]
    }
}

impl<'g, T: Token> Grammar<T> {
    pub fn new(ops: Vec<Operator<T>>) -> Grammar<T> {
        let mut largest_token: usize = 0;
        for op in &ops {
            for token in &op.tokens {
                largest_token = largest_token.max(token.as_usize());
            }
        }
        let mut token_to_op = vec![None; largest_token + 1];
        let mut missing = None;
        let mut juxtapose = None;
        for op in ops {
            if op.name == "Missing" {
                assert!(op.left_prec.is_none());
                assert!(op.right_prec.is_none());
                missing = Some(op);
            } else if op.name == "Juxtapose" {
                assert!(op.left_prec.is_some());
                assert!(op.right_prec.is_some());
                juxtapose = Some(op);
            } else if let Some(token) = op.tokens.first() {
                let index = token.as_usize();
                token_to_op[index] = Some(op);
            }
        }
        // TODO: unwrap -> Err
        Grammar {
            token_to_op,
            missing: missing.unwrap().to_owned(),
            juxtapose: juxtapose.unwrap().to_owned(),
        }
    }
}
