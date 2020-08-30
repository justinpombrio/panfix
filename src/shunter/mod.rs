use std::iter::Peekable;

mod builder;
mod grammar;

pub use crate::{Lexeme, Span, Token};

pub use builder::GrammarBuilder;
pub use grammar::{Fixity, Grammar, Operator, Prec};

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, T: Token> {
    pub op: &'g Operator<T>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

#[derive(Debug)]
struct Shunter<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    grammar: &'g Grammar<T>,
    // The input stream of lexemes, in their original order.
    lexemes: Peekable<I>,
    // Mode::Expr: looking for an expr.
    // Mode::Suffix: looking to extend an existing expr with a suffix.
    mode: Mode,
    // Operators that are still waiting for args. The span is the span of the operator itself.
    op_stack: Vec<(&'g Operator<T>, Span)>,
    // The full spans of the groups currently on the rpn stack.
    spans: Vec<Span>,
    // The left position of the last seen token.
    last_pos: usize,
    this_pos: usize,
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
    pub fn shunt<I>(&self, lexemes: I) -> impl Iterator<Item = Node<T>>
    where
        I: Iterator<Item = Lexeme<T>>,
    {
        Shunter::new(self, lexemes)
    }
}

impl<'g, T, I> Iterator for Shunter<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    type Item = Node<'g, T>;

    fn next(&mut self) -> Option<Node<'g, T>> {
        loop {
            match self.step() {
                Step::Produce(node) => return Some(node),
                Step::Continue => continue,
                Step::Done => return None,
            }
        }
    }
}

enum Step<'g, T: Token> {
    Produce(Node<'g, T>),
    Continue,
    Done,
}

impl<'g, T, I> Shunter<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    fn new(grammar: &'g Grammar<T>, lexemes: I) -> Shunter<'g, T, I> {
        Shunter {
            grammar,
            lexemes: lexemes.peekable(),
            mode: Mode::Expr,
            op_stack: vec![],
            spans: vec![],
            this_pos: 0,
            last_pos: 0,
        }
    }

    fn produce(&mut self, op: &'g Operator<T>, op_span: Span) -> Step<'g, T> {
        use Fixity::*;
        match op.fixity() {
            Nilfix => self.produce_nilfix(op, op_span),
            Prefix(_) => self.produce_prefix(op, op_span),
            Suffix(_) => self.produce_suffix(op, op_span),
            Infix(_, _) => self.produce_infix(op, op_span),
        }
    }

    fn produce_nilfix(&mut self, op: &'g Operator<T>, op_span: Span) -> Step<'g, T> {
        self.spans.push(op_span);
        Step::Produce(Node { op, span: op_span })
    }

    fn produce_prefix(&mut self, op: &'g Operator<T>, op_span: Span) -> Step<'g, T> {
        let arg_span = self.spans.pop().unwrap();
        let span = (op_span.0, arg_span.1);
        self.spans.push(span);
        Step::Produce(Node { op, span })
    }

    fn produce_suffix(&mut self, op: &'g Operator<T>, op_span: Span) -> Step<'g, T> {
        let arg_span = self.spans.pop().unwrap();
        let span = (arg_span.0, op_span.1);
        self.spans.push(span);
        Step::Produce(Node { op, span })
    }

    fn produce_infix(&mut self, op: &'g Operator<T>, _op_span: Span) -> Step<'g, T> {
        let arg_2_span = self.spans.pop().unwrap();
        let arg_1_span = self.spans.pop().unwrap();
        let span = (arg_1_span.0, arg_2_span.1);
        self.spans.push(span);
        Step::Produce(Node { op, span })
    }

    fn step(&mut self) -> Step<'g, T> {
        use Fixity::*;

        let juxt_prec: Prec = self.grammar.juxtapose.left_prec.unwrap();
        if let Some(lexeme) = self.lexemes.peek() {
            let lexeme = *lexeme;
            let op = self.grammar.token_to_op[lexeme.token.as_usize()]
                .as_ref()
                .unwrap_or_else(|| {
                    panic!("No operator provided for token {:?}", lexeme.token);
                });
            self.last_pos = self.this_pos;
            self.this_pos = lexeme.span.1;
            match self.mode {
                Mode::Expr => match op.fixity() {
                    // Rule 1.
                    Nilfix => {
                        self.lexemes.next();
                        self.mode = Mode::Suffix;
                        self.produce_nilfix(op, lexeme.span)
                    }
                    // Rule 2.
                    Prefix(_) => {
                        self.lexemes.next();
                        self.op_stack.push((op, lexeme.span));
                        Step::Continue
                    }
                    // Rule 9, then Rule 1.
                    Suffix(_) | Infix(_, _) => {
                        self.mode = Mode::Suffix;
                        self.produce_nilfix(&self.grammar.missing, (self.last_pos, self.last_pos))
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
                            self.produce(last_op, span)
                        }
                        // Rule 11 or 12, followed by 4.
                        Nilfix | Prefix(_) => {
                            let span = (lexeme.span.0, lexeme.span.0);
                            self.mode = Mode::Expr;
                            self.op_stack.push((&self.grammar.juxtapose, span));
                            Step::Continue
                        }
                        // Rule 3.
                        Infix(l, _) | Suffix(l) if prec <= l => {
                            let (last_op, span) = self.op_stack.pop().unwrap();
                            self.produce(last_op, span)
                        }
                        // Rule 4.
                        Infix(_, _) => {
                            self.lexemes.next();
                            self.op_stack.push((op, lexeme.span));
                            self.mode = Mode::Expr;
                            Step::Continue
                        }
                        // Rule 5.
                        Suffix(_) => {
                            self.lexemes.next();
                            self.produce_suffix(op, lexeme.span)
                        }
                    }
                }
            }
        } else if self.mode == Mode::Expr {
            // Rule 10, then Rule 1.
            self.mode = Mode::Suffix;
            self.produce_nilfix(&self.grammar.missing, (self.last_pos, self.last_pos))
        } else if let Some((op, span)) = self.op_stack.pop() {
            // Rule 8.
            self.produce(op, span)
        } else {
            Step::Done
        }
    }
}
