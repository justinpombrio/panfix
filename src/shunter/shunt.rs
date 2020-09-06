use super::node::{Node, NodeBuilder};
use super::op_stack::{OpStack, OpStackTop};
use super::shunter::{Operator, Prec, Shunter};
use crate::lexer::{Lexeme, Span, Token};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

enum Step<'g, T: Token> {
    Produce(&'g Operator<T>, Span),
    Continue,
    Done,
}

#[derive(Debug)]
struct Shunt<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    shunter: &'g Shunter<T>,
    // The input stream of lexemes, in their original order.
    lexemes: Peekable<I>,
    // Mode::Expr: looking for an expr.
    // Mode::Suffix: looking to extend an existing expr with a suffix.
    mode: Mode,
    // Operators that are still waiting for args or seps.
    op_stack: OpStack<'g, T>,
    // Construct nodes, with the correct spans over their children.
    node_builder: NodeBuilder,
    // The right position of the last seen token.
    last_pos: usize,
}

impl<'g, T: Token> Shunter<T> {
    pub fn shunt<I>(&self, lexemes: I) -> impl Iterator<Item = Node<T>>
    where
        I: Iterator<Item = Lexeme<T>>,
    {
        Shunt::new(self, lexemes)
    }
}

impl<'g, T, I> Iterator for Shunt<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    type Item = Node<'g, T>;

    fn next(&mut self) -> Option<Node<'g, T>> {
        loop {
            match self.step() {
                Step::Produce(op, span) => {
                    let node = self.node_builder.build(op, span);
                    return Some(node);
                }
                Step::Continue => continue,
                Step::Done => return None,
            }
        }
    }
}

impl<'g, T, I> Shunt<'g, T, I>
where
    T: Token,
    I: Iterator<Item = Lexeme<T>>,
{
    fn new(shunter: &'g Shunter<T>, lexemes: I) -> Shunt<'g, T, I> {
        Shunt {
            shunter,
            lexemes: lexemes.peekable(),
            mode: Mode::Expr,
            op_stack: OpStack::new(),
            node_builder: NodeBuilder::new(),
            last_pos: 0,
        }
    }

    fn upcoming_sep(&mut self) -> Option<T> {
        if let Some(lexeme) = self.lexemes.peek() {
            let token = lexeme.token;
            match self.lookup_op(token) {
                Some(_) => None,
                None => Some(token),
            }
        } else {
            None
        }
    }

    fn current_prec(&self) -> Prec {
        if let OpStackTop::RightPrec(prec) = self.op_stack.top() {
            prec
        } else {
            // Using Prec::MAX handles Rule 6 and Rule 7.
            Prec::MAX
        }
    }

    fn lookup_op(&self, token: T) -> Option<&'g Operator<T>> {
        self.shunter.token_to_op[token.as_usize()].as_ref()
    }

    fn halt(&mut self) {
        while let Some(_) = self.lexemes.next() {}
    }

    fn push(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let op = self.lookup_op(lexeme.token).unwrap();
        self.op_stack.push(op, lexeme.span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn forward(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let op = self.lookup_op(lexeme.token).unwrap();
        self.mode = Mode::Suffix;
        Step::Produce(op, lexeme.span)
    }

    fn missing_atom(&mut self) -> Step<'g, T> {
        let op = &self.shunter.missing_atom;
        let span = (self.last_pos, self.last_pos);
        self.mode = Mode::Suffix;
        Step::Produce(op, span)
    }

    fn juxtapose(&mut self) -> Step<'g, T> {
        let op = &self.shunter.juxtapose;
        let span = (self.last_pos, self.last_pos);
        self.op_stack.push(op, span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn pop(&mut self) -> Step<'g, T> {
        match self.op_stack.top() {
            OpStackTop::RightPrec(_) => {
                let (op, span) = self.op_stack.pop();
                Step::Produce(op, span)
            }
            OpStackTop::Separator(sep) => {
                if self.upcoming_sep() == Some(sep) {
                    let span = self.lexemes.next().unwrap().span;
                    if let Some((op, span)) = self.op_stack.found_sep(span) {
                        Step::Produce(op, span)
                    } else {
                        self.mode = Mode::Expr;
                        Step::Continue
                    }
                } else {
                    let op = &self.shunter.missing_sep;
                    let span = (self.last_pos, self.last_pos);
                    self.op_stack.missed_sep();
                    self.mode = Mode::Suffix;
                    Step::Produce(op, span)
                }
            }
            OpStackTop::Empty => match self.lexemes.next() {
                None => Step::Done,
                Some(lexeme) if lexeme.token == T::LEX_ERROR => {
                    let op = &self.shunter.lex_error;
                    self.halt();
                    Step::Produce(op, lexeme.span)
                }
                Some(lexeme) => {
                    debug_assert!(self.lookup_op(lexeme.token).is_none(), "shunt empty");
                    let op = &self.shunter.extra_sep;
                    self.halt();
                    Step::Produce(op, lexeme.span)
                }
            },
            OpStackTop::FinishedOp => {
                let (op, span) = self.op_stack.pop();
                Step::Produce(op, span)
            }
        }
    }

    fn step(&mut self) -> Step<'g, T> {
        let (op_left_prec, op_right_prec) = match self.lexemes.peek() {
            Some(lexeme) => {
                let token = lexeme.token;
                match self.lookup_op(token) {
                    Some(op) => {
                        if op.num_holes() > 0 {
                            (op.left_prec, Some(Prec::MAX))
                        } else {
                            (op.left_prec, op.right_prec)
                        }
                    }
                    None => (Some(Prec::MAX), None),
                }
            }
            None => (Some(Prec::MAX), None),
        };
        let prec = self.current_prec();
        let juxt_prec: Prec = self.shunter.juxtapose.left_prec.unwrap();
        let this_pos = self.lexemes.peek().map(|lex| lex.span.1);

        let step = match (self.mode, op_left_prec, op_right_prec) {
            // Rule 1.
            (Mode::Expr, None, None) => self.forward(),
            // Rule 2.
            (Mode::Expr, None, Some(_)) => self.push(),
            // Rule 9, then Rule 1.
            (Mode::Expr, Some(_), _) => self.missing_atom(),
            // Rule 11 or Rule 12, followed by Rule 3.
            (Mode::Suffix, None, _) if prec <= juxt_prec => self.pop(),
            // Rule 11 or Rule 12, followed by Rule 4.
            (Mode::Suffix, None, _) => self.juxtapose(),
            // Rule 5.
            (Mode::Suffix, Some(lprec), _) if prec <= lprec => self.pop(),
            // Rule 3.
            (Mode::Suffix, Some(_), None) => self.forward(),
            // Rule 4.
            (Mode::Suffix, Some(_), Some(_)) => self.push(),
        };
        if let Some(pos) = this_pos {
            self.last_pos = pos;
        }
        step
    }
}
