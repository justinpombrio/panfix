use super::grammar::{Fixity, Grammar, Operator, Prec};
use super::node::{Node, NodeBuilder};
use super::op_stack::{OpStack, OpStackTop};
use crate::{Lexeme, Span, Token};
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
    // Operators that are still waiting for args or seps.
    op_stack: OpStack<'g, T>,
    // Construct nodes, with the correct spans over their children.
    node_builder: NodeBuilder,
    // The right position of the last seen token.
    last_pos: usize,
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
        self.grammar.token_to_op[token.as_usize()].as_ref()
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
        let op = &self.grammar.missing_atom;
        let span = (self.last_pos, self.last_pos);
        self.mode = Mode::Suffix;
        Step::Produce(op, span)
    }

    fn juxtapose(&mut self) -> Step<'g, T> {
        let op = &self.grammar.juxtapose;
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
                    if let Some((op, span)) = self.op_stack.found_sep() {
                        Step::Produce(op, span)
                    } else {
                        self.mode = Mode::Expr;
                        Step::Continue
                    }
                } else {
                    let op = &self.grammar.missing_sep;
                    let span = (self.last_pos, self.last_pos);
                    self.mode = Mode::Suffix;
                    Step::Produce(op, span)
                }
            }
            OpStackTop::Empty => match self.lexemes.next() {
                None => Step::Done,
                Some(lexeme) if lexeme.token == T::LEX_ERROR => {
                    let op = self.lookup_op(T::LEX_ERROR).unwrap();
                    self.halt();
                    Step::Produce(op, lexeme.span)
                }
                Some(lexeme) => {
                    debug_assert!(self.lookup_op(lexeme.token).is_none(), "shunt empty");
                    let op = &self.grammar.extra_sep;
                    self.halt();
                    Step::Produce(op, lexeme.span)
                }
            },
        }
    }

    fn step(&mut self) -> Step<'g, T> {
        use Fixity::*;

        let fixity = match self.lexemes.peek() {
            Some(lexeme) => {
                let token = lexeme.token;
                match self.lookup_op(token) {
                    Some(op) => op.fixity(),
                    None => Suffix(Prec::MAX),
                }
            }
            None => Suffix(Prec::MAX),
        };
        let prec = self.current_prec();
        let juxt_prec: Prec = self.grammar.juxtapose.left_prec.unwrap();
        let this_pos = self.lexemes.peek().map(|lex| lex.span.1);

        let step = match (self.mode, fixity) {
            // Rule 1.
            (Mode::Expr, Nilfix) => self.forward(),
            // Rule 2.
            (Mode::Expr, Prefix(_)) => self.push(),
            // Rule 9, then Rule 1.
            (Mode::Expr, Suffix(_)) => self.missing_atom(),
            // Rule 9, then Rule 1.
            (Mode::Expr, Infix(_, _)) => self.missing_atom(),
            // Rule 11, followed by Rule 3.
            (Mode::Suffix, Nilfix) if prec <= juxt_prec => self.pop(),
            // Rule 11, followed by Rule 4.
            (Mode::Suffix, Nilfix) => self.juxtapose(),
            // Rule 12, followed by Rule 3.
            (Mode::Suffix, Prefix(_)) if prec <= juxt_prec => self.pop(),
            // Rule 12, followed by Rule 4.
            (Mode::Suffix, Prefix(_)) => self.juxtapose(),
            // Rule 5.
            (Mode::Suffix, Suffix(lprec)) if prec <= lprec => self.pop(),
            // Rule 3.
            (Mode::Suffix, Suffix(_)) => self.forward(),
            // Rule 3.
            (Mode::Suffix, Infix(lprec, _)) if prec <= lprec => self.pop(),
            // Rule 4.
            (Mode::Suffix, Infix(_, _)) => self.push(),
        };
        if let Some(pos) = this_pos {
            self.last_pos = pos;
        }
        step
    }
}
