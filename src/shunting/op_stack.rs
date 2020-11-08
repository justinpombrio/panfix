use super::grammar::{Grammar, Subgrammar};
use super::op::{Op, Prec};
use crate::lexing::{Span, Token};

#[derive(Debug, Clone)]
pub struct OpStack<'g, T: Token> {
    grammar: &'g Grammar<T>,
    starting_subgrammar: &'g Subgrammar<T>,
    stack: Vec<(&'g Op<T>, Span, usize)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum OpStackTop<T: Token> {
    /// The top op is waiting for its rightmost argument, with the given prec.
    RightPrec(Prec),
    /// The top op is waiting for a separator.
    Separator(T),
    /// The top op is done, and has no right prec. (So it's time to pop it.)
    FinishedOp,
    /// The op stack is empty.
    Empty,
}

impl<'g, T: Token> OpStack<'g, T> {
    pub fn new(grammar: &'g Grammar<T>) -> OpStack<'g, T> {
        OpStack {
            grammar,
            starting_subgrammar: &grammar.subgrammars[grammar.starting_nonterminal as usize],
            stack: vec![],
        }
    }

    pub fn current_subgrammar(&self) -> &'g Subgrammar<T> {
        if let Some((op, _, h)) = self.stack.last() {
            if *h < op.num_holes() {
                &self.grammar.subgrammars[op.followers[*h].subgrammar_index as usize]
            } else {
                &self.grammar.subgrammars[op.subgrammar as usize]
            }
        } else {
            self.starting_subgrammar
        }
    }

    pub fn top(&self) -> OpStackTop<T> {
        if let Some((op, _, h)) = self.stack.last() {
            if *h < op.num_holes() {
                OpStackTop::Separator(op.followers[*h].token)
            } else if op.right_prec.is_some() {
                OpStackTop::RightPrec(op.right_prec.unwrap())
            } else {
                OpStackTop::FinishedOp
            }
        } else {
            OpStackTop::Empty
        }
    }

    pub fn push(&mut self, op: &'g Op<T>, span: Span) {
        debug_assert!(op.right_prec.is_some() || op.num_holes() > 0, "op push");
        self.stack.push((op, span, 0));
    }

    pub fn pop(&mut self) -> (&'g Op<T>, Span) {
        let (op, span, h) = self.stack.pop().unwrap();
        debug_assert_eq!(h, op.num_holes(), "op pop");
        (op, span)
    }

    pub fn found_sep(&mut self, sep_span: Span) -> Option<(&'g Op<T>, Span)> {
        let (op, op_span, h) = self.stack.pop().unwrap();
        let span = (op_span.0, sep_span.1);
        if h + 1 == op.num_holes() && op.right_prec.is_none() {
            Some((op, span))
        } else {
            self.stack.push((op, span, h + 1));
            None
        }
    }

    // TODO: Is this behavior right if there are 2 seps, and the first is missing?
    pub fn missing_sep(&mut self) -> (&'g Op<T>, Span) {
        let (op, span, _) = self.stack.pop().unwrap();
        (op, span)
    }
}
