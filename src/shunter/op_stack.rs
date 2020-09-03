use super::grammar::{Operator, Prec};
use crate::{Span, Token};

#[derive(Debug, Clone)]
pub struct OpStack<'g, T: Token> {
    stack: Vec<(&'g Operator<T>, Span, usize)>,
}

pub enum OpStackTop<T: Token> {
    RightPrec(Prec),
    Separator(T),
    Empty,
}

impl<'g, T: Token> OpStack<'g, T> {
    pub fn new() -> OpStack<'g, T> {
        OpStack { stack: vec![] }
    }

    pub fn push(&mut self, op: &'g Operator<T>, span: Span) {
        debug_assert!(op.right_prec.is_some() || op.num_holes() > 0, "op push");
        self.stack.push((op, span, 0));
    }

    pub fn pop(&mut self) -> (&'g Operator<T>, Span) {
        let (op, span, h) = self.stack.pop().unwrap();
        debug_assert_eq!(h, op.num_holes(), "op pop");
        (op, span)
    }

    pub fn top(&self) -> OpStackTop<T> {
        if let Some((op, _, h)) = self.stack.last() {
            if *h < op.num_holes() {
                OpStackTop::Separator(op.tokens[*h])
            } else {
                OpStackTop::RightPrec(op.right_prec.unwrap())
            }
        } else {
            OpStackTop::Empty
        }
    }

    pub fn found_sep(&mut self) -> Option<(&'g Operator<T>, Span)> {
        let (op, span, h) = self.stack.pop().unwrap();
        if h + 1 == op.num_holes() && op.right_prec.is_none() {
            Some((op, span))
        } else {
            self.stack.push((op, span, h + 1));
            None
        }
    }
}
