use super::shunter::{Prec, Rule};
use crate::lexing::{Span, Token};

#[derive(Debug, Clone)]
pub struct OpStack<'g, T: Token> {
    stack: Vec<(&'g Rule<T>, Span, usize)>,
}

pub enum OpStackTop<T: Token> {
    RightPrec(Prec),
    Separator(T),
    Empty,
    FinishedOp,
}

impl<'g, T: Token> OpStack<'g, T> {
    pub fn new() -> OpStack<'g, T> {
        OpStack { stack: vec![] }
    }

    pub fn push(&mut self, rule: &'g Rule<T>, span: Span) {
        debug_assert!(
            rule.right_prec.is_some() || rule.num_holes() > 0,
            "rule push"
        );
        self.stack.push((rule, span, 0));
    }

    pub fn pop(&mut self) -> (&'g Rule<T>, Span) {
        let (rule, span, h) = self.stack.pop().unwrap();
        debug_assert_eq!(h, rule.num_holes(), "rule pop");
        (rule, span)
    }

    pub fn top(&self) -> OpStackTop<T> {
        if let Some((rule, _, h)) = self.stack.last() {
            if *h < rule.num_holes() {
                OpStackTop::Separator(rule.tokens[*h + 1])
            } else if rule.right_prec.is_some() {
                OpStackTop::RightPrec(rule.right_prec.unwrap())
            } else {
                OpStackTop::FinishedOp
            }
        } else {
            OpStackTop::Empty
        }
    }

    pub fn found_sep(&mut self, sep_span: Span) -> Option<(&'g Rule<T>, Span)> {
        let (rule, rule_span, h) = self.stack.pop().unwrap();
        let span = (rule_span.0, sep_span.1);
        if h + 1 == rule.num_holes() && rule.right_prec.is_none() {
            Some((rule, span))
        } else {
            self.stack.push((rule, span, h + 1));
            None
        }
    }

    pub fn missed_sep(&mut self) {
        self.stack.last_mut().unwrap().2 += 1;
    }
}
