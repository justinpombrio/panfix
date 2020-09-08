use super::rule_stack::{RuleStack, RuleStackTop};
use super::shunter::{Prec, Rule, Shunter};
use crate::lexing::{Lexeme, Span, Token};
use crate::rpn_visitor::Node as NodeTrait;
use std::iter::Peekable;

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, T: Token> {
    pub rule: &'g Rule<T>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

enum Step<'g, T: Token> {
    Produce(&'g Rule<T>, Span),
    Continue,
    Done,
    Error(ShuntError<T>),
}

#[derive(Debug, Clone)]
pub enum ShuntError<T: Token> {
    LexError(Lexeme<T>),
    ExtraSep(Lexeme<T>),
    MissingSep {
        rule_name: String,
        span: Span,
        token: T,
    },
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
    // Rules that are still waiting for args or seps.
    rule_stack: RuleStack<'g, T>,
    // The right position of the last seen token.
    last_pos: usize,
    done: bool,
}

impl<'g, T: Token> Shunter<T> {
    pub fn shunt<I>(&self, lexemes: I) -> impl Iterator<Item = Result<Node<T>, ShuntError<T>>>
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
    type Item = Result<Node<'g, T>, ShuntError<T>>;

    fn next(&mut self) -> Option<Result<Node<'g, T>, ShuntError<T>>> {
        loop {
            if self.done {
                return None;
            }
            match self.step() {
                Step::Produce(rule, span) => {
                    return Some(Ok(Node { rule, span }));
                }
                Step::Continue => continue,
                Step::Done => {
                    self.done = true;
                    return None;
                }
                Step::Error(error) => {
                    self.done = true;
                    return Some(Err(error));
                }
            }
        }
    }
}

impl<'g, T: Token> Node<'g, T> {
    pub fn arity(self) -> usize {
        self.rule.arity()
    }

    pub fn text(self, source: &str) -> &str {
        &source[self.span.0..self.span.1]
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
            rule_stack: RuleStack::new(),
            last_pos: 0,
            done: false,
        }
    }

    fn upcoming_sep(&mut self) -> Option<T> {
        if let Some(lexeme) = self.lexemes.peek() {
            let token = lexeme.token;
            match self.lookup_rule(token) {
                Some(_) => None,
                None => Some(token),
            }
        } else {
            None
        }
    }

    fn current_prec(&self) -> Prec {
        if let RuleStackTop::RightPrec(prec) = self.rule_stack.top() {
            prec
        } else {
            // Using Prec::MAX handles Rule 6 and Rule 7.
            Prec::MAX
        }
    }

    fn lookup_rule(&self, token: T) -> Option<&'g Rule<T>> {
        self.shunter.token_to_rule[token.as_usize()].as_ref()
    }

    fn push(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let rule = self.lookup_rule(lexeme.token).unwrap();
        self.rule_stack.push(rule, lexeme.span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn forward(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let rule = self.lookup_rule(lexeme.token).unwrap();
        self.mode = Mode::Suffix;
        Step::Produce(rule, lexeme.span)
    }

    fn missing_atom(&mut self) -> Step<'g, T> {
        let rule = &self.shunter.missing_atom;
        let span = (self.last_pos, self.last_pos);
        self.mode = Mode::Suffix;
        Step::Produce(rule, span)
    }

    fn juxtapose(&mut self) -> Step<'g, T> {
        let rule = &self.shunter.juxtapose;
        let span = (self.last_pos, self.last_pos);
        self.rule_stack.push(rule, span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn pop(&mut self) -> Step<'g, T> {
        match self.rule_stack.top() {
            RuleStackTop::RightPrec(_) => {
                let (rule, span) = self.rule_stack.pop();
                Step::Produce(rule, span)
            }
            RuleStackTop::Separator(sep) => {
                if self.upcoming_sep() == Some(sep) {
                    let span = self.lexemes.next().unwrap().span;
                    if let Some((rule, span)) = self.rule_stack.found_sep(span) {
                        Step::Produce(rule, span)
                    } else {
                        self.mode = Mode::Expr;
                        Step::Continue
                    }
                } else {
                    let (rule, span) = self.rule_stack.missing_sep();
                    let rule_name = rule.name.to_owned();
                    Step::Error(ShuntError::MissingSep {
                        rule_name,
                        span,
                        token: sep,
                    })
                }
            }
            RuleStackTop::Empty => match self.lexemes.next() {
                None => Step::Done,
                Some(lexeme) if lexeme.token == T::LEX_ERROR => {
                    Step::Error(ShuntError::LexError(lexeme))
                }
                Some(lexeme) => {
                    debug_assert!(self.lookup_rule(lexeme.token).is_none(), "shunt empty");
                    Step::Error(ShuntError::ExtraSep(lexeme))
                }
            },
            RuleStackTop::FinishedOp => {
                let (rule, span) = self.rule_stack.pop();
                Step::Produce(rule, span)
            }
        }
    }

    fn step(&mut self) -> Step<'g, T> {
        let (rule_left_prec, rule_right_prec) = match self.lexemes.peek() {
            Some(lexeme) => {
                let token = lexeme.token;
                match self.lookup_rule(token) {
                    Some(rule) => {
                        if rule.num_holes() > 0 {
                            (rule.left_prec, Some(Prec::MAX))
                        } else {
                            (rule.left_prec, rule.right_prec)
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

        let step = match (self.mode, rule_left_prec, rule_right_prec) {
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

impl<'g, T: Token> NodeTrait for Node<'g, T> {
    fn arity(&self) -> usize {
        Node::arity(*self)
    }
}
