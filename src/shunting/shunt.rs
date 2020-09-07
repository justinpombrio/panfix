use super::node::{Node, NodeBuilder};
use super::rule_stack::{OpStack, OpStackTop};
use super::shunter::{Prec, Rule, Shunter};
use crate::lexing::{Lexeme, Span, Token};
use std::iter::Peekable;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

enum Step<'g, T: Token> {
    Produce(&'g Rule<T>, Span),
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
    // Rules that are still waiting for args or seps.
    rule_stack: OpStack<'g, T>,
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
                Step::Produce(rule, span) => {
                    let node = self.node_builder.build(rule, span);
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
            rule_stack: OpStack::new(),
            node_builder: NodeBuilder::new(),
            last_pos: 0,
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
        if let OpStackTop::RightPrec(prec) = self.rule_stack.top() {
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
            OpStackTop::RightPrec(_) => {
                let (rule, span) = self.rule_stack.pop();
                Step::Produce(rule, span)
            }
            OpStackTop::Separator(sep) => {
                if self.upcoming_sep() == Some(sep) {
                    let span = self.lexemes.next().unwrap().span;
                    if let Some((rule, span)) = self.rule_stack.found_sep(span) {
                        Step::Produce(rule, span)
                    } else {
                        self.mode = Mode::Expr;
                        Step::Continue
                    }
                } else {
                    let rule = &self.shunter.missing_sep;
                    let span = (self.last_pos, self.last_pos);
                    self.rule_stack.missed_sep();
                    self.mode = Mode::Suffix;
                    Step::Produce(rule, span)
                }
            }
            OpStackTop::Empty => match self.lexemes.next() {
                None => Step::Done,
                Some(lexeme) if lexeme.token == T::LEX_ERROR => {
                    let rule = &self.shunter.lex_error;
                    Step::Produce(rule, lexeme.span)
                }
                Some(lexeme) => {
                    debug_assert!(self.lookup_rule(lexeme.token).is_none(), "shunt empty");
                    let rule = &self.shunter.extra_sep;
                    Step::Produce(rule, lexeme.span)
                }
            },
            OpStackTop::FinishedOp => {
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
