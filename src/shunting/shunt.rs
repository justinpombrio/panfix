use super::grammar::{Grammar, Subgrammar};
use super::op::{Op, Prec};
use super::op_stack::{OpStack, OpStackTop};
use crate::lexing::{Lexeme, Span, Token};
use std::iter::Peekable;

const DEBUG: bool = false;

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, T: Token> {
    pub op: &'g Op<T>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Expr,
    Suffix,
}

enum Step<'g, T: Token> {
    Produce(&'g Op<T>, Span),
    Continue,
    Done,
    Error(ShuntError<T>),
}

#[derive(Debug, Clone)]
pub enum ShuntError<T: Token> {
    LexError(Lexeme<T>),
    ExtraSep(Lexeme<T>),
    MissingSep {
        op_name: String,
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
    grammar: &'g Grammar<T>,
    // The input stream of lexemes, in their original order.
    lexemes: Peekable<I>,
    // Mode::Expr: looking for an expr.
    // Mode::Suffix: looking to extend an existing expr with a suffix.
    mode: Mode,
    // Ops that are still waiting for args or seps.
    op_stack: OpStack<'g, T>,
    // The current subgrammar. Stateful and constantly changing!
    subgrammar: &'g Subgrammar<T>,
    // The right position of the last seen token.
    last_pos: usize,
    done: bool,
}

impl<'g, T: Token> Grammar<T> {
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
        if self.done {
            return None;
        }
        loop {
            match self.step() {
                Step::Continue => continue,
                Step::Produce(op, span) => {
                    return Some(Ok(Node { op, span }));
                }
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
        self.op.arity()
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
    fn new(grammar: &'g Grammar<T>, lexemes: I) -> Shunt<'g, T, I> {
        if DEBUG {
            println!();
            println!("Shunting:");
        }
        Shunt {
            grammar,
            lexemes: lexemes.peekable(),
            mode: Mode::Expr,
            op_stack: OpStack::new(grammar),
            subgrammar: &grammar.subgrammars[grammar.starting_nonterminal as usize],
            last_pos: 0,
            done: false,
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

    fn lookup_op(&self, token: T) -> Option<&'g Op<T>> {
        let index = token.as_usize();
        let prefixy_op = self.subgrammar.token_to_prefixy_op[index].as_ref();
        let suffixy_op = self.subgrammar.token_to_suffixy_op[index].as_ref();
        if self.mode == Mode::Suffix {
            suffixy_op.or(prefixy_op)
        } else {
            prefixy_op.or(suffixy_op)
        }
    }

    fn push(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let op = self.lookup_op(lexeme.token).unwrap();
        if DEBUG {
            println!("  Push    {}", op.name);
        }
        self.op_stack.push(op, lexeme.span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn forward(&mut self) -> Step<'g, T> {
        let lexeme = self.lexemes.next().unwrap();
        let op = self.lookup_op(lexeme.token).unwrap();
        if DEBUG {
            println!("  Forward {}", op.name);
        }
        self.mode = Mode::Suffix;
        Step::Produce(op, lexeme.span)
    }

    fn missing_atom(&mut self) -> Step<'g, T> {
        if DEBUG {
            println!("  Missing");
        }
        let op = &self.subgrammar.missing_atom;
        let span = (self.last_pos, self.last_pos);
        self.mode = Mode::Suffix;
        Step::Produce(op, span)
    }

    fn juxtapose(&mut self) -> Step<'g, T> {
        if DEBUG {
            println!("  Juxt");
        }
        let op = &self.subgrammar.juxtapose;
        let span = (self.last_pos, self.last_pos);
        self.op_stack.push(op, span);
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn pop(&mut self) -> Step<'g, T> {
        if DEBUG {
            println!("  Pop");
        }
        match self.op_stack.top() {
            OpStackTop::RightPrec(_) => {
                let (op, span) = self.op_stack.pop();
                Step::Produce(op, span)
            }
            OpStackTop::Separator(sep) => {
                if self.lexemes.peek().map(|lex| lex.token) == Some(sep) {
                    let span = self.lexemes.next().unwrap().span;
                    if let Some((op, span)) = self.op_stack.found_sep(span) {
                        Step::Produce(op, span)
                    } else {
                        self.mode = Mode::Expr;
                        Step::Continue
                    }
                } else {
                    let (op, span) = self.op_stack.missing_sep();
                    let op_name = op.name.to_owned();
                    Step::Error(ShuntError::MissingSep {
                        op_name,
                        span,
                        token: sep,
                    })
                }
            }
            OpStackTop::Empty => match self.lexemes.next() {
                None => Step::Done,
                Some(lexeme) if lexeme.token == T::LEX_ERROR => {
                    Step::Error(ShuntError::LexError(lexeme))
                }
                Some(lexeme) => {
                    debug_assert!(self.lookup_op(lexeme.token).is_none(), "shunt empty");
                    Step::Error(ShuntError::ExtraSep(lexeme))
                }
            },
            OpStackTop::FinishedOp => {
                let (op, span) = self.op_stack.pop();
                Step::Produce(op, span)
            }
        }
    }

    fn next_token_prec(&mut self) -> (Option<Prec>, Option<Prec>) {
        match self.lexemes.peek() {
            Some(lexeme) => {
                let token = lexeme.token;
                if self.op_stack.top() == OpStackTop::Separator(token) {
                    return (Some(Prec::MAX), None);
                }
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
        }
    }

    fn step(&mut self) -> Step<'g, T> {
        self.subgrammar = self.op_stack.current_subgrammar();
        let (op_left_prec, op_right_prec) = self.next_token_prec();
        let prec = self.current_prec();
        let juxt_prec: Prec = self.subgrammar.juxtapose.left_prec.unwrap();
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
