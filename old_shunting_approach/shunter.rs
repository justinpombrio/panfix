use super::op::{Op, Token};
use crate::lexing::{Lexeme, Position};
use std::iter;
use thiserror::Error;

// TODO
const EOF: Token = 123000;

pub type StateId = usize;

/// StateId -> Token -> Action
pub struct TransitionTable<'g>(Vec<Vec<Action<'g>>>);

// Basically a pushdown automaton, but specialized for shunting
pub struct Shunter<'s, 'g, I: Iterator<Item = Lexeme<'s>>> {
    lexemes: iter::Peekable<I>,
    /// StateId -> Token -> Action
    transition_table: TransitionTable<'g>,
    stack: Vec<(StateId, &'g Op, Position, Position)>,
    /// The right position of the last seen token.
    last_pos: Position,
    halted: bool,
}

// TODO: Make these errors better
#[derive(Debug, Clone, Error)]
pub enum ShuntError<'s, 'g> {
    #[error("Failed to lex {0:?}")]
    LexError(Lexeme<'s>),
    #[error("Unexpected lexeme {0:?}")]
    UnexpectedToken(Lexeme<'s>),
    #[error("Operator {op} expected token {expected}, but found {found:?}")]
    MissingFollower {
        op: &'g Op,
        expected: Token,
        found: Option<Lexeme<'s>>,
    },
}

#[derive(Debug, Clone, Copy)]
pub enum ShuntErrorBase<'g> {
    LexError,
    UnexpectedToken,
    MissingFollower { op: &'g Op, expected: Token },
}

impl<'g> ShuntErrorBase<'g> {
    fn into_error<'s>(self, lexeme: Option<Lexeme<'s>>) -> ShuntError<'s, 'g> {
        match self {
            ShuntErrorBase::LexError => ShuntError::LexError(lexeme.unwrap()),
            ShuntErrorBase::UnexpectedToken => ShuntError::UnexpectedToken(lexeme.unwrap()),
            ShuntErrorBase::MissingFollower { op, expected } => ShuntError::MissingFollower {
                op,
                expected,
                found: lexeme,
            },
        }
    }
}

#[derive(Debug)]
enum Step<'s, 'g> {
    Continue,
    Produce(&'g Op, Position, Position),
    Done,
    Error(ShuntError<'s, 'g>),
}

#[derive(Debug, Clone, Copy)]
pub enum Action<'g> {
    Transition(StateId),
    Advance(StateId, &'g Op),
    Push(StateId, &'g Op),
    PushPhantom(StateId, &'g Op),
    Pop,
    PopPhantom(&'g Op),
    HaltOk,
    HaltError(ShuntErrorBase<'g>),
}

impl<'g> TransitionTable<'g> {
    pub fn lookup(&self, state: StateId, token: Token) -> Action<'g> {
        self.0[state][token]
    }
}

impl<'s, 'g, I: Iterator<Item = Lexeme<'s>>> Shunter<'s, 'g, I> {
    fn perform_action(&mut self, action: Action<'g>) -> Step<'s, 'g> {
        use Action::*;
        use Step::*;

        let this_pos = self.lexemes.peek().map(|lex| lex.end);
        let step = match action {
            Transition(state) => {
                self.stack.last_mut().unwrap().0 = state;
                Continue
            }
            Advance(state, op) => {
                self.stack.last_mut().unwrap().0 = state;
                let lexeme = self.lexemes.next().unwrap();
                Produce(op, lexeme.start, lexeme.end)
            }
            Push(state, op) => {
                let lexeme = self.lexemes.next().unwrap();
                self.stack.push((state, op, lexeme.start, lexeme.end));
                Continue
            }
            PushPhantom(state, op) => {
                self.stack.push((state, op, self.last_pos, self.last_pos));
                Continue
            }
            Pop => {
                let (_, op, start, end) = self.stack.pop().unwrap();
                Produce(op, start, end)
            }
            PopPhantom(op) => Produce(op, self.last_pos, self.last_pos),
            HaltOk => {
                self.halted = true;
                Done
            }
            HaltError(error) => {
                self.halted = true;
                let lexeme = self.lexemes.next();
                Error(error.into_error(lexeme))
            }
        };
        if let Some(pos) = this_pos {
            self.last_pos = pos;
        }
        step
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Node<'g> {
    op: &'g Op,
    start: Position,
    end: Position,
}

impl<'s, 'g, I> Iterator for Shunter<'s, 'g, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    type Item = Result<Node<'g>, ShuntError<'s, 'g>>;

    fn next(&mut self) -> Option<Result<Node<'g>, ShuntError<'s, 'g>>> {
        loop {
            if self.halted {
                return None;
            }

            let token = self.lexemes.peek().map(|lex| lex.token).unwrap_or(EOF);
            let state = self.stack.last().unwrap().0;
            let action = self.transition_table.lookup(state, token);
            match self.perform_action(action) {
                Step::Continue => continue,
                Step::Produce(op, start, end) => {
                    return Some(Ok(Node { op, start, end }));
                }
                Step::Done => {
                    return None;
                }
                Step::Error(error) => {
                    return Some(Err(error));
                }
            }
        }
    }
}
