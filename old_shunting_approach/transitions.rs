use super::grammar::{Grammar, Subgrammar};
use super::op::{NonTerminal, Op, Prec, Token};
use super::shunter::{Action, StateId};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Mode {
    /// Looking for an expression.
    Expr,
    /// Looking to extend an expression with a suffix.
    Suffix,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum OpState<'g> {
    Arg(&'g Op),
    Follower(&'g Op, &'g [(NonTerminal, Token)]),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct State<'g> {
    mode: Mode,
    subgrammar: &'g Subgrammar,
    op_state: Option<OpState<'g>>,
}

struct Compiler<'g> {
    grammar: &'g Grammar,
    states: HashMap<State<'g>, StateId>,
}

/// Look up an operator in the given subgrammar by its token and the current mode.  Returns the
/// operator and its left and right precedence (or None if there is no such operator).
fn lookup_op<'g>(mode: Mode, subgrammar: &'g Subgrammar, token: Token) -> Option<(Option<Prec>, &'g Op, Option<Prec>)> {
    let prefixy_op = subgrammar.token_to_prefixy_op[token].as_ref();
    let suffixy_op = subgrammar.token_to_suffixy_op[token].as_ref();
    let op = match mode {
        Mode::Suffix => suffixy_op.or(prefixy_op),
        Mode::Expr => prefixy_op.or(suffixy_op),
    }?;
    if op.followers.is_empty() {
        Some((op.left_prec, op, op.right_prec))
    } else {
        Some((op.left_prec, op, Some(Prec::MAX)))
    }
}

fn transition<'g>(state: State<'g>, next_token: Option<Token>) -> Action<'g> {
    match state.mode {
        Mode::Expr => {
            if let Some(token) = next_token {
                match lookup_op(state.mode, state.subgrammar, token) {
                    // Rule 1.
                    Some((None, op, None)) => {
                        let new_state = State {
                            mode: Mode::Suffix,
                            subgrammar: state.subgrammar,
                            op_state: state.op_state,
                        };
                        Advance(new_state, op)
                    }
                    // Rule 2.
                    Some((None, op, Some(_))) => {
                        let new_state = State {
                            mode: Mode::Suffix,
                            subgrammar: state.subgrammar,
                            op_state: OpState::Arg(op),
                        };
                        Push(new_state, op)
                    }
                    // Rule 9, then Rule 1.
                    None => PopPhantom(&state.subgrammar.missing_atom)
            } else {
                // Rule 9, then Rule 1.
                PopPhantom(&state.subgrammar.missing_atom)
            }
        }
        Mode::Suffix => {
            match state.op_state {
                // Upcoming::Nothing
                None => (),
                //Upcoming::Suffix(op.right_prec.unwrap())
                Some(OpState::Arg(op)) => (),
                // Upcoming::Follower(op, followers[0].1
                Some(OpState::Follower(op, followers)) => (),
            }
        }
    }

    /*
    pub enum Action<'g> {
        Transition(StateId),
        Advance(StateId, &'g Op),
        Push(StateId, &'g Op),
        PushPhantom(StateId, &'g Op),
        Pop,
        PopPhantom(&'g Op),
        Halt,
    }
    */

    /*
    /// For what do we eagerly await?
    #[derive(Debug, Clone, Copy)]
    enum Expecting<'g> {
        /// Expecting an expression.
        Expr,
        /// Expecting a suffix of the given precedence.
        Suffix(Prec),
        /// Expecting a follower token.
        Follower(&'g Op, Token),
        /// Fully satisfied, and the op stack is empty.
        Nothing,
    }

    let expecting = match state.mode {
        Mode::Expr => Expecting::Expr,
        Mode::Suffix => match state.op_state {
            None => Expecting::Nothing,
            Some(OpState::Arg(op)) => Expecting::Suffix(op.right_prec.unwrap()),
            Some(OpState::Follower(op, followers)) => Expecting::Follower(op, followers[0].1),
        },
    };

    /// A categorization of the upcoming lexeme.
    #[derive(Debug, Clone, Copy)]
    enum Upcoming<'g> {
        /// The start of an operator, with the given left&right precedences.
        Op(&'g Op, Option<Prec>, Option<Prec>),
        /// Some other token, which is not an operator.
        Token(Token),
        /// EOS. The lexeme stream has ended.
        End,
    }

    let upcoming = if let Some(token) = next_token {
        if let Some(op) = lookup_op(state.mode, state.subgrammar, token) {
            if op.followers.is_empty() {
                Upcoming::Op(op, op.left_prec, op.right_prec)
            } else {
                Upcoming::Op(op, op.left_prec, Some(Prec::MAX))
            }
        } else {
            Upcoming::Token(token)
        }
    } else {
        Upcoming::End
    };
    */
}

impl<'g> Compiler<'g> {
    fn new(grammar: &'g Grammar) -> Compiler<'g> {
        Compiler {
            grammar,
            states: HashMap::new(),
        }
    }

    fn compile() -> Vec<Vec<Action<'g>>> {
        unimplemented!();
    }
}
