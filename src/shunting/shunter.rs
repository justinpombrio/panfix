use super::grammar::{Grammar, Subgrammar};
use super::op::{Op, OpName, Prec, Token, LEX_ERROR, NT};
use std::iter::Peekable;

pub type Span = (usize, usize);

#[derive(Debug)]
struct Shunter<'g, N, I>
where
    N: OpName,
    I: Iterator<Item = Lexeme>,
{
    grammar: &'g Grammar<N>,
    /// The input stream of lexemes, in their original order.
    lexemes: Peekable<I>,
    /// Mode::Expr: looking for an expr.
    /// Mode::Suffix: looking to extend an existing expr with a suffix.
    /// Mode::Halt: finished, either successfully or with error.
    mode: Mode,
    /// Ops that are still waiting for args or followers.
    op_stack: Vec<OpPart<'g, N>>,
    /// The current subgrammar. This pointer changes.
    subgrammar: &'g Subgrammar<N>,
    /// The left precedence of the juxtaposition operator of the current subgrammar.
    juxt_prec: Prec,
    /// The right position of the last seen token.
    last_pos: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, N: OpName> {
    op: &'g Op<N>,
    span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    /// Looking for an expression.
    Expr,
    /// Looking to extend an expression with a suffix.
    Suffix,
    /// We've finished, either successfully or with error.
    Halt,
}

/// For what do we eagerly await?
#[derive(Debug, Clone, Copy)]
enum Expecting<'g, N: OpName> {
    /// Expecting an expression.
    Expr,
    /// Expecting a suffix of the given precedence.
    Suffix(Prec),
    /// Expecting a follower token.
    Follower(&'g Op<N>, Token),
    /// Fully satisfied, and the op stack is empty.
    Nothing,
}

/// A categorization of the upcoming lexeme.
#[derive(Debug, Clone, Copy)]
enum Upcoming<'g, N: OpName> {
    /// The start of an operator, with the given left&right precedences.
    Op(&'g Op<N>, Option<Prec>, Option<Prec>),
    /// Some other token, which is not an operator.
    Token(Token),
    /// EOS. The lexeme stream has ended.
    End,
}

#[derive(Debug)]
enum Step<'g, N: OpName> {
    Continue,
    Produce(Span, &'g Op<N>),
    Done,
    Error(ShuntError<N>),
}

// TODO: impl Error
#[derive(Debug, Clone)]
pub enum ShuntError<N: OpName> {
    LexError(Lexeme),
    UnexpectedToken(Lexeme),
    MissingFollower(N, Token, Option<Lexeme>),
}

#[derive(Debug)]
enum OpPart<'g, N: OpName> {
    OpArg(Span, &'g Op<N>),
    OpFollower(Span, &'g Op<N>, &'g [(NT, Token)]),
}

impl<'g, N: OpName + 'static> Grammar<N> {
    pub fn shunt(
        &self,
        lexemes: impl Iterator<Item = Lexeme>,
    ) -> impl Iterator<Item = Result<Node<N>, ShuntError<N>>> {
        Shunter::new(self, lexemes)
    }
}

impl<'g, N, I> Iterator for Shunter<'g, N, I>
where
    N: OpName + 'static,
    I: Iterator<Item = Lexeme>,
{
    type Item = Result<Node<'g, N>, ShuntError<N>>;

    fn next(&mut self) -> Option<Result<Node<'g, N>, ShuntError<N>>> {
        loop {
            match self.step() {
                Step::Continue => continue,
                Step::Produce(span, op) => {
                    return Some(Ok(Node { op, span }));
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

impl<'g, N: OpName> Node<'g, N> {
    pub fn op(&self) -> &Op<N> {
        &self.op
    }

    pub fn span(self) -> (usize, usize) {
        self.span
    }
}

impl<'g, N, I> Shunter<'g, N, I>
where
    N: OpName,
    I: Iterator<Item = Lexeme>,
{
    pub fn new(grammar: &'g Grammar<N>, input: I) -> Shunter<'g, N, I> {
        let subgrammar = &grammar.subgrammars[grammar.starting_nonterminal];
        let shunter = Shunter {
            grammar,
            lexemes: input.peekable(),
            mode: Mode::Expr,
            op_stack: vec![],
            subgrammar,
            juxt_prec: subgrammar.juxtapose.left_prec.unwrap(),
            last_pos: 0,
        };
        shunter
    }

    fn lookup_op(&self, token: Token) -> Option<&'g Op<N>> {
        let prefixy_op = self.subgrammar.token_to_prefixy_op[token].as_ref();
        let suffixy_op = self.subgrammar.token_to_suffixy_op[token].as_ref();
        match self.mode {
            Mode::Suffix => suffixy_op.or(prefixy_op),
            Mode::Expr => prefixy_op.or(suffixy_op),
            Mode::Halt => panic!("should not lookup op once halted"),
        }
    }

    fn push(&mut self, op: &'g Op<N>) -> Step<'g, N> {
        let lexeme = self.lexemes.next().unwrap();
        if op.followers.is_empty() {
            self.op_stack.push(OpPart::OpArg(lexeme.span, op));
        } else {
            let nt = op.followers[0].0;
            self.subgrammar = &self.grammar.subgrammars[nt];
            self.juxt_prec = self.subgrammar.juxtapose.left_prec.unwrap();
            self.op_stack
                .push(OpPart::OpFollower(lexeme.span, op, &op.followers));
        };
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn forward(&mut self, op: &'g Op<N>) -> Step<'g, N> {
        let lexeme = self.lexemes.next().unwrap();
        self.mode = Mode::Suffix;
        Step::Produce(lexeme.span, op)
    }

    fn pop(&mut self) -> Step<'g, N> {
        let (span, op) = match self.op_stack.pop().unwrap() {
            OpPart::OpArg(span, op) => (span, op),
            _ => unreachable!(),
        };
        Step::Produce(span, op)
    }

    fn juxtapose(&mut self) -> Step<'g, N> {
        let op = &self.subgrammar.juxtapose;
        let span = (self.last_pos, self.last_pos);
        self.op_stack.push(OpPart::OpArg(span, op));
        self.mode = Mode::Expr;
        Step::Continue
    }

    fn missing_atom(&mut self) -> Step<'g, N> {
        let op = &self.subgrammar.missing_atom;
        let span = (self.last_pos, self.last_pos);
        self.mode = Mode::Suffix;
        Step::Produce(span, op)
    }

    fn missing_follower(&mut self, op: &'g Op<N>, token: Token) -> Step<'g, N> {
        let lexeme = self.lexemes.peek().copied();
        self.mode = Mode::Halt;
        Step::Error(ShuntError::MissingFollower(op.name, token, lexeme))
    }

    fn found_follower(&mut self) -> Step<'g, N> {
        self.lexemes.next();
        let (span, op, followers) = match self.op_stack.pop().unwrap() {
            OpPart::OpFollower(span, op, followers) => (span, op, followers),
            _ => unreachable!(),
        };
        if followers.len() > 1 {
            self.op_stack
                .push(OpPart::OpFollower(span, op, &followers[1..]));
            self.mode = Mode::Expr;
            Step::Continue
        } else if op.right_prec.is_some() {
            self.op_stack.push(OpPart::OpArg(span, op));
            self.mode = Mode::Expr;
            Step::Continue
        } else {
            self.mode = Mode::Suffix;
            Step::Produce(span, op)
        }
    }

    fn step(&mut self) -> Step<'g, N> {
        use Expecting::{Expr, Follower, Nothing, Suffix};
        use Upcoming::{End, Op, Token};

        let this_pos = self.lexemes.peek().map(|lex| lex.span.1);

        let upcoming = if let Some(lexeme) = self.lexemes.peek().copied() {
            if let Some(op) = self.lookup_op(lexeme.token) {
                if op.followers.is_empty() {
                    Op(op, op.left_prec, op.right_prec)
                } else {
                    Op(op, op.left_prec, Some(Prec::MAX))
                }
            } else {
                Token(lexeme.token)
            }
        } else {
            End
        };

        let expecting = match self.mode {
            Mode::Expr => Expr,
            Mode::Suffix => match self.op_stack.last() {
                None => Nothing,
                Some(OpPart::OpArg(_, op)) => Suffix(op.right_prec.unwrap()),
                Some(OpPart::OpFollower(_, op, followers)) => Follower(op, followers[0].1),
            },
            Mode::Halt => return Step::Done,
        };

        let step = match (expecting, upcoming) {
            // Rule 1.
            (Expr, Op(op, None, None)) => self.forward(op),

            // Rule 2.
            (Expr, Op(op, None, Some(_))) => self.push(op),

            // Rule 9, then Rule 1.
            (Expr, Op(_, Some(_), _) | Token(_) | End) => self.missing_atom(),

            // Rule 11 or Rule 12, followed by Rule 3.
            (Suffix(i), Op(_, None, _)) if i <= self.juxt_prec => self.pop(),

            // Rule 11 or Rule 12, followed by Rule 4.
            (Suffix(_) | Nothing, Op(_, None, _)) => self.juxtapose(),

            // Rule 5.
            (Suffix(i), Op(_, Some(j), _)) if i <= j => self.pop(),
            (Suffix(_), Token(_) | End) => self.pop(),

            // Rule 3.
            (Suffix(_) | Follower(_, _) | Nothing, Op(op, Some(_), None)) => self.forward(op),

            // Rule 4.
            (Suffix(_) | Follower(_, _) | Nothing, Op(op, Some(_), Some(_))) => self.push(op),

            // Follower rules
            (Follower(_, tok1), Token(tok2)) if tok1 == tok2 => self.found_follower(),
            (Follower(op, tok), End | Token(_)) => self.missing_follower(op, tok),
            (Follower(_, _), Op(_, _, _)) => unreachable!(),

            // Terminating cases
            (Nothing, End) => {
                self.mode = Mode::Halt;
                Step::Done
            }
            (Nothing, Token(LEX_ERROR)) => {
                let lexeme = self.lexemes.next().unwrap();
                self.mode = Mode::Halt;
                Step::Error(ShuntError::LexError(lexeme))
            }
            (Nothing, Token(_)) => {
                let lexeme = self.lexemes.next().unwrap();
                self.mode = Mode::Halt;
                Step::Error(ShuntError::UnexpectedToken(lexeme))
            }
        };
        if let Some(pos) = this_pos {
            self.last_pos = pos;
        }
        step
    }
}
