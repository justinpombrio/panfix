use crate::{Lexeme, Prec};
use std::iter;

/// Convert a token stream into reverse polish notation. For example, `1 * 2 + 3 * 4` would become
/// `1 2 * 3 4 * +`.
///
/// The precedence table is indexed by `Token`, and says what that token's left and right
/// precedence is. Smaller precedence binds tighter.
pub fn shunt<'a, 's: 'a>(
    prec_table: &'a [(Prec, Prec)],
    iter: impl Iterator<Item = Lexeme> + 'a,
) -> impl Iterator<Item = Lexeme> + 'a {
    Shunter {
        prec_table,
        stack: vec![],
        iter: iter.peekable(),
        pop_mode: false,
    }
}

struct Shunter<'a, I>
where
    I: Iterator<Item = Lexeme>,
{
    prec_table: &'a [(Prec, Prec)],
    stack: Vec<Lexeme>,
    iter: iter::Peekable<I>,
    pop_mode: bool,
}

impl<'a, I: Iterator<Item = Lexeme>> Shunter<'a, I> {
    fn top_rprec(&self) -> Prec {
        self.stack
            .last()
            .map(|lex| self.prec_table[lex.token].1)
            .unwrap_or(Prec::MAX)
    }
}

impl<'a, I: Iterator<Item = Lexeme>> Iterator for Shunter<'a, I> {
    type Item = Lexeme;

    fn next(&mut self) -> Option<Lexeme> {
        loop {
            if self.pop_mode {
                let lexeme = self.stack.pop().unwrap();
                let lprec = self.prec_table[lexeme.token].0;
                let rprec = self.top_rprec();
                if rprec > lprec {
                    self.pop_mode = false;
                }
                return Some(lexeme);
            } else if let Some(lexeme) = self.iter.peek().copied() {
                let rprec = self.top_rprec();
                let lprec = self.prec_table[lexeme.token].0;
                if rprec >= lprec {
                    self.stack.push(lexeme);
                    self.iter.next();
                } else {
                    self.pop_mode = true;
                }
            } else {
                return self.stack.pop();
            }
        }
    }
}
