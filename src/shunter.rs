use crate::{Col, Lexeme, Prec};
use std::iter;

/// Convert a token stream into reverse polish notation. For example, `1 * 2 + 3 * 4` would become
/// `1 2 * 3 4 * +`.
///
/// The precedence table is indexed by `Token`, and says what that token's left and right
/// precedence is. Smaller precedence binds tighter.
pub fn shunt(prec_table: &[(Prec, Prec)], lexemes: Vec<Lexeme>) -> Vec<Lexeme> {
    let mut stack = Vec::<Lexeme>::new();
    let top_rprec = |stack: &[Lexeme]| -> Prec {
        stack
            .last()
            .copied()
            .map(|lex| prec_table[lex.token].1)
            .unwrap_or(Prec::MAX)
    };
    let mut output = Vec::<Lexeme>::new();
    for lexeme in lexemes {
        loop {
            let rprec = top_rprec(&stack);
            let lprec = prec_table[lexeme.token].0;
            if rprec >= lprec {
                stack.push(lexeme);
                break;
            } else {
                while let Some(top_lexeme) = stack.pop() {
                    let lprec = prec_table[top_lexeme.token].0;
                    let rprec = top_rprec(&stack);
                    output.push(top_lexeme);
                    if rprec > lprec {
                        break;
                    }
                }
            }
        }
    }
    while let Some(lexeme) = stack.pop() {
        output.push(lexeme);
    }
    output
}
