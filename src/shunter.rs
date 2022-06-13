use crate::{Col, Lexeme, Prec};
use std::iter;

/// Convert a token stream into reverse polish notation. For example, `1 * 2 + 3 * 4` would become
/// `1 2 * 3 4 * +`.
///
/// The precedence table is indexed by `Token`, and says what that token's left and right
/// precedence is. Smaller precedence binds tighter.
pub fn shunt<'a, 's: 'a, I>(
    prec_table: &'a Vec<(Prec, Prec)>,
    iter: I,
) -> impl Iterator<Item = Lexeme> + 'a
where
    I: Iterator<Item = Lexeme> + 'a,
{
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
    prec_table: &'a Vec<(Prec, Prec)>,
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
                let top_lexeme = self.stack.pop().unwrap();
                let lprec = self.prec_table[top_lexeme.token].0;
                let rprec = self.top_rprec();
                if rprec > lprec {
                    self.pop_mode = false;
                }
                return Some(top_lexeme);
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

#[test]
fn test_shunting() {
    use crate::{Position, Token, TOKEN_BLANK, TOKEN_ERROR, TOKEN_JUXTAPOSE};

    const TOKEN_ID: Token = 3;
    const TOKEN_TIMES: Token = 4;
    const TOKEN_PLUS: Token = 5;
    const TOKEN_NEG: Token = 6;
    const TOKEN_MINUS: Token = 7;
    const TOKEN_BANG: Token = 8;
    const TOKEN_OPEN: Token = 9;
    const TOKEN_CLOSE: Token = 10;
    const TOKEN_QMARK: Token = 11;
    const TOKEN_COLON: Token = 12;
    const NUM_TOKENS: usize = 13;

    fn lex(src: &str) -> impl Iterator<Item = Lexeme> {
        let mut lexemes = vec![];
        for i in 0..src.len() {
            let ch = src[i..i + 1].chars().next().unwrap();
            if ch == ' ' {
                continue;
            }
            let token = match ch {
                '_' => TOKEN_BLANK,
                '.' => TOKEN_JUXTAPOSE,
                'a'..='z' => TOKEN_ID,
                '*' => TOKEN_TIMES,
                '+' => TOKEN_PLUS,
                '~' => TOKEN_NEG,
                '-' => TOKEN_MINUS,
                '!' => TOKEN_BANG,
                '(' => TOKEN_OPEN,
                ')' => TOKEN_CLOSE,
                '?' => TOKEN_QMARK,
                ':' => TOKEN_COLON,
                _ => TOKEN_ERROR,
            };
            let start = Position {
                line: 0,
                col: i as Col,
                utf8_col: i as Col,
            };
            let end = Position {
                line: 0,
                col: (i + 1) as Col,
                utf8_col: (i + 1) as Col,
            };
            lexemes.push(Lexeme::new(token, start, end));
        }
        lexemes.into_iter()
    }

    fn show_stream<'s>(src: &str, stream: &mut impl Iterator<Item = Lexeme>) -> String {
        stream
            .map(|lex| &src[lex.span.start.col as usize..lex.span.end.col as usize])
            .map(|lex| if lex == "" { "_" } else { lex })
            .collect::<Vec<_>>()
            .join(" ")
    }

    let mut prec_table = Vec::new();
    for _ in 0..NUM_TOKENS {
        prec_table.push((0, 0));
    }
    prec_table[TOKEN_ERROR] = (0, 0);
    prec_table[TOKEN_BLANK] = (0, 0);
    prec_table[TOKEN_JUXTAPOSE] = (10, 10);
    prec_table[TOKEN_ID] = (0, 0);
    prec_table[TOKEN_BANG] = (50, 0);
    prec_table[TOKEN_TIMES] = (60, 60);
    prec_table[TOKEN_PLUS] = (100, 99);
    prec_table[TOKEN_MINUS] = (100, 99);
    prec_table[TOKEN_NEG] = (0, 80);
    prec_table[TOKEN_OPEN] = (0, Prec::MAX);
    prec_table[TOKEN_CLOSE] = (Prec::MAX, 0);
    prec_table[TOKEN_QMARK] = (80, Prec::MAX);
    prec_table[TOKEN_COLON] = (Prec::MAX, 80);

    let mut link_table = Vec::new();
    for _ in 0..NUM_TOKENS {
        let mut inner_table = Vec::new();
        for _ in 0..NUM_TOKENS {
            inner_table.push(false);
        }
        link_table.push(inner_table);
    }

    let src = "_";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_");

    let src = "_+_";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_ _ +");

    let src = "1-2+3*4*5!-~6";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 2 - 3 4 5 ! * * + 6 ~ -");

    let src = "(~_)";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "_ ~ ) (");

    let src = "%";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "%");

    let src = "1 + %";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 % +");

    let src = "1 ? 2 : 3";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "1 2 3 : ?");

    // TODO: This is not great behavior
    let src = "( 2 : 3";
    let lexemes = &mut shunt(&prec_table, lex(src));
    assert_eq!(show_stream(src, lexemes), "2 3 : (");
}
