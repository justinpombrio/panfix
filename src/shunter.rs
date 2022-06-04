use crate::{Lexeme, Prec};
use std::iter;

pub struct Shunter {
    pub prec_table: Vec<(Prec, Prec)>,
}

impl Shunter {
    pub fn shunt<'a, 's: 'a, I>(&'a self, iter: I) -> impl Iterator<Item = Lexeme<'s>> + 'a
    where
        I: Iterator<Item = Lexeme<'s>> + 'a,
    {
        ShuntingIter {
            prec_table: &self.prec_table,
            stack: vec![],
            iter: iter.peekable(),
        }
    }
}

struct ShuntingIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    prec_table: &'a Vec<(Prec, Prec)>,
    stack: Vec<Lexeme<'s>>,
    iter: iter::Peekable<I>,
}

impl<'a, 's, I> Iterator for ShuntingIter<'a, 's, I>
where
    I: Iterator<Item = Lexeme<'s>>,
{
    type Item = Lexeme<'s>;

    fn next(&mut self) -> Option<Lexeme<'s>> {
        loop {
            // TODO: debugging
            print!("shunting:");
            for item in &self.stack {
                print!(" {}", item.lexeme);
            }
            if let Some(lexeme) = self.iter.peek() {
                println!(" | {}", lexeme.lexeme);
            }

            let top = self.stack.last().copied();
            let rprec = top.map(|lex| self.prec_table[lex.token].1);

            let next = self.iter.peek().copied();
            let lprec = next.map(|lex| self.prec_table[lex.token].0);

            if rprec == None && lprec == None {
                return None;
            } else if rprec.unwrap_or(Prec::MAX) >= lprec.unwrap_or(Prec::MAX) {
                self.stack.push(self.iter.next().unwrap());
            } else {
                return Some(self.stack.pop().unwrap());
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
    const NUM_TOKENS: usize = 11;

    fn lex<'s>(src: &'s str) -> impl Iterator<Item = Lexeme<'s>> {
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
                _ => TOKEN_ERROR,
            };
            let pos = Position::start(); // we don't care about positions in this test
            lexemes.push(Lexeme::new(token, &src[i..i + 1], pos, pos));
        }
        lexemes.into_iter()
    }

    fn show_stream<'s>(stream: &mut impl Iterator<Item = Lexeme<'s>>) -> String {
        stream
            .map(|lex| if lex.lexeme == "" { "_" } else { lex.lexeme })
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
    prec_table[TOKEN_OPEN] = (0, 1000);
    prec_table[TOKEN_CLOSE] = (1000, 0);
    let shunter = Shunter { prec_table };

    let src = "_";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "_");

    let src = "_+_";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "_ _ +");

    let src = "1-2+3*4*5!-~6";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "1 2 - 3 4 5 ! * * + 6 ~ -");

    let src = "(~_)";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "_ ~ ) (");

    let src = "%";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "%");

    let src = "1 + %";
    let lexemes = &mut shunter.shunt(lex(src));
    assert_eq!(show_stream(lexemes), "1 % +");
}
