use crate::{Lexeme, Position, Token, TOKEN_BLANK, TOKEN_JUXTAPOSE};
use std::iter;

pub fn insert_blanks(
    prefixy_tokens: &[Option<(Token, bool)>],
    suffixy_tokens: &[Option<(Token, bool)>],
    lexemes: Vec<Lexeme>,
) -> Vec<Lexeme> {
    let mut arg_mode = true;
    let mut last_pos = Position::start_of_file();
    let mut output = vec![];
    for lexeme in lexemes {
        let (preferred_table, unpreferred_table, fake_token) = if arg_mode {
            (prefixy_tokens, suffixy_tokens, TOKEN_BLANK)
        } else {
            (suffixy_tokens, prefixy_tokens, TOKEN_JUXTAPOSE)
        };
        if let Some((optok, has_arg)) = preferred_table[lexeme.token] {
            arg_mode = has_arg;
            output.push(Lexeme {
                token: optok,
                ..lexeme
            });
        } else if let Some((optok, has_arg)) = unpreferred_table[lexeme.token] {
            output.push(Lexeme::new(fake_token, last_pos, last_pos));
            arg_mode = has_arg;
            output.push(Lexeme {
                token: optok,
                ..lexeme
            });
        } else {
            panic!("insert_blanks: invalid token tables at {}", lexeme.token);
        }
        last_pos = lexeme.span.end;
    }
    if arg_mode {
        output.push(Lexeme::new(TOKEN_BLANK, last_pos, last_pos));
    }
    output
}
