use super::grammar::Parser;
use super::visitor::Parsed;
use crate::lexing::Pattern;
use crate::rpn_visitor::Stack as RpnStack;
use crate::shunting::ShuntError;
use std::error::Error;
use std::fmt;

// TODO: Get line&col nums
#[derive(Debug, Clone)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError {
        lexeme: String,
        pos: Position,
    },
    ExtraSeparator {
        separator: String,
        pos: Position,
    },
    MissingSeparator {
        op_name: String,
        separator: String,
        pos: Position,
    },
}

impl Error for ParseError {}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use ParseError::*;

        match self {
            LexError{lexeme, pos} => write!(
                f,
                "Lexing failed. It did not recognize the characters '{}'. Line {} ({}:{})",
                lexeme, pos.line, pos.line, pos.column
            ),
            ExtraSeparator{separator, pos} => write!(
               f,
               "Parsing failed. It did not expect to find '{}' on its own. Line {} ({}:{})",
               separator, pos.line, pos.line, pos.column
            ),
            MissingSeparator{op_name, separator, pos} => write!(
            f,
            "Parsing failed. It expected to find '{}' as part of {}, but could not. Line {} ({}:{})",
            op_name, separator, pos.line, pos.line, pos.column
            ),
        }
    }
}

impl Parser {
    pub fn parse<'s>(&'s self, source: &'s str) -> Result<Parsed<'s>, ParseError> {
        let tokens = self.lexer.lex(source);
        let rpn = self.shunter.shunt(tokens);
        let mut stack = RpnStack::new();
        for node in rpn {
            match node {
                Err(ShuntError::LexError(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let lexeme = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::LexError { lexeme, pos });
                }
                Err(ShuntError::ExtraSep(lexeme)) => {
                    let pos = Position {
                        line: 0,
                        column: lexeme.span.0 + 1,
                    };
                    let separator = source[lexeme.span.0..lexeme.span.1].to_owned();
                    return Err(ParseError::ExtraSeparator { separator, pos });
                }
                Err(ShuntError::MissingSep {
                    op_name,
                    span,
                    token,
                }) => {
                    let pos = Position {
                        line: 0,
                        column: span.0 + 1,
                    };
                    let separator = match self.lexer.get_token_pattern(token).unwrap() {
                        Pattern::Constant(constant) => constant.to_string(),
                        Pattern::Regex { name, .. } => format!("{}", name),
                    };
                    return Err(ParseError::MissingSeparator {
                        op_name,
                        separator,
                        pos,
                    });
                }
                Ok(node) => stack.push(node),
            }
        }
        Ok(Parsed { source, stack })
    }
}
