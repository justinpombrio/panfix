use crate::resolver::ResolverError;
use crate::{Lexeme, OpTokenInfo, Source, Span, TokenInfo};
use std::error;
use std::fmt;
use thiserror::Error;

#[derive(Debug)]
pub struct ParseError<'s> {
    source: &'s Source,
    cause: ParseErrorCause,
    span: Option<Span>,
}

#[derive(Debug, Error)]
enum ParseErrorCause {
    #[error("{0}")]
    Custom(String),
    #[error("Unexpected token '{0}'.")]
    UnexpectedLexeme(String),
    #[error(
        "While parsing '{}', expected '{}' but found '{}'.",
        op_name,
        expected,
        found
    )]
    MissingSep {
        op_name: String,
        expected: String,
        found: String,
    },
    #[error(
        "While parsing '{}', expected '{}' but found end of file.",
        op_name,
        expected
    )]
    MissingSepEof { op_name: String, expected: String },
}

impl<'s> ParseError<'s> {
    pub fn custom_error(source: &'s Source, message: &str, span: Span) -> ParseError<'s> {
        ParseError {
            source,
            cause: ParseErrorCause::Custom(message.to_owned()),
            span: Some(span),
        }
    }

    pub(crate) fn from_resolver_error(
        source: &'s Source,
        token_table: &[TokenInfo],
        op_token_table: &[OpTokenInfo],
        error: ResolverError,
    ) -> ParseError<'s> {
        use ResolverError::{UnexpectedEof, UnexpectedToken, WrongToken};

        match error {
            UnexpectedEof { op, expected } => ParseError {
                source,
                cause: ParseErrorCause::MissingSepEof {
                    op_name: op_token_table[op].name.clone(),
                    expected: token_table[expected].name.clone(),
                },
                span: Some(Span::new_at_pos(source.end_of_file())),
            },
            WrongToken {
                op,
                expected,
                found,
            } => ParseError {
                source,
                cause: ParseErrorCause::MissingSep {
                    op_name: op_token_table[op].name.clone(),
                    expected: token_table[expected].name.clone(),
                    found: source.substr(found.span).to_owned(),
                },
                span: Some(found.span),
            },
            UnexpectedToken { found } => ParseError {
                source,
                cause: ParseErrorCause::UnexpectedLexeme(source.substr(found.span).to_owned()),
                span: Some(found.span),
            },
        }
    }

    /*
    pub(crate) fn unexpected_lexeme(source: &'s Source, lexeme: Lexeme) -> ParseError<'s> {
        ParseError {
            source,
            cause: ParseErrorCause::UnexpectedLexeme(source.substr(lexeme.span).to_owned()),
            span: Some(lexeme.span),
        }
    }
    */
}

impl<'s> fmt::Display for ParseError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Parse Error: {}", self.cause)?;
        if let Some(span) = self.span {
            if span.start.line == span.end.line {
                writeln!(
                    f,
                    "At '{}' line {}.",
                    self.source.filename(),
                    span.start.line
                )?;
                writeln!(f)?;
                let line = self.source.line_contents(span.start.line);
                writeln!(f, "{}", line)?;
                for _ in 0..span.start.utf8_col {
                    write!(f, " ")?;
                }
                let len = (span.end.utf8_col - span.start.utf8_col).max(1);
                for _ in 0..len {
                    write!(f, "^")?;
                }
            } else {
                writeln!(
                    f,
                    "At '{}' lines {}-{}.",
                    self.source.filename(),
                    span.start.line,
                    span.end.line
                )?;
                writeln!(f)?;
                let line = self.source.line_contents(span.start.line);
                writeln!(f, "{}", line)?;
                for _ in 0..span.start.utf8_col {
                    write!(f, " ")?;
                }
                for _ in 0..(line.chars().count() - span.start.utf8_col as usize) {
                    write!(f, "^")?;
                }
            }
        } else {
            writeln!(f, "{}:", self.source.filename())?;
        }
        Ok(())
    }
}
impl<'s> error::Error for ParseError<'s> {}
