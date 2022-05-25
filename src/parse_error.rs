use crate::lexer::{Lexeme, Span};
use std::error;
use std::fmt;
use thiserror::Error;

#[derive(Debug)]
pub struct ParseError<'s> {
    filename: &'s str,
    source: &'s str,
    cause: ParseErrorCause<'s>,
    span: Option<Span>,
}

impl<'s> ParseError<'s> {
    pub fn custom_error(
        filename: &'s str,
        source: &'s str,
        message: &str,
        span: Span,
    ) -> ParseError<'s> {
        ParseError {
            filename,
            source,
            cause: ParseErrorCause::Custom(message.to_owned()),
            span: Some(span),
        }
    }

    pub(crate) fn no_such_sort(filename: &'s str, source: &'s str, sort: &str) -> ParseError<'s> {
        ParseError {
            filename,
            source,
            cause: ParseErrorCause::NoSuchSort(sort.to_owned()),
            span: None,
        }
    }

    pub(crate) fn unexpected_lexeme(
        filename: &'s str,
        source: &'s str,
        lexeme: Lexeme<'s>,
    ) -> ParseError<'s> {
        ParseError {
            filename,
            source,
            cause: ParseErrorCause::UnexpectedLexeme(lexeme),
            span: Some(lexeme.span),
        }
    }

    pub(crate) fn missing_sep(
        filename: &'s str,
        source: &'s str,
        op_name: &str,
        expected: &str,
        found: Lexeme<'s>,
    ) -> ParseError<'s> {
        ParseError {
            filename,
            source,
            cause: ParseErrorCause::MissingSep {
                op_name: op_name.to_owned(),
                expected: expected.to_owned(),
                found,
            },
            span: Some(found.span),
        }
    }

    pub(crate) fn missing_sep_eof(
        filename: &'s str,
        source: &'s str,
        op_name: &str,
        expected: &str,
        span: Span,
    ) -> ParseError<'s> {
        ParseError {
            filename,
            source,
            cause: ParseErrorCause::MissingSepEof {
                op_name: op_name.to_owned(),
                expected: expected.to_owned(),
            },
            span: Some(span),
        }
    }
}

impl<'s> fmt::Display for ParseError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Parse Error: {}", self.cause)?;
        if let Some(span) = self.span {
            if span.start.line == span.end.line {
                writeln!(f, "At '{}' line {}.", self.filename, span.start.line)?;
                writeln!(f)?;
                let line = self.source.lines().nth(span.start.line).unwrap();
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
                    self.filename, span.start.line, span.end.line
                )?;
                writeln!(f)?;
                let line = self.source.lines().nth(span.start.line).unwrap();
                writeln!(f, "{}", line)?;
                for _ in 0..span.start.utf8_col {
                    write!(f, " ")?;
                }
                for _ in 0..(line.chars().count() - span.end.utf8_col) {
                    write!(f, "^")?;
                }
            }
        } else {
            writeln!(f, "{}:", self.filename)?;
        }
        Ok(())
    }
}
impl<'s> error::Error for ParseError<'s> {}

#[derive(Debug, Error)]
enum ParseErrorCause<'s> {
    #[error("{0}")]
    Custom(String),
    #[error("Unexpected token '{0}'.")]
    UnexpectedLexeme(Lexeme<'s>),
    #[error("While parsing '{}', expected '{}' but found '{}'.",
            op_name, expected, found.lexeme)]
    MissingSep {
        op_name: String,
        expected: String,
        found: Lexeme<'s>,
    },
    #[error(
        "While parsing '{}', expected '{}' but found end of file.",
        op_name,
        expected
    )]
    MissingSepEof { op_name: String, expected: String },
    #[error("Sort '{0}' is not defined in the grammar.")]
    NoSuchSort(String),
}
