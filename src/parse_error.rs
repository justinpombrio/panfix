use crate::resolver::ResolverError;
use crate::{Source, Span};
use std::error;
use std::fmt;

/// An error while parsing. When Displayed, shows the message and the portion of the source it
/// occurred in.
#[derive(Debug)]
pub struct ParseError<'s> {
    source: &'s Source,
    message: String,
    span: Span,
}

impl<'s> ParseError<'s> {
    /// Construct a custom parsing error message. (This is useful so that you can re-use the
    /// existing parsing error message printing, and because it will have the same type as other
    /// parse errors.
    pub fn custom_error(source: &'s Source, message: &str, span: Span) -> ParseError<'s> {
        ParseError {
            source,
            message: message.to_owned(),
            span,
        }
    }

    pub(crate) fn from_resolver_error(
        source: &'s Source,
        tok_to_name: &[String],
        optok_to_name: &[String],
        error: ResolverError,
    ) -> ParseError<'s> {
        use ResolverError::{IncompleteOp, LexError, UnexpectedToken};

        match error {
            LexError(lexeme) => ParseError {
                source,
                message: "Unrecognized token.".to_owned(),
                span: lexeme.span,
            },
            UnexpectedToken(found) => ParseError {
                source,
                message: format!("Unexpected token '{}'", source.substr(found.span)),
                span: found.span,
            },
            IncompleteOp {
                op,
                expected,
                found: None,
                op_span,
            } => ParseError {
                source,
                message: format!(
                    "While parsing '{}', expected '{}' but found end of file.",
                    &optok_to_name[op], &tok_to_name[expected]
                ),
                span: op_span,
            },
            IncompleteOp {
                op,
                expected,
                found: Some(found),
                op_span: _,
            } => ParseError {
                source,
                message: format!(
                    "While parsing '{}', expected '{}' but found '{}'.",
                    &optok_to_name[op],
                    &tok_to_name[expected],
                    source.substr(found.span)
                ),
                span: found.span,
            },
        }
    }
}

impl<'s> fmt::Display for ParseError<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Parse Error: {}", self.message)?;
        if self.span.start.line == self.span.end.line {
            writeln!(
                f,
                "At '{}' line {}.",
                self.source.filename(),
                self.span.start.line
            )?;
            writeln!(f)?;
            self.source.show_span(f, self.span)
        } else {
            writeln!(
                f,
                "At '{}' lines {}-{}.",
                self.source.filename(),
                self.span.start.line,
                self.span.end.line
            )?;
            self.source.show_span(f, self.span)
        }
    }
}
impl<'s> error::Error for ParseError<'s> {}
