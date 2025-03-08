use crate::resolver::ResolverError;
use crate::{Source, Span, Token};
use std::error;
use std::fmt;
use std::fmt::Write;

/// An error while parsing. When Displayed, shows the message and the portion of the source it
/// occurred in.
#[derive(Debug)]
pub struct ParseError<'s> {
    pub source: &'s Source,
    pub short_message: String,
    pub message: String,
    pub span: Span,
}

impl<'s> ParseError<'s> {
    /// Construct a custom parsing error message. (This is useful so that you can re-use the
    /// existing parsing error message printing, and because it will have the same type as other
    /// parse errors.
    pub fn custom_error(
        source: &'s Source,
        short_message: &str,
        message: &str,
        span: Span,
    ) -> ParseError<'s> {
        ParseError {
            source,
            short_message: short_message.to_owned(),
            message: message.to_owned(),
            span,
        }
    }

    pub(crate) fn from_resolver_error<T: Token>(
        source: &'s Source,
        tok_to_name: &[String],
        optok_to_token: &[T],
        error: ResolverError,
    ) -> ParseError<'s> {
        use ResolverError::{IncompleteOp, LexError, UnexpectedToken};

        match error {
            LexError(lexeme) => ParseError {
                source,
                short_message: "unrecognized token".to_owned(),
                message: "Unrecognized token.".to_owned(),
                span: lexeme.span,
            },
            UnexpectedToken(found) => ParseError {
                source,
                short_message: "unexpected token".to_owned(),
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
                short_message: format!("expected {}", &optok_to_token[op]),
                message: format!(
                    "While parsing '{}', expected '{}' but found end of file.",
                    &optok_to_token[op], &tok_to_name[expected]
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
                short_message: format!("expected {}", &optok_to_token[op]),
                message: format!(
                    "While parsing '{}', expected '{}' but found '{}'.",
                    &optok_to_token[op],
                    &tok_to_name[expected],
                    source.substr(found.span)
                ),
                span: found.span,
            },
        }
    }

    /// The regular `fmt::Display` implementation attempts to infer whether to print with color.
    /// Use this method to manually set whether to print with color.
    pub fn display_with_color_override(&self, use_color: bool) -> impl fmt::Display + '_ {
        /// Wrapper struct for recording whether to print with color.
        struct DisplayParseError<'a> {
            error: &'a ParseError<'a>,
            use_color: bool,
        }

        impl fmt::Display for DisplayParseError<'_> {
            fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
                colored::control::set_override(self.use_color);
                write!(f, "{}", self.error)?;
                colored::control::unset_override();
                Ok(())
            }
        }

        DisplayParseError {
            error: self,
            use_color,
        }
    }
}

impl fmt::Display for ParseError<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        show_error_with_loc(
            f,
            self.source,
            "Parse Error",
            self.span,
            &self.short_message,
            &self.message,
        )
    }
}

impl error::Error for ParseError<'_> {}

struct LineInfo<'a> {
    num: String,
    contents: &'a str,
    carets_start: usize,
    carets_len: usize,
}

fn show_error_with_loc(
    buffer: &mut impl Write,
    source: &Source,
    error_kind: &str,
    span: Span,
    short_message: &str,
    long_message: &str,
) -> fmt::Result {
    use colored::Colorize;

    let start = span.start;
    let end = span.end;
    let (first_line, last_line, margin, ellided) = {
        if start.line == end.line {
            let first_line = LineInfo {
                num: format!("{}", start.line + 1),
                contents: source.line_contents(start.line),
                carets_start: start.col as usize,
                carets_len: (end.col - start.col).max(1) as usize,
            };
            let margin = first_line.num.len() + 1;
            (first_line, None, margin, false)
        } else {
            let first_line = LineInfo {
                num: format!("{}", start.line + 1),
                contents: source.line_contents(start.line),
                carets_start: start.col as usize,
                carets_len: source.line_contents(start.line).chars().count() - start.col as usize,
            };
            let last_line = LineInfo {
                num: format!("{}", end.line + 1),
                contents: source.line_contents(end.line),
                carets_start: 0,
                carets_len: end.col.max(1) as usize,
            };
            let margin = first_line.num.len().max(last_line.num.len()) + 1;
            let ellided = end.line - start.line >= 2;
            (first_line, Some(last_line), margin, ellided)
        }
    };

    writeln!(
        buffer,
        "{}{} {}",
        error_kind.red().bold(),
        ":".bold(),
        long_message.bold(),
    )?;

    writeln!(
        buffer,
        "{:margin$}{} {}:{}:{}",
        "",
        "-->".blue().bold(),
        source.filename(),
        start.line + 1,
        start.col + 1,
        margin = margin - 1,
    )?;

    if let Some(last_line) = last_line {
        show_line(buffer, margin, "", "")?;
        show_line(buffer, margin, &first_line.num, first_line.contents)?;
        let carets = show_carets(first_line.carets_start, first_line.carets_len, "");
        show_line(buffer, margin, "", &carets)?;
        if ellided {
            writeln!(buffer, "{}", "...".blue().bold())?;
        }
        show_line(buffer, margin, &last_line.num, last_line.contents)?;
        let carets = show_carets(last_line.carets_start, last_line.carets_len, short_message);
        show_line(buffer, margin, "", &carets)
    } else {
        show_line(buffer, margin, "", "")?;
        show_line(buffer, margin, &first_line.num, first_line.contents)?;
        let carets = show_carets(
            first_line.carets_start,
            first_line.carets_len,
            short_message,
        );
        show_line(buffer, margin, "", &carets)
    }
}

fn show_carets(carets_start: usize, carets_len: usize, short_message: &str) -> String {
    use colored::Colorize;

    if short_message.is_empty() {
        format!(
            "{:padding$}{}",
            "",
            "^".repeat(carets_len).red().bold(),
            padding = carets_start
        )
    } else {
        format!(
            "{:padding$}{} {}",
            "",
            "^".repeat(carets_len).red().bold(),
            short_message,
            padding = carets_start
        )
    }
}

fn show_line(
    buffer: &mut impl Write,
    margin: usize,
    line_num: &str,
    contents: &str,
) -> fmt::Result {
    use colored::Colorize;

    writeln!(
        buffer,
        "{:<margin$}{}{}",
        line_num.blue().bold(),
        "|".blue().bold(),
        contents,
        margin = margin,
    )
}
