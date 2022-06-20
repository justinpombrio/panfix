use crate::{Col, Line, Offset, Position, Span};
use std::fmt;

/// A store of newline locations within a source text, for the purpose of quickly computing line
/// and column positions.
#[derive(Debug, Clone)]
pub struct Source {
    filename: String,
    source: String,
    newline_positions: Vec<Offset>,
}

impl Source {
    /// Scan a source file for newlines, to allow all further operations to be O(1).
    pub fn new(filename: &str, source: String) -> Source {
        let mut pos = 0;
        let mut newline_positions = vec![0];
        for ch in source.chars() {
            pos += ch.len_utf8();
            if ch == '\n' {
                newline_positions.push(pos);
            }
        }
        Source {
            filename: filename.to_owned(),
            source,
            newline_positions,
        }
    }

    pub fn filename(&self) -> &str {
        &self.filename
    }

    /// Get the original source text.
    pub fn source(&self) -> &str {
        &self.source
    }

    /// Get the total number of lines in the source.
    pub fn num_lines(&self) -> usize {
        self.newline_positions.len() - 1
    }

    /// Get the substring of the source between the start and end of the given span.
    pub fn substr(&self, span: Span) -> &str {
        &self.source[self.offset(span.start)..self.offset(span.end)]
    }

    /// Get the substring of the source between the start and end offset.
    pub fn substr_between(&self, start: Offset, end: Offset) -> &str {
        &self.source[start..end]
    }

    /// Convert a position into its byte offset from the beginning of the source.
    pub fn offset(&self, pos: Position) -> Offset {
        self.newline_positions[pos.line as usize] + pos.col as usize
    }

    /// Get the contents of the `line`th line. Excludes the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line` lines.
    pub fn line_contents(&self, line: Line) -> &str {
        let (start, end) = self.line_span(line);
        &self.source[start..end]
    }

    /// Like `line_contents`, but includes the line termination character(s).
    pub fn line_contents_inclusive(&self, line: Line) -> &str {
        let (start, end) = self.line_span_inclusive(line);
        &self.source[start..end]
    }

    /// Get the start and end position (byte index) of the `line`th line. The start is
    /// inclusive, and the end is exclusive. Does not include the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line` lines.
    pub fn line_span(&self, line: Line) -> (Offset, Offset) {
        let (start, mut end) = self.line_span_inclusive(line);
        let line = &self.source[start..end];
        if line.ends_with("\r\n") {
            end -= 2;
        } else if line.ends_with('\n') {
            end -= 1;
        }
        (start, end)
    }

    /// Like `line_span`, but includes the line termination character(s).
    pub fn line_span_inclusive(&self, line: Line) -> (Offset, Offset) {
        let start = self.newline_positions[line as usize];
        let end = match self.newline_positions.get(line as usize + 1) {
            Some(end_pos) => *end_pos,
            None => self.source.len(),
        };
        (start, end)
    }

    /// Get the position at the end of the file.
    pub fn end_of_file(&self) -> Position {
        let line = (self.newline_positions.len() - 1) as Line;
        let col = self.line_contents_inclusive(line).len() as Col;
        let utf8_col = self.line_contents_inclusive(line).chars().count() as Col;
        Position {
            line,
            col,
            utf8_col,
        }
    }

    /// Display a highlighted span of the source. For example:
    ///
    /// ```text
    ///     x += n + 1;
    ///          ^^^^^
    pub fn show_span(&self, f: &mut fmt::Formatter, span: Span) -> fmt::Result {
        if span.start.line == span.end.line {
            self.show_line(
                f,
                span.start.line,
                Some(span.start.utf8_col),
                Some(span.end.utf8_col),
            )
        } else {
            self.show_line(f, span.start.line, Some(span.start.utf8_col), None)?;
            for line_num in span.start.line + 1..span.end.line {
                self.show_line(f, line_num, None, None)?;
            }
            self.show_line(f, span.end.line, None, Some(span.end.utf8_col))
        }
    }

    fn show_line(
        &self,
        f: &mut fmt::Formatter,
        line_num: Line,
        start: Option<Col>,
        end: Option<Col>,
    ) -> fmt::Result {
        let line = self.line_contents(line_num);
        let start = start.unwrap_or(0);
        let end = end.unwrap_or_else(|| line.chars().count() as Col);
        writeln!(f, "{}", line)?;
        for _ in 0..start {
            write!(f, " ")?;
        }
        for _ in 0..(end - start).max(1) {
            write!(f, "^")?;
        }
        writeln!(f)
    }
}
