use crate::{Col, Line, Offset, Position, Span};

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
}
