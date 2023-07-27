use std::fmt;

/// A byte offset into the source file.
pub type Offset = usize;
/// A line number of a source file. Zero indexed.
pub type Line = u32;
/// A column number of a source file. Zero indexed.
pub type Col = u32;

/// A position in the source text. Positions are _between_ characters.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    /// Line number. Zero indexed.
    pub line: Line,
    /// Column number, counted in bytes. Zero indexed.
    pub col: Col,
    /// Column number, counted in utf8 codepoints. Zero indexed.
    pub utf8_col: Col,
}

/// A start and end position in the source text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.utf8_col)
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.start, self.end)
    }
}

impl Position {
    /// The position at the start of any document.
    pub fn start_of_file() -> Position {
        Position {
            line: 0,
            col: 0,
            utf8_col: 0,
        }
    }

    /// Assuming that `ch` appears just after this position, return the position just
    /// after `ch`.
    pub fn advance_by_char(self, ch: char) -> Position {
        if ch == '\n' {
            Position {
                line: self.line + 1,
                col: 0,
                utf8_col: 0,
            }
        } else {
            Position {
                line: self.line,
                col: self.col + ch.len_utf8() as Col,
                utf8_col: self.utf8_col + 1,
            }
        }
    }
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        Span { start, end }
    }

    pub fn new_at_pos(pos: Position) -> Span {
        Span {
            start: pos,
            end: pos,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

/// Source text, and a "filename" for it (though it need not have been read from a file).
/// Newline positions within the source are indexed, which allows all methods to run in
/// constant time.
#[derive(Debug, Clone)]
pub struct Source {
    filename: String,
    source: String,
    newline_positions: Vec<Offset>,
    ends_in_newline: bool,
}

impl Source {
    /// Read from stdin. The "filename" will be `"[stdin]"`.
    pub fn from_stdin() -> std::io::Result<Source> {
        use std::io::Read;

        let mut input = String::new();
        std::io::stdin().lock().read_to_string(&mut input)?;
        Ok(Source::new("[stdin]", input))
    }

    /// Read from a file. The "filename" will be the whole path, with any invalid
    /// unicode in the path replaced with U+FFFD REPLACEMENT CHARACTER.
    pub fn open(path: impl AsRef<std::path::Path>) -> std::io::Result<Source> {
        let path = path.as_ref();
        let text = std::fs::read_to_string(path)?;
        Ok(Source::new(&path.to_string_lossy(), text))
    }

    pub fn new(filename: &str, source: String) -> Source {
        let mut pos = 0;
        let mut newline_positions = vec![0];
        for ch in source.chars() {
            pos += ch.len_utf8();
            if ch == '\n' {
                newline_positions.push(pos);
            }
        }
        let ends_in_newline = source.chars().rev().next() == Some('\n');
        Source {
            filename: filename.to_owned(),
            source,
            newline_positions,
            ends_in_newline,
        }
    }

    /// The "filename" given in the constructor (though the source need not have been
    /// read from a file).
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

    /// Get the position at the end of the file. If the file ends with a newline, returns the
    /// position _just before_ the newline.
    pub fn end_of_file(&self) -> Position {
        let mut line = (self.newline_positions.len() - 1) as Line;
        if self.ends_in_newline {
            line -= 1;
        }
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
