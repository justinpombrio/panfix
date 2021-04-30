/// Compute line and column info for a source file. Upon construction, this will scan the source
/// file once for newlines. After construction, you can query for the line&column of a position
/// within the file in O(1) time.
#[derive(Debug, Clone)]
pub struct LineCounter<'s> {
    source: &'s str,
    line_starts: Vec<usize>,
}

impl<'s> LineCounter<'s> {
    /// Construct a line counter for the source file. This will scan the file for newlines, to
    /// allow all operations to be O(1).
    pub fn new(source: &'s str) -> LineCounter<'s> {
        let mut pos = 0;
        let mut line_starts = vec![];
        for line in source.split_inclusive('\n') {
            line_starts.push(pos);
            pos += line.len();
        }
        LineCounter {
            source,
            line_starts,
        }
    }

    /// Get the line and column of a position within the source. `pos` is relative to the start of
    /// the `source` string. A newline or return character is considered part of the line it ends.
    ///
    /// The line and column are 0-indexed. There is unfortunately not a strong consensus on whether
    /// lines should be 0-indexed or 1-indexed; you may need to convert depending on your use case.
    ///
    /// # Panics
    ///
    /// Panics if `pos` is past the end of the source string.
    pub fn line_col(&self, pos: usize) -> (usize, usize) {
        let line = match self.line_starts.binary_search(&pos) {
            Ok(line) => line,
            Err(line) => line - 1,
        };
        let col = pos - self.line_starts[line];
        (line, col)
    }

    /// Get the total number of lines in the source.
    pub fn num_lines(&self) -> usize {
        self.line_starts.len()
    }

    /// Get the contents of the `line_num`th line. Excludes the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line_num` lines.
    pub fn line_contents(&self, line_num: usize) -> &'s str {
        let (start, end) = self.line_span(line_num);
        &self.source[start..end]
    }

    /// Get the start and end position of the `line_num`th line. The start is inclusive, and the
    /// end is exclusive. Excludes the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line_num` lines.
    pub fn line_span(&self, line_num: usize) -> (usize, usize) {
        let (start, mut end) = self.line_span_inclusive(line_num);
        let line = &self.source[start..end];
        if line.ends_with("\r\n") {
            end -= 2;
        } else if line.ends_with("\n") {
            end -= 1;
        }
        (start, end)
    }

    /// Get the contents of the `line_num`th line. Includes the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line_num` lines.
    pub fn line_contents_inclusive(&self, line_num: usize) -> &'s str {
        let (start, end) = self.line_span_inclusive(line_num);
        &self.source[start..end]
    }

    /// Get the start and end position of the `line_num`th line. The start is inclusive, and the
    /// end is exclusive. Includes the line termination character(s).
    ///
    /// # Panics
    ///
    /// Panics if there are fewer than `line_num` lines.
    pub fn line_span_inclusive(&self, line_num: usize) -> (usize, usize) {
        let start = self.line_starts[line_num];
        let end = match self.line_starts.get(line_num + 1) {
            Some(end_pos) => *end_pos,
            None => self.source.len(),
        };
        (start, end)
    }
}
