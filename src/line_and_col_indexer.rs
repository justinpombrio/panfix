//! Compute line and column info for a source file. You do not need to use this module directly if
//! you're using the `panfix` parser.
//!
//! Upon construction, the `LineAndColIndexer` will scan the source file once for newlines. After
//! construction, you can query for the line&column of a position within the file in O(1) time.
//!
//! ```
//! use panfix::line_and_col_indexer::LineAndColIndexer;
//!
//! //                                    0123 4 567
//! let counter = LineAndColIndexer::new("abc\r\ndef");
//!
//! // There are two lines
//! assert_eq!(counter.num_lines(), 2);
//!
//! // "c" is at line 0, col 3
//! assert_eq!(counter.line_col(3), (0, 3));
//!
//! // "e" is at line 1, col 1
//! assert_eq!(counter.line_col(6), (1, 1));
//!
//! // View all of "e"s line
//! assert_eq!(counter.line_contents(1), "def");
//! ```

/// A store of newline locations within a source text, for the purpose of quickly computing line
/// and column positions.
#[derive(Debug, Clone)]
pub struct LineAndColIndexer<'s> {
    source: &'s str,
    line_starts: Vec<usize>,
}

impl<'s> LineAndColIndexer<'s> {
    /// Construct a line counter for the source file. This will scan the file for newlines, to
    /// allow all further operations to be O(1).
    pub fn new(source: &'s str) -> LineAndColIndexer<'s> {
        let mut pos = 0;
        let mut line_starts = vec![];
        for line in source.split_inclusive('\n') {
            line_starts.push(pos);
            pos += line.len();
        }
        LineAndColIndexer {
            source,
            line_starts,
        }
    }

    /// Get the original source text.
    pub fn source(&self) -> &'s str {
        self.source
    }

    /// Get the line and column of a position (byte index) within the source. `pos` is relative to
    /// the start of the `source` string. A newline or return character is considered part of the
    /// line it ends.
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

    /// Like `line_contents`, but includes the line termination character(s).
    pub fn line_contents_inclusive(&self, line_num: usize) -> &'s str {
        let (start, end) = self.line_span_inclusive(line_num);
        &self.source[start..end]
    }

    /// Get the start and end position (byte index) of the `line_num`th line. The start is
    /// inclusive, and the end is exclusive. Does not include the line termination character(s).
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

    /// Like `line_span`, but includes the line termination character(s).
    pub fn line_span_inclusive(&self, line_num: usize) -> (usize, usize) {
        let start = self.line_starts[line_num];
        let end = match self.line_starts.get(line_num + 1) {
            Some(end_pos) => *end_pos,
            None => self.source.len(),
        };
        (start, end)
    }
}
