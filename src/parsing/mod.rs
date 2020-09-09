mod parsed;
mod parser;

/// White space, according to the Pattern_White_Space Unicode property.
pub const WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]*";

pub use parsed::{ParseError, Parsed, Visitor, VisitorIter};
pub use parser::{Assoc, Fixity, Grammar, Parser, Pattern, Rule};
