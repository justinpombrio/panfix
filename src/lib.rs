mod grammar;
mod lexer;
mod op;
mod parse_error;
mod parse_tree;
mod resolver;
mod shunter;
mod source;
mod tree_visitor;

use lexer::Lexer;
use op::Op;

pub use grammar::{Grammar, GrammarError, Pattern};
pub use op::{Fixity, Prec};
pub use parse_error::ParseError;
pub use parse_tree::{ParseTree, Visitor};
pub use source::{Col, Line, Offset, Position, Source, Span};

/// A category of lexeme, such as "INTEGER" or "VARIABLE" or "OPEN_PAREN".
pub type Token = usize;
type OpToken = Token;

/// Represents a lexing error.
pub const TOKEN_ERROR: Token = 0;
/// Represents a missing argument.
pub const TOKEN_BLANK: Token = 1;
/// Represents a missing operator.
pub const TOKEN_JUXTAPOSE: Token = 2;

const NAME_ERROR: &str = "LexError";
const NAME_BLANK: &str = "Blank";
const NAME_JUXTAPOSE: &str = "Juxtapose";

/// One "word" in the stream returned by the lexer.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Lexeme {
    pub token: Token,
    pub span: Span,
}

/// A Panfix grammar, that's ready to parse.
#[derive(Debug, Clone)]
pub struct Parser {
    lexer: Lexer,
    tok_to_name: Vec<String>,
    tok_to_prefix: Vec<Option<(OpToken, bool)>>,
    tok_to_suffix: Vec<Option<(OpToken, bool)>>,
    optok_to_follower: Vec<Option<(Token, OpToken, bool)>>,
    optok_to_name: Vec<String>,
    optok_to_op: Vec<Option<Op>>,
    optok_to_prec: Vec<(Prec, Prec)>,
}

impl Parser {
    /// Parse `source`. Runs in linear time.
    pub fn parse<'s, 'g>(
        &'g self,
        source: &'s Source,
    ) -> Result<ParseTree<'s, 'g>, ParseError<'s>> {
        use parse_tree::Item;
        use resolver::resolve;
        use shunter::shunt;
        use std::iter::FromIterator;
        use tree_visitor::Forest;

        // 1. Lex
        let lexemes = self.lexer.lex(source.source());
        #[cfg(feature = "debug_mode")]
        let lexemes = self.print_lexemes(source, "Lexed:    ", lexemes);

        // 2. Resolve
        let lexemes = resolve(
            &self.tok_to_prefix,
            &self.tok_to_suffix,
            &self.optok_to_follower,
            lexemes,
        )
        .map_err(|err| {
            ParseError::from_resolver_error(source, &self.tok_to_name, &self.optok_to_name, err)
        })?;
        #[cfg(feature = "debug_mode")]
        let lexemes = self.print_lexemes(source, "Resolved: ", lexemes);

        // 3. Shunt
        let lexemes = shunt(&self.optok_to_prec, lexemes.into_iter());
        #[cfg(feature = "debug_mode")]
        let lexemes = self.print_lexemes(source, "Shunted:  ", lexemes);

        // 4. Filter
        let lexemes = lexemes
            .into_iter()
            .filter(|lex| self.optok_to_op[lex.token].is_some());
        #[cfg(feature = "debug_mode")]
        let lexemes = self.print_lexemes(source, "Filtered: ", lexemes);

        // 5. Map to Op
        let lexemes = lexemes.into_iter().map(|lex| Item {
            op: self.optok_to_op[lex.token].as_ref().unwrap(),
            span: lex.span,
        });

        // 6. Construct forest
        let forest = Forest::from_iter(lexemes);
        Ok(ParseTree::new(source, self, forest))
    }

    #[cfg(feature = "debug_mode")]
    fn print_lexemes(
        &self,
        source: &Source,
        message: &str,
        lexemes: impl IntoIterator<Item = Lexeme>,
    ) -> impl IntoIterator<Item = Lexeme> {
        eprint!("{}", message);
        let lexemes = lexemes.into_iter().collect::<Vec<_>>();
        for lexeme in &lexemes {
            if lexeme.span.is_empty() {
                eprint!("_ ");
            } else {
                eprint!("{} ", source.substr(lexeme.span));
            }
        }
        eprintln!();
        lexemes
    }
}

impl Lexeme {
    pub fn new(token: Token, start: Position, end: Position) -> Lexeme {
        Lexeme {
            token,
            span: Span { start, end },
        }
    }
}

/// Describe the syntax of an operator.
///
/// Consists of a sequence of tokens as string literals, with an optional underscore at the
/// beginning and/or end, to indicate whether the operator takes a left argument and/or a right
/// argument. In actual source code there will typically be arguments between the tokens, but this
/// is not shown in the pattern. For example:
///
///     use panfix::{pattern, Fixity};
///
///     pattern!(_ "+" _);
///     pattern!("if" "(" ")" _);  // C-like if
///     pattern!("if" "{" "}");    // Rust-like if
#[macro_export]
macro_rules! pattern {
    (_ $($items:tt)*) => {
        pattern!(@ Y [ ] $($items)*)
    };

    ($tok:literal $($items:tt)*) => {
        pattern!(@ N [ ] $tok $($items)*)
    };

    (@ $l:ident [ $($tokens:tt)* ] $tok:literal $($rest:tt)*) => {
        pattern!(@ $l [ $($tokens)* $tok.to_string(), ] $($rest)*)
    };

    (@ Y [ $($tokens:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Infix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ Y [ $($tokens:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Suffix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ N [ $($tokens:tt)* ] _) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Prefix,
            tokens: vec![$($tokens)*],
        }
    };

    (@ N [ $($tokens:tt)* ]) => {
        $crate::Pattern {
            fixity: $crate::Fixity::Nilfix,
            tokens: vec![$($tokens)*],
        }
    };
}

/// If you want to peek under the hood, and use the components that make up the parser separately.
pub mod implementation {
    pub mod lexer {
        pub use crate::lexer::*;
    }
    pub mod resolver {
        pub use crate::resolver::*;
    }
    pub mod shunter {
        pub use crate::shunter::*;
    }
    pub mod tree_visitor {
        pub use crate::tree_visitor::*;
    }
}
