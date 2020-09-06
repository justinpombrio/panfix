// TODO:
// - ParsrConstructionError
// - Proper Source data structure, with line numbers
// - Thorough testing

//! # Panfix
//!
//! Panfix is a new approach to parsing, using a modified version of [operator precedence
//! grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar). It:
//!
//! - [TODO] Question: how complicated of a grammar can it handle?
//! - Runs in linear time.
//! - Can gracefully handle some kinds of parse errors, producing [TODO] excellent error messages
//! in those cases.
//!
//! The Panfix parser is here. It is probably what you want to view:
//!
//! [**☞ The Panfix Parser**](parser)
//!
//! The parser is made up of [TODO] N pieces. If you want more control over the parsing process,
//! you can use or replace any combination of these pieces (they are all independent). Here are the
//! pieces, in the order that they run in the parsing pipeline:
//!
//! 1. [Lexing](lexer)
//! 2. [Shunting](shunter)
//! 3. [Visitor](rpn_visitor)

pub mod lexer;
pub mod parser;
pub mod rpn_visitor;
pub mod shunter;
