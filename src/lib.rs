// TODO:
// - ParsrConstructionError
// - Proper Source data structure, with line numbers
// - Thorough testing
// - Grammar validation! If you have an op `t1 NT t2`, then no op in `NT` can begin with `t2`.

//! # Panfix
//!
//! Panfix is a new approach to parsing, using a modified version of [operator precedence
//! grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar). It:
//!
//! - (TODO) Question: how complicated of a grammar can it handle?
//! - Runs in linear time.
//! - Can gracefully handle some kinds of parse errors, producing (TODO) excellent error messages
//! in those cases.
//!
//! This crate is incomplete, and in a messy state. For now, you should use the reference
//! implementation:
//!
//! [**Reference Implementation**](refn_impl)
//!
//! Below is information about the (incomplete!) more robust/efficient implementation.
//!
//! ------------------------
//!
//! The Panfix parser is here. It is probably what you want to view:
//!
//! [**☞ The Panfix Parser**](parsing)
//!
//! The parser is made up of three pieces. If you want more control over the parsing process,
//! you can use or replace any combination of these pieces (they are all independent). Here are the
//! pieces, in the order that they run in the parsing pipeline:
//!
//! 1. [Lexing](lexing)
//! 2. [Shunting](shunting)
//! 3. [Visiting](rpn_visitor)

pub mod lexing;
/*
pub mod parsing;
pub mod rpn_visitor;
pub mod shunting;

pub mod refn_impl;

impl<'g, T: lexing::Token> rpn_visitor::Node for shunting::Node<'g, T> {
    fn arity(&self) -> usize {
        shunting::Node::arity(*self)
    }
}
*/
