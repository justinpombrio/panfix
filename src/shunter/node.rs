use super::grammar::{Fixity, Operator};
use crate::{Span, Token};

#[derive(Debug, Clone, Copy)]
pub struct Node<'g, T: Token> {
    pub op: &'g Operator<T>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct NodeBuilder {
    spans: Vec<Span>,
}

impl<'g, T: Token> Node<'g, T> {
    pub fn fixity(self) -> Fixity {
        self.op.fixity()
    }

    pub fn arity(self) -> usize {
        self.op.arity()
    }

    pub fn text(self, source: &str) -> &str {
        &source[self.span.0..self.span.1]
    }
}

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder { spans: vec![] }
    }

    pub fn build<'g, T: Token>(&mut self, op: &'g Operator<T>, op_span: Span) -> Node<'g, T> {
        use Fixity::*;
        println!("Build: {}", op.name);
        match op.fixity() {
            Nilfix => self.build_nilfix(op, op_span),
            Prefix(_) => self.build_prefix(op, op_span),
            Suffix(_) => self.build_suffix(op, op_span),
            Infix(_, _) => self.build_infix(op, op_span),
        }
    }

    fn build_nilfix<'g, T: Token>(&mut self, op: &'g Operator<T>, op_span: Span) -> Node<'g, T> {
        self.spans.push(op_span);
        Node { op, span: op_span }
    }

    fn build_prefix<'g, T: Token>(&mut self, op: &'g Operator<T>, op_span: Span) -> Node<'g, T> {
        let arg_span = self.spans.pop().unwrap();
        let span = (op_span.0, arg_span.1);
        self.spans.push(span);
        Node { op, span }
    }

    fn build_suffix<'g, T: Token>(&mut self, op: &'g Operator<T>, op_span: Span) -> Node<'g, T> {
        let arg_span = self.spans.pop().unwrap();
        let span = (arg_span.0, op_span.1);
        self.spans.push(span);
        Node { op, span }
    }

    fn build_infix<'g, T: Token>(&mut self, op: &'g Operator<T>, _op_span: Span) -> Node<'g, T> {
        let arg_2_span = self.spans.pop().unwrap();
        let arg_1_span = self.spans.pop().unwrap();
        let span = (arg_1_span.0, arg_2_span.1);
        self.spans.push(span);
        Node { op, span }
    }
}
