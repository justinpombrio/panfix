use super::shunter::Operator;
use crate::lexer::{Span, Token};
use crate::rpn_visitor::Node as NodeTrait;

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
        println!("Build: {}", op.name);
        let mut span = op_span;
        for _ in 0..op.arity() {
            let arg_span = self.spans.pop().unwrap();
            span.0 = span.0.min(arg_span.0);
            span.1 = span.1.max(arg_span.1);
        }
        self.spans.push(span);
        Node { op, span }
    }
}

impl<'g, T: Token> NodeTrait for Node<'g, T> {
    fn arity(&self) -> usize {
        Node::arity(*self)
    }
}
