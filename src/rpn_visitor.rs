//! Navigate a stack in RPN notation, as it it were a tree.
//!
//! All operations are amortized O(1) cost.  The stack itself is stored as a vector; besides that
//! there are no heap allocations.

use std::fmt::Debug;
use std::iter::FromIterator;
use std::ops::Deref;

pub trait Node: Debug {
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, Copy)]
struct Link<N: Node> {
    node: N,
    next: usize,
}

// The outermost groups are joined in a circularly linked list. Likewise each group's children, etc.
#[derive(Debug)]
pub struct Stack<N: Node> {
    stack: Vec<Link<N>>,
    groups: Vec<usize>,
}

// TODO: Stick the soruce in here too!
#[derive(Debug, Clone, Copy)]
pub struct Visitor<'s, N: Node> {
    stack: &'s [Link<N>],
    ptr: usize,
}

pub struct VisitorIter<'s, N: Node> {
    stack: &'s [Link<N>],
    ptr: usize,
    remaining: usize,
}

impl<'s, N: Node> VisitorIter<'s, N> {
    fn empty() -> Self {
        VisitorIter {
            stack: &[],
            ptr: 0,
            remaining: 0,
        }
    }
}

impl<'s, N: Node> Visitor<'s, N> {
    pub fn node(&self) -> &N {
        &self.stack[self.ptr].node
    }

    pub fn children(&self) -> VisitorIter<'s, N> {
        if self.stack[self.ptr].node.arity() == 0 {
            VisitorIter::empty()
        } else {
            VisitorIter {
                stack: self.stack,
                ptr: self.stack[self.ptr - 1].next,
                remaining: self.arity(),
            }
        }
    }

    pub fn last_child(&self) -> Option<Visitor<'s, N>> {
        if self.stack[self.ptr].node.arity() == 0 {
            None
        } else {
            Some(Visitor {
                stack: self.stack,
                ptr: self.ptr - 1,
            })
        }
    }
}

impl<'s, N: Node> Deref for Visitor<'s, N> {
    type Target = N;

    fn deref(&self) -> &N {
        &self.stack[self.ptr].node
    }
}

impl<'s, N: Node> Iterator for VisitorIter<'s, N> {
    type Item = Visitor<'s, N>;

    fn next(&mut self) -> Option<Visitor<'s, N>> {
        if self.remaining == 0 {
            return None;
        }
        let visitor = Visitor {
            stack: self.stack,
            ptr: self.ptr,
        };
        self.ptr = self.stack[self.ptr].next;
        self.remaining -= 1;
        Some(visitor)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'s, N: Node> ExactSizeIterator for VisitorIter<'s, N> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl<N: Node> Default for Stack<N> {
    fn default() -> Stack<N> {
        Stack::new()
    }
}

impl<N: Node> FromIterator<N> for Stack<N> {
    fn from_iter<I: IntoIterator<Item = N>>(iter: I) -> Stack<N> {
        let mut stack = Stack::new();
        for node in iter {
            stack.push(node);
        }
        stack
    }
}

impl<N: Node> Stack<N> {
    /// Construct an empty stack.
    pub fn new() -> Stack<N> {
        Stack {
            stack: vec![],
            groups: vec![],
        }
    }

    /// Push a node onto the stack. Amortized O(1).
    ///
    /// # Panics
    ///
    /// Panics if `self.num_groups() < node.arity()`, which would form an invalid stack. (For
    /// example, `2 +` is invalid: the binary `+` does not have enough arguments.)
    pub fn push(&mut self, node: N) {
        assert!(self.groups.len() >= node.arity(), "Invalid Rpn stack");
        let arity = node.arity();

        // Link the new node's children together into a loop.
        if arity > 0 {
            let first_child = self.groups[self.groups.len() - arity];
            let last_child = self.groups[self.groups.len() - 1];
            self.stack[last_child].next = first_child;
        }

        // Push the new node, pointing to itself at least for the moment.
        let new_group = self.stack.len();
        self.stack.push(Link {
            node,
            next: new_group,
        });

        // Update the stack of group pointers.
        for _ in 0..arity {
            self.groups.pop();
        }
        self.groups.push(new_group);

        // Thread the new node into the existing loop of groups.
        if self.groups.len() > 1 {
            let first_group = self.groups[0];
            let last_group = self.groups[self.groups.len() - 2];
            self.stack[last_group].next = new_group;
            self.stack[new_group].next = first_group;
        }
    }

    pub fn num_groups(&self) -> usize {
        self.groups.len()
    }

    pub fn groups(&self) -> VisitorIter<N> {
        if self.groups.is_empty() {
            VisitorIter::empty()
        } else {
            VisitorIter {
                stack: &self.stack,
                ptr: self.groups[0],
                remaining: self.groups.len(),
            }
        }
    }

    pub fn last_group(&self) -> Option<Visitor<N>> {
        if self.groups.is_empty() {
            None
        } else {
            Some(Visitor {
                stack: &self.stack,
                ptr: self.stack.len() - 1,
            })
        }
    }

    pub fn nodes(&self) -> impl ExactSizeIterator<Item = &N> {
        self.stack.iter().map(|link| &link.node)
    }
}
