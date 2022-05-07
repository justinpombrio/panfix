//! Navigate a stack in RPN notation, as it it were a tree.
//!
//! All operations are amortized O(1) cost.  The stack itself is stored as a vector; besides that
//! there are no heap allocations.

use std::fmt::Debug;
use std::iter::FromIterator;
use std::ops::Deref;

/// The requirement for elements in the stack: each must have an arity.
pub trait RpnNode: Debug {
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone)]
struct Link<N: RpnNode> {
    node: N,
    next: usize,
}

// The outermost groups are joined in a circularly linked list. Likewise each group's children, etc.
/// A stack in RPN notation.
#[derive(Debug)]
pub struct RpnStack<N: RpnNode> {
    stack: Vec<Link<N>>,
    groups: Vec<usize>,
}

// TODO: Stick the soruce in here too!
/// Walk the stack as if it were a tree.
#[derive(Debug)]
pub struct RpnVisitor<'s, N: RpnNode> {
    stack: &'s [Link<N>],
    ptr: usize,
}

/// Walk a node's children.
#[derive(Debug)]
pub struct RpnVisitorIter<'s, N: RpnNode> {
    stack: &'s [Link<N>],
    ptr: usize,
    remaining: usize,
}

impl<'s, N: RpnNode> Clone for RpnVisitor<'s, N> {
    fn clone(&self) -> RpnVisitor<'s, N> {
        RpnVisitor {
            stack: self.stack,
            ptr: self.ptr,
        }
    }
}
impl<'s, N: RpnNode> Copy for RpnVisitor<'s, N> {}

impl<'s, N: RpnNode> RpnVisitorIter<'s, N> {
    fn empty() -> Self {
        RpnVisitorIter {
            stack: &[],
            ptr: 0,
            remaining: 0,
        }
    }
}

impl<'s, N: RpnNode> RpnVisitor<'s, N> {
    pub fn node(&self) -> &N {
        &self.stack[self.ptr].node
    }

    pub fn children(&self) -> RpnVisitorIter<'s, N> {
        if self.stack[self.ptr].node.arity() == 0 {
            RpnVisitorIter::empty()
        } else {
            RpnVisitorIter {
                stack: self.stack,
                ptr: self.stack[self.ptr - 1].next,
                remaining: self.arity(),
            }
        }
    }

    pub fn last_child(&self) -> Option<RpnVisitor<'s, N>> {
        if self.stack[self.ptr].node.arity() == 0 {
            None
        } else {
            Some(RpnVisitor {
                stack: self.stack,
                ptr: self.ptr - 1,
            })
        }
    }
}

impl<'s, N: RpnNode> Deref for RpnVisitor<'s, N> {
    type Target = N;

    fn deref(&self) -> &N {
        &self.stack[self.ptr].node
    }
}

impl<'s, N: RpnNode> Iterator for RpnVisitorIter<'s, N> {
    type Item = RpnVisitor<'s, N>;

    fn next(&mut self) -> Option<RpnVisitor<'s, N>> {
        if self.remaining == 0 {
            return None;
        }
        let visitor = RpnVisitor {
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

impl<'s, N: RpnNode> ExactSizeIterator for RpnVisitorIter<'s, N> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl<N: RpnNode> Default for RpnStack<N> {
    fn default() -> RpnStack<N> {
        RpnStack::new()
    }
}

impl<N: RpnNode> FromIterator<N> for RpnStack<N> {
    fn from_iter<I: IntoIterator<Item = N>>(iter: I) -> RpnStack<N> {
        let mut stack = RpnStack::new();
        for node in iter {
            stack.push(node);
        }
        stack
    }
}

impl<N: RpnNode> RpnStack<N> {
    /// Construct an empty stack.
    pub fn new() -> RpnStack<N> {
        RpnStack {
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

    /// The number of top-level nodes.
    pub fn num_groups(&self) -> usize {
        self.groups.len()
    }

    /// Iterate over the top-level nodes, from left to right.
    pub fn groups(&self) -> RpnVisitorIter<N> {
        if self.groups.is_empty() {
            RpnVisitorIter::empty()
        } else {
            RpnVisitorIter {
                stack: &self.stack,
                ptr: self.groups[0],
                remaining: self.groups.len(),
            }
        }
    }

    /// The last top-level group, if any.
    pub fn last_group(&self) -> Option<RpnVisitor<N>> {
        if self.groups.is_empty() {
            None
        } else {
            Some(RpnVisitor {
                stack: &self.stack,
                ptr: self.stack.len() - 1,
            })
        }
    }

    /// Iterate over all nodes in the stack, from left to right.
    pub fn nodes(&self) -> impl ExactSizeIterator<Item = &N> {
        self.stack.iter().map(|link| &link.node)
    }
}
