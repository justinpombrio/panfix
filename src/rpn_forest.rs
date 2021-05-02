//! Navigate a stack in RPN notation, as it it were a forest.
//!
//! For example, say you want to navigate expressions in RPN notation, over a little language
//! consiting of five kinds of _nodes_:
//!
//! - binary `+`
//! - binary `-`
//! - unary `√`
//! - the number `4`
//! - the number `5`
//!
//! Honestly who needs numbers besides 4 and 5? Those are great numbers.
//!
//! The _proper_ way to represent these nodes would be with an `enum`, but let's be improper and
//! just wrap a `char`:
//!
//! ```
//! use panfix::rpn_forest::{RpnNode, RpnForest};
//!
//! #[derive(Debug)]
//! struct Node(char);
//!
//! impl RpnNode for Node {
//!     fn arity(&self) -> usize {
//!         match self.0 {
//!             '4' | '5' => 0,
//!             '√'       => 1,
//!             '+' | '-' => 2,
//!             _ => unreachable!(),
//!         }
//!     }
//! }
//! ```
//!
//! This `RpnNode` impl is all that's required to make an `RpnForest` out of a sequence of these
//! nodes:
//!
//! ```
//! # use panfix::rpn_forest::{RpnNode, RpnForest, RpnTree};
//! #
//! # #[derive(Debug)]
//! # struct Node(char);
//! #
//! # impl RpnNode for Node {
//! #     fn arity(&self) -> usize {
//! #         match self.0 {
//! #             '4' | '5' => 0,
//! #             '√'       => 1,
//! #             '+' | '-' => 2,
//! #             _ => unreachable!(),
//! #         }
//! #     }
//! # }
//! #
//! // Oh no actually I want a three, how do I... oh I know!
//! let three = "4 5 + √";
//!
//! let nodes = three.chars().filter(|ch| *ch != ' ').map(Node);
//! let forest: RpnForest<Node> = nodes.collect();
//!
//! fn parenthesize(tree: RpnTree<Node>) -> String {
//!     let children = tree.children().map(parenthesize).collect::<Vec<_>>();
//!     match tree.node().0 {
//!         '4' => "4".to_string(),
//!         '5' => "5".to_string(),
//!         '√' => format!("(√ {})", children[0]),
//!         '+' => format!("({} + {})", children[0], children[1]),
//!         '-' => format!("({} - {})", children[0], children[1]),
//!         _ => unreachable!(),
//!     }
//! }
//!
//! assert_eq!(forest.trees().len(), 1);
//! let root = forest.trees().next().unwrap();
//! assert_eq!(parenthesize(root), "(√ (4 + 5))");
//! ```
//!
//! ## Multiple outermost trees
//!
//! It's also possible for there to be more than one outermost tree. For example, the expression `4
//! 4 + 5 5 +` has two trees: `forest.trees()` would iterate over both of them.
//!
//! ## Efficiency
//!
//! All operations are amortized O(1) cost. The stack itself is stored as a single vector; besides
//! that there are no heap allocations.

use std::fmt::Debug;
use std::iter::FromIterator;
use std::ops::Deref;

/// The trait required for nodes in the tree. The `arity` function gives the number of children for
/// this node type. For example `7` would have arity `0`, and `+` would have arity `2`.
pub trait RpnNode: Debug {
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, Copy)]
struct Link<N: RpnNode> {
    node: N,
    next: usize,
}

/// A stack of RPN operations, navigable as a set of trees (a.k.a. a forest).
// The outermost trees are joined in a circularly linked list. Likewise each tree's children, etc.
#[derive(Debug)]
pub struct RpnForest<N: RpnNode> {
    stack: Vec<Link<N>>,
    trees: Vec<usize>,
}

/// A reference to a node in the RPN stack, navigable as a tree.
///
/// For example, a tree might reference the expression `2 3 * 1 +`, in which case its node is `+`
/// and its children are `2 3 *` and `1`.
#[derive(Debug, Clone, Copy)]
pub struct RpnTree<'s, N: RpnNode> {
    stack: &'s [Link<N>],
    ptr: usize,
}

struct RpnTreeIter<'s, N: RpnNode> {
    stack: &'s [Link<N>],
    ptr: usize,
    remaining: usize,
}

impl<'s, N: RpnNode> RpnTreeIter<'s, N> {
    fn empty() -> Self {
        RpnTreeIter {
            stack: &[],
            ptr: 0,
            remaining: 0,
        }
    }
}

impl<'s, N: RpnNode> RpnTree<'s, N> {
    /// The node at the root of this tree.
    pub fn node(&self) -> &N {
        &self.stack[self.ptr].node
    }

    /// The children of this tree. For example, the tree `2 3 * 1 +` has children `2 3 *` and `1`.
    pub fn children(&self) -> impl ExactSizeIterator<Item = RpnTree<'s, N>> {
        if self.stack[self.ptr].node.arity() == 0 {
            RpnTreeIter::empty()
        } else {
            RpnTreeIter {
                stack: self.stack,
                ptr: self.stack[self.ptr - 1].next,
                remaining: self.arity(),
            }
        }
    }

    /// The last child of this tree. Equivalent to `tree.children().last()`, but faster.
    pub fn last_child(&self) -> Option<RpnTree<'s, N>> {
        if self.stack[self.ptr].node.arity() == 0 {
            None
        } else {
            Some(RpnTree {
                stack: self.stack,
                ptr: self.ptr - 1,
            })
        }
    }
}

impl<'s, N: RpnNode> Deref for RpnTree<'s, N> {
    type Target = N;

    fn deref(&self) -> &N {
        &self.stack[self.ptr].node
    }
}

impl<'s, N: RpnNode> Iterator for RpnTreeIter<'s, N> {
    type Item = RpnTree<'s, N>;

    fn next(&mut self) -> Option<RpnTree<'s, N>> {
        if self.remaining == 0 {
            return None;
        }
        let tree = RpnTree {
            stack: self.stack,
            ptr: self.ptr,
        };
        self.ptr = self.stack[self.ptr].next;
        self.remaining -= 1;
        Some(tree)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl<'s, N: RpnNode> ExactSizeIterator for RpnTreeIter<'s, N> {
    fn len(&self) -> usize {
        self.remaining
    }
}

impl<N: RpnNode> Default for RpnForest<N> {
    fn default() -> RpnForest<N> {
        RpnForest::new()
    }
}

impl<N: RpnNode> FromIterator<N> for RpnForest<N> {
    fn from_iter<I: IntoIterator<Item = N>>(iter: I) -> RpnForest<N> {
        let mut stack = RpnForest::new();
        for node in iter {
            stack.push(node);
        }
        stack
    }
}

impl<N: RpnNode> RpnForest<N> {
    /// Construct an empty stack.
    pub fn new() -> RpnForest<N> {
        RpnForest {
            stack: vec![],
            trees: vec![],
        }
    }

    /// Push a node onto the stack. Amortized O(1).
    ///
    /// # Panics
    ///
    /// Panics if `self.num_trees() < node.arity()`, which would form an invalid stack. (For
    /// example, `2 +` is invalid: the binary `+` does not have enough arguments.)
    pub fn push(&mut self, node: N) {
        assert!(self.trees.len() >= node.arity(), "Invalid Rpn stack");
        let arity = node.arity();

        // Link the new node's children together into a loop.
        if arity > 0 {
            let first_child = self.trees[self.trees.len() - arity];
            let last_child = self.trees[self.trees.len() - 1];
            self.stack[last_child].next = first_child;
        }

        // Push the new node, pointing to itself at least for the moment.
        let new_tree = self.stack.len();
        self.stack.push(Link {
            node,
            next: new_tree,
        });

        // Update the stack of tree pointers.
        for _ in 0..arity {
            self.trees.pop();
        }
        self.trees.push(new_tree);

        // Thread the new node into the existing loop of trees.
        if self.trees.len() > 1 {
            let first_tree = self.trees[0];
            let last_tree = self.trees[self.trees.len() - 2];
            self.stack[last_tree].next = new_tree;
            self.stack[new_tree].next = first_tree;
        }
    }

    /// The outermost trees of the forest. For example, `1 2 + 3 4 +` has two outermost
    /// trees: `1 2 +` and `3 4 +`.
    pub fn trees(&self) -> impl ExactSizeIterator<Item = RpnTree<N>> {
        if self.trees.is_empty() {
            RpnTreeIter::empty()
        } else {
            RpnTreeIter {
                stack: &self.stack,
                ptr: self.trees[0],
                remaining: self.trees.len(),
            }
        }
    }

    /// The last tree of the forest. Equivalent to `forest.trees().last()`, but faster.
    pub fn last_tree(&self) -> Option<RpnTree<N>> {
        if self.trees.is_empty() {
            None
        } else {
            Some(RpnTree {
                stack: &self.stack,
                ptr: self.stack.len() - 1,
            })
        }
    }

    /// The number of outermost trees in the forest.
    pub fn num_trees(&self) -> usize {
        self.trees.len()
    }

    /// All of the nodes in the stack in order. For example, the stack `1 2 +` has nodes `1`, `2`,
    /// and `+` in that order.
    pub fn nodes(&self) -> impl ExactSizeIterator<Item = &N> {
        self.stack.iter().map(|link| &link.node)
    }
}
