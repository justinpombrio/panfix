//! Given a sequence of items in RPN (reverse polish notation) order, allow navigating them as a
//! tree.
//!
//! All operations are amortized O(1) cost. All storage is held in two vectors; besides that there
//! are no heap allocations.
use std::fmt::Debug;
use std::iter::FromIterator;

/// The requirement for elements in the tree: each must have an arity.
pub trait Arity: Debug + Copy {
    fn arity(&self) -> usize;
}

#[derive(Debug, Clone, Copy)]
struct Node<I: Arity> {
    item: I,
    first_child_ptr: usize,
}

/// A set of trees, built from a sequence in RPN order.
#[derive(Debug)]
pub struct Forest<I: Arity> {
    roots: Vec<Node<I>>,
    nodes: Vec<Node<I>>,
}

/// One node in a tree.
#[derive(Debug, Clone, Copy)]
pub struct Visitor<'f, I: Arity> {
    forest: &'f Forest<I>,
    node: Node<I>,
}

impl<'f, I: Arity> Visitor<'f, I> {
    pub fn item(&self) -> &I {
        &self.node.item
    }

    pub fn arity(&self) -> usize {
        self.node.item.arity()
    }

    pub fn child(&self, index: usize) -> Option<Visitor<'f, I>> {
        if index < self.node.item.arity() {
            Some(Visitor {
                forest: self.forest,
                node: self.forest.nodes[self.node.first_child_ptr + index],
            })
        } else {
            None
        }
    }
}

impl<I: Arity> Default for Forest<I> {
    fn default() -> Forest<I> {
        Forest::new()
    }
}

impl<I: Arity> FromIterator<I> for Forest<I> {
    fn from_iter<Iter: IntoIterator<Item = I>>(iter: Iter) -> Forest<I> {
        let mut forest = Forest::new();
        for item in iter {
            forest.push(item);
        }
        forest
    }
}

impl<I: Arity> Forest<I> {
    /// Construct an empty forest.
    pub fn new() -> Forest<I> {
        Forest {
            roots: Vec::new(),
            nodes: Vec::new(),
        }
    }

    /// Push an item onto the forest. Amortized O(1).
    ///
    /// # Panics
    ///
    /// Panics if `self.num_trees() < item.arity()`, which would form an invalid RPN sequence. (For
    /// example, `2 +` is invalid: the binary `+` does not have enough arguments.)
    pub fn push(&mut self, item: I) {
        let arity = item.arity();
        assert!(arity <= self.roots.len(), "Invalid RPN stack");
        let mut node = Node {
            item,
            first_child_ptr: 0,
        };
        if arity > 0 {
            let mut tail = self.roots.split_off(self.roots.len() - arity);
            node.first_child_ptr = self.nodes.len();
            self.nodes.extend_from_slice(&mut tail);
        }
        self.roots.push(node);
    }

    /// The number of top-level nodes.
    pub fn num_trees(&self) -> usize {
        self.roots.len()
    }

    /// Get the `n`th tree.
    pub fn tree(&self, n: usize) -> Option<Visitor<I>> {
        if n <= self.roots.len() {
            Some(Visitor {
                forest: &self,
                node: self.roots[n],
            })
        } else {
            None
        }
    }
}

#[test]
fn test_tree_visitor() {
    impl Arity for usize {
        fn arity(&self) -> usize {
            *self
        }
    }

    fn show_forest(forest: &Forest<usize>) -> String {
        let mut out = String::new();
        for i in 0..forest.num_trees() {
            if i != 0 {
                out.push(' ');
            }
            show_tree(forest.tree(i).unwrap(), &mut out);
        }
        out
    }

    fn show_tree(visitor: Visitor<usize>, out: &mut String) {
        match *visitor.item() {
            0 => out.push('0'),
            n => {
                out.push('(');
                out.push_str(&format!("{}", n));
                for i in 0..visitor.arity() {
                    out.push(' ');
                    show_tree(visitor.child(i).unwrap(), out);
                }
                out.push(')');
            }
        }
    }

    let forest = Forest::from_iter([0].iter().copied());
    assert_eq!(&show_forest(&forest), "0");

    let forest = Forest::from_iter([0, 0, 2].iter().copied());
    assert_eq!(&show_forest(&forest), "(2 0 0)");

    let forest = Forest::from_iter([0, 1, 1, 1].iter().copied());
    assert_eq!(&show_forest(&forest), "(1 (1 (1 0)))");

    let forest = Forest::from_iter([0, 1, 0, 0, 1, 0, 3, 0, 0, 2].iter().copied());
    assert_eq!(&show_forest(&forest), "(1 0) (3 0 (1 0) 0) (2 0 0)");

    let forest = Forest::from_iter([0, 1, 1, 0, 0, 1, 3, 0, 0, 1, 2, 0, 3].iter().copied());
    assert_eq!(
        &show_forest(&forest),
        "(3 (3 (1 (1 0)) 0 (1 0)) (2 0 (1 0)) 0)"
    );
}
