mod rpn_forest {
    use panfix::rpn_forest::{RpnForest, RpnNode, RpnTree};

    #[derive(Debug)]
    struct Node(char);

    impl RpnNode for Node {
        fn arity(&self) -> usize {
            match self.0 {
                'a' | 'b' => 0,
                '√' => 1,
                '+' | '*' => 2,
                '?' => 3,
                _ => unreachable!(),
            }
        }
    }

    fn write_sexpr(sexpr: &mut String, tree: RpnTree<Node>) {
        let node = tree.node();
        if node.arity() == 0 {
            sexpr.push(node.0);
        } else {
            sexpr.push('(');
            sexpr.push(node.0);
            assert_eq!(tree.children().len(), node.arity());
            for child in tree.children() {
                sexpr.push(' ');
                write_sexpr(sexpr, child);
            }
            sexpr.push(')');
        }
    }

    fn make_stack(source: &str) -> RpnForest<Node> {
        let mut rpn = RpnForest::new();
        if !source.is_empty() {
            for token in source.split(' ') {
                let ch = token.chars().next().unwrap();
                rpn.push(Node(ch));
            }
        }
        rpn
    }

    fn to_tree(source: &str) -> String {
        let rpn = make_stack(source);
        let mut sexpr = "".to_string();
        let len = rpn.trees().len();
        assert_eq!(len, rpn.num_trees());
        for (i, tree) in rpn.trees().enumerate() {
            write_sexpr(&mut sexpr, tree);
            if i + 1 != len {
                sexpr.push(' ');
            }
        }
        sexpr
    }

    fn last_to_tree(source: &str) -> String {
        let rpn = make_stack(source);
        let mut sexpr = "".to_string();
        if let Some(tree) = rpn.last_tree() {
            write_sexpr(&mut sexpr, tree);
        }
        sexpr
    }

    #[test]
    fn test_empty() {
        assert_eq!(to_tree(""), "");
    }

    #[test]
    fn test_many_trees() {
        assert_eq!(to_tree("a a b a"), "a a b a");
        assert_eq!(to_tree("a b + b a *"), "(+ a b) (* b a)");
    }

    #[test]
    fn test_left_and_right() {
        assert_eq!(to_tree("a b a b a + * + *"), "(* a (+ b (* a (+ b a))))");
        assert_eq!(to_tree("a b + a * b + a *"), "(* (+ (* (+ a b) a) b) a)");
        assert_eq!(to_tree("a a b a ? a ?"), "(? a (? a b a) a)");
    }

    #[test]
    fn test_complicated() {
        assert_eq!(to_tree("a a * b b * + √"), "(√ (+ (* a a) (* b b)))");
        assert_eq!(
            to_tree("a a * b b * + √ a a * b b * + √"),
            "(√ (+ (* a a) (* b b))) (√ (+ (* a a) (* b b)))"
        );
    }

    #[test]
    fn test_last_tree() {
        assert_eq!(last_to_tree(""), "");
        assert_eq!(last_to_tree("a a b"), "b");
        assert_eq!(last_to_tree("a a * b b *"), "(* b b)");
    }

    #[test]
    fn test_last_child_none() {
        let rpn = make_stack("a b");
        let group = rpn.last_tree().unwrap();
        assert!(group.last_child().is_none());
    }

    #[test]
    fn test_last_child() {
        let rpn = make_stack("a a * a b * +");
        let group = rpn.trees().next().unwrap();
        let child = group.last_child().unwrap();
        let mut sexpr = String::new();
        write_sexpr(&mut sexpr, child);
        assert_eq!(sexpr, "(* a b)");
    }
}
