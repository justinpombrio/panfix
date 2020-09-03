use panfix::rpn_visitor::{Node, Stack, Visitor};

#[cfg(test)]
mod rpn_tests {
    use super::*;

    #[derive(Debug)]
    struct Item(char);

    impl Node for Item {
        fn arity(&self) -> usize {
            match self.0 {
                'a' | 'b' => 0,
                '√' => 1,
                '+' | '*' => 2,
                '?' => 3,
                _ => unimplemented!(),
            }
        }
    }

    fn write_sexpr(sexpr: &mut String, visitor: Visitor<Item>) {
        let node = visitor.node();
        if node.arity() == 0 {
            sexpr.push(node.0);
        } else {
            sexpr.push('(');
            sexpr.push(node.0);
            assert_eq!(visitor.children().len(), node.arity());
            for child in visitor.children() {
                sexpr.push(' ');
                write_sexpr(sexpr, child);
            }
            sexpr.push(')');
        }
    }

    fn to_tree(source: &str) -> String {
        let mut rpn = Stack::new();
        if !source.is_empty() {
            for token in source.split(' ') {
                let ch = token.chars().next().unwrap();
                rpn.push(Item(ch));
            }
        }
        let mut sexpr = "".to_string();
        let len = rpn.groups().len();
        assert_eq!(len, rpn.num_groups());
        for (i, visitor) in rpn.groups().enumerate() {
            write_sexpr(&mut sexpr, visitor);
            if i + 1 != len {
                sexpr.push(' ');
            }
        }
        sexpr
    }

    #[test]
    fn test_empty() {
        assert_eq!(to_tree(""), "");
    }

    #[test]
    fn test_many_groups() {
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
}
