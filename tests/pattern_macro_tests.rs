use panfix::{pattern, Fixity, Pattern};

#[test]
fn test_pattern_macro() {
    use Fixity::{Infix, Nilfix, Prefix, Suffix};

    assert_eq!(
        pattern!("null"),
        Pattern {
            fixity: Nilfix,
            tokens: vec!["null".to_string()],
        }
    );

    assert_eq!(
        pattern!("(" ")"),
        Pattern {
            fixity: Nilfix,
            tokens: vec!["(".to_string(), ")".to_string()],
        }
    );

    assert_eq!(
        pattern!("-" _),
        Pattern {
            fixity: Prefix,
            tokens: vec!["-".to_string()],
        }
    );

    assert_eq!(
        pattern!("if" "then" "else" _),
        Pattern {
            fixity: Prefix,
            tokens: vec!["if".to_string(), "then".to_string(), "else".to_string()],
        }
    );

    assert_eq!(
        pattern!(_ "++"),
        Pattern {
            fixity: Suffix,
            tokens: vec!["++".to_string()]
        }
    );

    assert_eq!(
        pattern!(_ "[" "]"),
        Pattern {
            fixity: Suffix,
            tokens: vec!["[".to_string(), "]".to_string()],
        }
    );

    assert_eq!(
        pattern!(_ "+" _),
        Pattern {
            fixity: Infix,
            tokens: vec!["+".to_string()],
        }
    );

    assert_eq!(
        pattern!(_ "?" ":" _),
        Pattern {
            fixity: Infix,
            tokens: vec!["?".to_string(), ":".to_string()],
        }
    );
}
