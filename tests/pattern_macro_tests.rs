use panfix::{pattern, Fixity, Pattern};

#[test]
fn test_pattern_macro() {
    use Fixity::{Infix, Nilfix, Prefix, Suffix};

    assert_eq!(
        pattern!("null"),
        Pattern {
            fixity: Nilfix,
            first_token: "null",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!("(" ")"),
        Pattern {
            fixity: Nilfix,
            first_token: "(",
            followers: vec![")"],
        }
    );

    assert_eq!(
        pattern!("-" _),
        Pattern {
            fixity: Prefix,
            first_token: "-",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!("if" "then" "else" _),
        Pattern {
            fixity: Prefix,
            first_token: "if",
            followers: vec!["then", "else"],
        }
    );

    assert_eq!(
        pattern!(_ "++"),
        Pattern {
            fixity: Suffix,
            first_token: "++",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!(_ "[" "]"),
        Pattern {
            fixity: Suffix,
            first_token: "[",
            followers: vec!["]"],
        }
    );

    assert_eq!(
        pattern!(_ "+" _),
        Pattern {
            fixity: Infix,
            first_token: "+",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!(_ "?" ":" _),
        Pattern {
            fixity: Infix,
            first_token: "?",
            followers: vec![":"],
        }
    );
}
