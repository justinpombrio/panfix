use panfix::{pattern, Fixity, Pattern};

#[test]
fn test_pattern_macro() {
    use Fixity::{InfixL, InfixR, Nilfix, Prefix, Suffix};

    assert_eq!(
        pattern!("null"),
        Pattern {
            fixity: Nilfix,
            first_token: "null",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!("(" Expr ")"),
        Pattern {
            fixity: Nilfix,
            first_token: "(",
            followers: vec![("Expr", ")")],
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
        pattern!("if" Expr "then" Stmt "else" _),
        Pattern {
            fixity: Prefix,
            first_token: "if",
            followers: vec![("Expr", "then"), ("Stmt", "else")],
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
        pattern!(_ "[" Expr "]"),
        Pattern {
            fixity: Suffix,
            first_token: "[",
            followers: vec![("Expr", "]")],
        }
    );

    assert_eq!(
        pattern!(_ "+" _),
        Pattern {
            fixity: InfixL,
            first_token: "+",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!(_ "?" Expr ":" _),
        Pattern {
            fixity: InfixL,
            first_token: "?",
            followers: vec![("Expr", ":")],
        }
    );

    assert_eq!(
        pattern!(_ "+" _ infixl),
        Pattern {
            fixity: InfixL,
            first_token: "+",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!(_ "?" Expr ":" _ infixl),
        Pattern {
            fixity: InfixL,
            first_token: "?",
            followers: vec![("Expr", ":")],
        }
    );

    assert_eq!(
        pattern!(_ "+" _ infixr),
        Pattern {
            fixity: InfixR,
            first_token: "+",
            followers: vec![],
        }
    );

    assert_eq!(
        pattern!(_ "?" Expr ":" _ infixr),
        Pattern {
            fixity: InfixR,
            first_token: "?",
            followers: vec![("Expr", ":")],
        }
    );
}
