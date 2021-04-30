use crate::shunting::{Fixity, OpName};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Op<N: OpName> {
    pub name: N,
    pub fixity: Fixity,
    pub first_token: String,
    pub followers: Vec<(String, String)>,
}

#[macro_export]
macro_rules! op {
    ($name:ident : _ $token:literal $($followers:tt)*) => {
        op!(@ $name Y $token [ ] $($followers)*)
    };

    ($name:ident : $token:literal $($followers:tt)*) => {
        op!(@ $name N $token [ ] $($followers)*)
    };

    (@ $name:ident $l:ident $token:literal [ $($followers:tt)* ] $nt:ident $tok:literal $($rest:tt)*) => {
        op!(@ $name $l $token [ $($followers)* (std::stringify!($nt).to_owned(), $tok.to_owned()), ] $($rest)*)
    };

    (@ $name:ident Y $token:literal [ $($followers:tt)* ] _) => {
        $crate::parsing::Op {
            name: stringify!($name),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Infix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident Y $token:literal [ $($followers:tt)* ]) => {
        $crate::parsing::Op {
            name: stringify!($name),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Suffix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ] _) => {
        $crate::parsing::Op {
            name: stringify!($name),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Prefix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ]) => {
        $crate::parsing::Op {
            name: stringify!($name),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Nilfix,
            followers: vec![$($followers)*],
        }
    };
}

#[test]
fn test_macro() {
    assert_eq!(
        op!(Plus: _ "+" _),
        Op {
            name: "Plus",
            first_token: "+".to_owned(),
            fixity: Fixity::Infix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Apply: _ "(" Expr ")"),
        Op {
            name: "Apply",
            first_token: "(".to_owned(),
            fixity: Fixity::Suffix,
            followers: vec![("Expr".to_owned(), ")".to_owned())]
        }
    );
    assert_eq!(
        op!(Neg: "-" _),
        Op {
            name: "Neg",
            first_token: "-".to_owned(),
            fixity: Fixity::Prefix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Zero: "0"),
        Op {
            name: "Zero",
            first_token: "0".to_owned(),
            fixity: Fixity::Nilfix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Ifte: "if" Expr1 "then" Expr2 "end"),
        Op {
            name: "Ifte",
            first_token: "if".to_owned(),
            fixity: Fixity::Nilfix,
            followers: vec![
                ("Expr1".to_owned(), "then".to_owned()),
                ("Expr2".to_owned(), "end".to_owned())
            ],
        }
    );
}
