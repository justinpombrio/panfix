use crate::lexing::Lexer;
use crate::lexing::Token as TokenTrait;
use crate::shunting::OpSpec as RealOpSpec;
use crate::shunting::{Assoc, Fixity, Grammar as ShuntingGrammar, Prec};
use std::collections::HashMap;

// TODO: Op -> CompiledOp, shunting::OpSpec -> Op, OpSpec -> Op

/// White space, according to the Pattern_White_Space Unicode property.
const WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]*";

pub type RegexPattern = String;

#[doc(hidden)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    LexError,
    Normal(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpSpec {
    pub name: String,
    pub fixity: Fixity,
    pub first_token: String,
    pub followers: Vec<(String, String)>,
}

#[derive(Debug)]
pub struct Grammar {
    language_name: String,
    whitespace: String,
    token_set: TokenSet,
    ops: Vec<RealOpSpec<Token>>,
    nonterminals: Vec<String>,
}

#[derive(Debug)]
pub struct Subgrammar<'a> {
    name: String,
    ops: Vec<RealOpSpec<Token>>,
    prec: Prec,
    token_set: &'a mut TokenSet,
}

#[derive(Debug)]
pub struct Parser {
    pub(super) lexer: Lexer<Token>,
    pub(super) shunter: ShuntingGrammar<Token>,
}

#[derive(Debug)]
struct TokenSet {
    next_token: u32,
    regexes: Vec<(String, RegexPattern, Token)>,
    constants: HashMap<String, Token>,
}

impl OpSpec {
    pub fn juxtapose() -> OpSpec {
        OpSpec {
            name: "$Juxtapose".to_owned(),
            fixity: Fixity::Infix,
            first_token: "".to_owned(),
            followers: vec![],
        }
    }
}

impl TokenTrait for Token {
    const LEX_ERROR: Token = Token::LexError;

    fn as_usize(self) -> usize {
        use Token::*;
        match self {
            LexError => 0,
            Normal(tok) => tok as usize + 1,
        }
    }
}

impl Grammar {
    pub fn new(language_name: &str) -> Grammar {
        Grammar {
            language_name: language_name.to_owned(),
            whitespace: WHITESPACE_REGEX.to_owned(),
            token_set: TokenSet::new(),
            ops: vec![],
            nonterminals: vec![],
        }
    }

    pub fn with_whitespace(mut self, whitespace: &str) -> Self {
        self.whitespace = whitespace.to_owned();
        self
    }

    pub fn regex(mut self, name: &str, regex: &str) -> Self {
        let token = self
            .token_set
            .insert_regex(name.to_owned(), regex.to_owned());
        self.ops.push(RealOpSpec {
            nonterminal: "".to_owned(), // sentinal value
            name: name.to_owned(),
            fixity: Fixity::Nilfix,
            assoc: Assoc::Left, // irrelevant
            first_token: Some(token),
            followers: vec![],
            prec: 0,
        });
        self
    }

    pub fn constant(mut self, name: &str, constant: &str) -> Self {
        let token = self.token_set.insert_constant(constant.to_owned());
        self.ops.push(RealOpSpec {
            nonterminal: "".to_owned(), // sentinal value
            name: name.to_owned(),
            fixity: Fixity::Nilfix,
            assoc: Assoc::Left, // irrelevant
            first_token: Some(token),
            followers: vec![],
            prec: 0,
        });
        self
    }

    pub fn subgrammar(mut self, name: &str, build: impl Fn(Subgrammar) -> Subgrammar) -> Self {
        assert!(!name.is_empty());
        let mut subgrammar = Subgrammar::new(name, &mut self.token_set);
        subgrammar = build(subgrammar);
        self.ops.extend(subgrammar.ops);
        self.nonterminals.push(name.to_owned());
        self
    }

    // TODO: Errors
    pub fn build(self, starting_nonterminal: &str) -> Parser {
        let lexer = self.token_set.into_lexer(self.whitespace);
        let mut ops = vec![];
        for op in self.ops {
            for (nonterminal, _) in &op.followers {
                if !self.nonterminals.contains(nonterminal) {
                    panic!("No such subgrammar: {}", nonterminal);
                }
            }
            if op.nonterminal.is_empty() {
                // These are atoms made with `regex` or `constant`, which should be shared across
                // all subgrammars.
                for nonterminal in &self.nonterminals {
                    let mut op = op.clone();
                    op.nonterminal = nonterminal.to_owned();
                    ops.push(op);
                }
            } else {
                ops.push(op);
            }
        }
        let shunter =
            ShuntingGrammar::new(self.language_name, starting_nonterminal.to_owned(), ops);
        Parser { lexer, shunter }
    }
}

impl<'a> Subgrammar<'a> {
    fn new(name: &str, token_set: &'a mut TokenSet) -> Subgrammar<'a> {
        Subgrammar {
            name: name.to_owned(),
            ops: vec![],
            prec: 1,
            token_set,
        }
    }

    pub fn op_l(mut self, op: OpSpec) -> Self {
        assert_ne!(op.fixity, Fixity::Nilfix);
        self.add_op(op, Assoc::Left);
        self.prec += 1;
        self
    }

    pub fn op_r(mut self, op: OpSpec) -> Self {
        assert_ne!(op.fixity, Fixity::Nilfix);
        self.add_op(op, Assoc::Right);
        self.prec += 1;
        self
    }

    pub fn op(mut self, op: OpSpec) -> Self {
        assert_ne!(op.fixity, Fixity::Infix);
        if op.fixity == Fixity::Nilfix {
            assert_eq!(
                self.prec, 1,
                "For clarity, please list nilfix operators like {} first.",
                op.name
            );
        }
        self.add_op(op, Assoc::Left);
        self
    }

    pub fn ops_l(mut self, ops: Vec<OpSpec>) -> Self {
        for op in ops {
            assert_ne!(op.fixity, Fixity::Nilfix);
            self.add_op(op, Assoc::Left);
        }
        self.prec += 1;
        self
    }

    pub fn ops_r(mut self, ops: Vec<OpSpec>) -> Self {
        for op in ops {
            assert_ne!(op.fixity, Fixity::Nilfix);
            self.add_op(op, Assoc::Right);
        }
        self.prec += 1;
        self
    }

    fn add_op(&mut self, op: OpSpec, assoc: Assoc) {
        let mut followers = vec![];
        for (nonterminal, constant) in op.followers {
            let token = self.token_set.insert_constant(constant);
            followers.push((nonterminal, token));
        }
        let first_token = if op.first_token.is_empty() {
            assert_eq!(op.name, "$Juxtapose");
            None
        } else {
            Some(self.token_set.insert_constant(op.first_token))
        };
        let prec = if op.fixity == Fixity::Nilfix {
            0
        } else {
            self.prec
        };
        self.ops.push(RealOpSpec {
            nonterminal: self.name.clone(),
            name: op.name,
            fixity: op.fixity,
            assoc,
            first_token,
            followers,
            prec,
        })
    }
}

impl TokenSet {
    fn new() -> TokenSet {
        TokenSet {
            next_token: 0,
            regexes: Vec::new(),
            constants: HashMap::new(),
        }
    }

    fn new_token(&mut self) -> Token {
        let token = Token::Normal(self.next_token);
        self.next_token += 1;
        token
    }

    fn insert_regex(&mut self, name: String, regex: RegexPattern) -> Token {
        let token = self.new_token();
        self.regexes.push((name, regex, token));
        token
    }

    fn insert_constant(&mut self, constant: String) -> Token {
        if constant.is_empty() {
            panic!("Constants cannot be empty! Add at least one character to the token.");
        }
        if let Some(token) = self.constants.get(&constant) {
            *token
        } else {
            let token = self.new_token();
            self.constants.insert(constant.clone(), token);
            token
        }
    }

    fn into_lexer(self, whitespace: RegexPattern) -> Lexer<Token> {
        Lexer::new(
            whitespace,
            self.regexes,
            self.constants.into_iter().collect(),
        )
        .unwrap()
    }
}

#[macro_export]
macro_rules! juxtapose {
    () => {
        $crate::parsing::OpSpec::juxtapose()
    };
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
        $crate::parsing::OpSpec {
            name: stringify!($name).to_owned(),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Infix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident Y $token:literal [ $($followers:tt)* ]) => {
        $crate::parsing::OpSpec {
            name: stringify!($name).to_owned(),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Suffix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ] _) => {
        $crate::parsing::OpSpec {
            name: stringify!($name).to_owned(),
            first_token: $token.to_owned(),
            fixity: $crate::parsing::Fixity::Prefix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ]) => {
        $crate::parsing::OpSpec {
            name: stringify!($name).to_owned(),
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
        OpSpec {
            name: "Plus".to_owned(),
            first_token: "+".to_owned(),
            fixity: Fixity::Infix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Apply: _ "(" Expr ")"),
        OpSpec {
            name: "Apply".to_owned(),
            first_token: "(".to_owned(),
            fixity: Fixity::Suffix,
            followers: vec![("Expr".to_owned(), ")".to_owned())]
        }
    );
    assert_eq!(
        op!(Neg: "-" _),
        OpSpec {
            name: "Neg".to_owned(),
            first_token: "-".to_owned(),
            fixity: Fixity::Prefix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Zero: "0"),
        OpSpec {
            name: "Zero".to_owned(),
            first_token: "0".to_owned(),
            fixity: Fixity::Nilfix,
            followers: vec![]
        }
    );
    assert_eq!(
        op!(Ifte: "if" Expr1 "then" Expr2 "end"),
        OpSpec {
            name: "Ifte".to_owned(),
            first_token: "if".to_owned(),
            fixity: Fixity::Nilfix,
            followers: vec![
                ("Expr1".to_owned(), "then".to_owned()),
                ("Expr2".to_owned(), "end".to_owned())
            ],
        }
    );
}
