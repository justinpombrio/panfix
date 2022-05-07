use super::grammar::{Assoc, Fixity, Grammar, NonTerminal, Op, Prec, Subgrammar, Token};
use regex::Regex;
use std::collections::HashMap;
use std::default::Default;

/// White space, according to the Pattern_White_Space Unicode property.
const WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]*";

#[derive(Debug)]
pub struct GrammarBuilder {
    grammar: Grammar,
    next_token: usize,
    constants: HashMap<String, Token>,
}

impl Default for GrammarBuilder {
    fn default() -> GrammarBuilder {
        GrammarBuilder::new()
    }
}

impl GrammarBuilder {
    pub fn new() -> Self {
        GrammarBuilder {
            next_token: 1,
            constants: HashMap::new(),
            grammar: Grammar {
                whitespace: new_regex(WHITESPACE_REGEX),
                regexes: vec![],
                constants: vec![],
                token_display: vec!["LEX_ERROR".to_owned()],
                subgrammars: HashMap::new(),
            },
        }
    }

    pub fn whitespace(mut self, whitespace_regex: &str) -> Self {
        self.grammar.whitespace = new_regex(whitespace_regex);
        self
    }

    pub fn subgrammar<F>(self, name: &str, build: F) -> Self
    where
        F: FnOnce(SubgrammarBuilder) -> SubgrammarBuilder,
    {
        let missing_atom = Op::new_atom("$MissingAtom", None);
        let juxtapose = Op::new("$Juxtapose", 1, Assoc::Right, None, vec![], Fixity::Infix);
        let builder = SubgrammarBuilder {
            grammar_builder: self,
            subgrammar: Subgrammar {
                name: name.to_owned(),
                token_to_prefixy_op: vec![None],
                token_to_suffixy_op: vec![None],
                missing_atom,
                juxtapose,
            },
            prec_level: 2,
        };
        build(builder).done()
    }

    pub fn build(self) -> Grammar {
        self.grammar
    }

    fn new_token(&mut self) -> Token {
        let token = self.next_token;
        self.next_token += 1;
        token
    }

    fn insert_regex(&mut self, name: &str, regex: &str) -> Token {
        let regex = new_regex(regex);
        let token = self.new_token();
        self.grammar.regexes.push((regex, token));
        self.grammar.token_display.push(name.to_owned());
        token
    }

    fn insert_constant(&mut self, constant: &str) -> Token {
        if let Some(token) = self.constants.get(constant) {
            *token
        } else {
            let token = self.new_token();
            self.constants.insert(constant.to_owned(), token);
            self.grammar.constants.push((constant.to_owned(), token));
            token
        }
    }
}

pub struct SubgrammarBuilder {
    grammar_builder: GrammarBuilder,
    subgrammar: Subgrammar,
    prec_level: Prec,
}

impl SubgrammarBuilder {
    pub fn regex(mut self, name: &str, regex: &str) -> Self {
        let token = self.grammar_builder.insert_regex(name, regex);
        let op = Op::new_atom(name, Some(token));
        self.add_op(op);
        self
    }

    pub fn constant(mut self, name: &str, constant: &str) -> Self {
        let token = self.grammar_builder.insert_constant(constant);
        let op = Op::new_atom(name, Some(token));
        self.add_op(op);
        self
    }

    pub fn ops_l(mut self, ops: Vec<OpSpec>) -> Self {
        for op in ops {
            self.add_op_spec(op, Assoc::Left);
        }
        self.prec_level += 1;
        self
    }

    pub fn ops_r(mut self, ops: Vec<OpSpec>) -> Self {
        for op in ops {
            self.add_op_spec(op, Assoc::Right);
        }
        self.prec_level += 1;
        self
    }

    fn add_op_spec(&mut self, op: OpSpec, assoc: Assoc) {
        let token = self.grammar_builder.insert_constant(op.token);
        let followers = op
            .followers
            .into_iter()
            .map(|(nonterminal, constant)| {
                let token = self.grammar_builder.insert_constant(constant);
                (nonterminal, token)
            })
            .collect();
        let op = Op::new(
            op.name,
            self.prec_level,
            assoc,
            Some(token),
            followers,
            op.fixity,
        );
        self.add_op(op);
    }

    fn add_op(&mut self, op: Op) {
        use Fixity::*;

        let token = op.token.unwrap();
        while self.subgrammar.token_to_prefixy_op.len() <= token {
            self.subgrammar.token_to_prefixy_op.push(None);
            self.subgrammar.token_to_suffixy_op.push(None);
        }
        match op.fixity {
            Prefix | Nilfix => self.subgrammar.token_to_prefixy_op[token] = Some(op),
            Suffix | Infix => self.subgrammar.token_to_suffixy_op[token] = Some(op),
        }
    }

    fn done(mut self) -> GrammarBuilder {
        self.grammar_builder
            .grammar
            .subgrammars
            .insert(self.subgrammar.name.clone(), self.subgrammar);
        self.grammar_builder
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpSpec {
    pub name: &'static str,
    pub token: &'static str,
    pub fixity: Fixity,
    pub followers: Vec<(NonTerminal, &'static str)>,
}

#[macro_export]
macro_rules! rule {
    ($name:ident : _ $token:literal $($followers:tt)*) => {
        rule!(@ $name Y $token [ ] $($followers)*)
    };

    ($name:ident : $token:literal $($followers:tt)*) => {
        rule!(@ $name N $token [ ] $($followers)*)
    };

    (@ $name:ident $l:ident $token:literal [ $($followers:tt)* ] $nt:ident $tok:literal $($rest:tt)*) => {
        rule!(@ $name $l $token [ $($followers)* (std::stringify!($nt), $tok), ] $($rest)*)
    };

    (@ $name:ident Y $token:literal [ $($followers:tt)* ] _) => {
        $crate::refn_impl::OpSpec {
            name: stringify!($name),
            token: $token,
            fixity: $crate::refn_impl::Fixity::Infix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident Y $token:literal [ $($followers:tt)* ]) => {
        $crate::refn_impl::OpSpec {
            name: stringify!($name),
            token: $token,
            fixity: $crate::refn_impl::Fixity::Suffix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ] _) => {
        $crate::refn_impl::OpSpec {
            name: stringify!($name),
            token: $token,
            fixity: $crate::refn_impl::Fixity::Prefix,
            followers: vec![$($followers)*],
        }
    };

    (@ $name:ident N $token:literal [ $($followers:tt)* ]) => {
        $crate::refn_impl::OpSpec {
            name: stringify!($name),
            token: $token,
            fixity: $crate::refn_impl::Fixity::Nilfix,
            followers: vec![$($followers)*],
        }
    };
}

fn new_regex(regex: &str) -> Regex {
    Regex::new(&format!("^({})", regex)).unwrap()
}

#[test]
fn test_macro() {
    assert_eq!(
        rule!(Plus: _ "+" _),
        OpSpec {
            name: "Plus",
            token: "+",
            fixity: Fixity::Infix,
            followers: vec![]
        }
    );
    assert_eq!(
        rule!(Apply: _ "(" Expr ")"),
        OpSpec {
            name: "Apply",
            token: "(",
            fixity: Fixity::Suffix,
            followers: vec![("Expr", ")")]
        }
    );
    assert_eq!(
        rule!(Neg: "-" _),
        OpSpec {
            name: "Neg",
            token: "-",
            fixity: Fixity::Prefix,
            followers: vec![]
        }
    );
    assert_eq!(
        rule!(Zero: "0"),
        OpSpec {
            name: "Zero",
            token: "0",
            fixity: Fixity::Nilfix,
            followers: vec![]
        }
    );
    assert_eq!(
        rule!(Ifte: "if" Expr1 "then" Expr2 "end"),
        OpSpec {
            name: "Ifte",
            token: "if",
            fixity: Fixity::Nilfix,
            followers: vec![("Expr1", "then"), ("Expr2", "end")],
        }
    );
}
