use super::grammar::{Assoc, Fixity, Grammar, NonTerminal, Op, Prec, Subgrammar, Token};
use regex::Regex;
use std::collections::HashMap;

/// White space, according to the Pattern_White_Space Unicode property.
const WHITESPACE_REGEX: &str =
    "[\\u0009\\u000A\\u000B\\u000C\\u000D\\u0020\\u0085\\u200E\\u200F\\u2028\\u2029]*";

pub struct GrammarBuilder {
    grammar: Grammar,
    next_token: usize,
    constants: HashMap<String, Token>,
}

impl GrammarBuilder {
    pub fn new() -> Self {
        GrammarBuilder {
            next_token: 1,
            constants: HashMap::new(),
            grammar: Grammar {
                whitespace: Regex::new(WHITESPACE_REGEX).unwrap(),
                regexes: vec![],
                constants: vec![],
                token_display: vec!["LEX_ERROR".to_owned()],
                subgrammars: HashMap::new(),
            },
        }
    }

    pub fn whitespace(mut self, whitespace_regex: &str) -> Self {
        self.grammar.whitespace = Regex::new(whitespace_regex).unwrap();
        self
    }

    pub fn subgrammar(self, name: &str) -> SubgrammarBuilder {
        let missing_atom = Op::new_atom("$MissingAtom", None);
        let juxtapose = Op::new("$Juxtapose", 0, Assoc::Right, None, vec![], Fixity::Infix);
        SubgrammarBuilder {
            grammar_builder: self,
            subgrammar: Subgrammar {
                name: name.to_owned(),
                token_to_prefixy_op: vec![None],
                token_to_suffixy_op: vec![None],
                missing_atom,
                juxtapose,
            },
            prec_level: 1,
        }
    }

    fn new_token(&mut self) -> Token {
        let token = self.next_token;
        self.next_token += 1;
        token
    }

    fn insert_regex(&mut self, name: &str, regex: &str) -> Token {
        let regex = Regex::new(regex).unwrap();
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

    pub fn ops_l(self) -> OpsBuilder {
        OpsBuilder {
            subgrammar_builder: self,
            assoc: Assoc::Left,
        }
    }

    pub fn ops_r(self) -> OpsBuilder {
        OpsBuilder {
            subgrammar_builder: self,
            assoc: Assoc::Right,
        }
    }

    pub fn done(mut self) -> GrammarBuilder {
        self.grammar_builder
            .grammar
            .subgrammars
            .insert(self.subgrammar.name.clone(), self.subgrammar);
        self.grammar_builder
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
}

pub struct OpsBuilder {
    subgrammar_builder: SubgrammarBuilder,
    assoc: Assoc,
}

impl OpsBuilder {
    pub fn nilfix(mut self, name: &str, token: &str, followers: Vec<(NonTerminal, &str)>) -> Self {
        self.add_op(name, token, followers, Fixity::Nilfix);
        self
    }

    pub fn prefix(mut self, name: &str, token: &str, followers: Vec<(NonTerminal, &str)>) -> Self {
        self.add_op(name, token, followers, Fixity::Prefix);
        self
    }

    pub fn suffix(mut self, name: &str, token: &str, followers: Vec<(NonTerminal, &str)>) -> Self {
        self.add_op(name, token, followers, Fixity::Suffix);
        self
    }

    pub fn infix(mut self, name: &str, token: &str, followers: Vec<(NonTerminal, &str)>) -> Self {
        self.add_op(name, token, followers, Fixity::Infix);
        self
    }

    pub fn done(mut self) -> SubgrammarBuilder {
        self.subgrammar_builder.prec_level += 1;
        self.subgrammar_builder
    }

    fn add_op(
        &mut self,
        name: &str,
        constant: &str,
        followers: Vec<(NonTerminal, &str)>,
        fixity: Fixity,
    ) {
        let token = self
            .subgrammar_builder
            .grammar_builder
            .insert_constant(constant);
        let followers = followers
            .into_iter()
            .map(|(nonterminal, constant)| {
                let token = self
                    .subgrammar_builder
                    .grammar_builder
                    .insert_constant(constant);
                (nonterminal, token)
            })
            .collect();
        let op = Op::new(
            name,
            self.subgrammar_builder.prec_level,
            self.assoc,
            Some(token),
            followers,
            fixity,
        );
        self.subgrammar_builder.add_op(op)
    }
}
