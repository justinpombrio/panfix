use crate::lexing::Token;

pub type Prec = u32;

#[derive(Debug, Clone)]
pub struct Rule<T: Token> {
    pub name: String,
    pub left_prec: Option<Prec>,
    pub right_prec: Option<Prec>,
    pub tokens: Vec<T>,
}

#[derive(Debug, Clone)]
pub struct Shunter<T: Token> {
    // Map from the first token in a Prefix or Nilfix rule, to that rule.
    pub(super) token_to_prefixy_rule: Vec<Option<Rule<T>>>,
    // Map from the first token in a Suffix or Infix rule, to that rule.
    pub(super) token_to_suffixy_rule: Vec<Option<Rule<T>>>,
    pub(super) missing_atom: Rule<T>,
    pub(super) juxtapose: Rule<T>,
}

impl<'g, T: Token> Rule<T> {
    pub fn arity(&self) -> usize {
        let mut arity = self.num_holes();
        if self.left_prec.is_some() {
            arity += 1;
        }
        if self.right_prec.is_some() {
            arity += 1;
        }
        arity
    }

    pub fn num_holes(&self) -> usize {
        self.tokens.len() - 1
    }
}

impl<'g, T: Token> Shunter<T> {
    pub fn new(rules: Vec<Rule<T>>) -> Shunter<T> {
        let mut largest_token: usize = 0;
        for rule in &rules {
            for token in &rule.tokens {
                largest_token = largest_token.max(token.as_usize());
            }
        }
        let mut token_to_prefixy_rule = vec![None; largest_token + 1];
        let mut token_to_suffixy_rule = vec![None; largest_token + 1];

        let mut juxtapose_prec = (0, 0);
        for rule in rules {
            if &rule.name == "$Juxtapose" {
                assert_eq!(rule.tokens.len(), 0);
                juxtapose_prec = (rule.left_prec.unwrap(), rule.right_prec.unwrap());
                continue;
            }
            assert!(!rule.tokens.is_empty());
            let token = rule.tokens.first().unwrap();
            let index = token.as_usize();
            if rule.left_prec.is_none() {
                assert!(
                    token_to_prefixy_rule[index].is_none(),
                    "Duplicate first rule token"
                );
                token_to_prefixy_rule[index] = Some(rule);
            } else {
                assert!(
                    token_to_suffixy_rule[index].is_none(),
                    "Duplicate first rule token"
                );
                token_to_suffixy_rule[index] = Some(rule);
            }
        }
        let missing_atom = Rule {
            name: "$MissingAtom".to_owned(),
            left_prec: None,
            right_prec: None,
            tokens: vec![T::MISSING_ATOM],
        };
        let juxtapose = Rule {
            name: "$Juxtapose".to_owned(),
            left_prec: Some(juxtapose_prec.0),
            right_prec: Some(juxtapose_prec.1),
            tokens: vec![T::JUXTAPOSE],
        };
        // TODO: unwrap -> Err
        Shunter {
            token_to_prefixy_rule,
            token_to_suffixy_rule,
            missing_atom,
            juxtapose,
        }
    }
}
