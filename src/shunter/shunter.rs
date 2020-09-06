use crate::lexer::Token;

pub type Prec = u32;

#[derive(Debug, Clone)]
pub struct Operator<T: Token> {
    pub name: String,
    pub left_prec: Option<Prec>,
    pub right_prec: Option<Prec>,
    pub tokens: Vec<T>,
}

#[derive(Debug, Clone)]
pub struct Shunter<T: Token> {
    // Map from the first token in an operator, to that operator
    pub(super) token_to_op: Vec<Option<Operator<T>>>,
    pub(super) missing_atom: Operator<T>,
    pub(super) missing_sep: Operator<T>,
    pub(super) extra_sep: Operator<T>,
    pub(super) juxtapose: Operator<T>,
    pub(super) lex_error: Operator<T>,
}

impl<'g, T: Token> Operator<T> {
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
    pub fn new(ops: Vec<Operator<T>>, juxtapose_prec: Option<(Prec, Prec)>) -> Shunter<T> {
        let mut largest_token: usize = 0;
        for op in &ops {
            for token in &op.tokens {
                largest_token = largest_token.max(token.as_usize());
            }
        }
        let mut token_to_op = vec![None; largest_token + 1];

        let missing_atom = Operator {
            name: "$MissingAtom".to_owned(),
            left_prec: None,
            right_prec: None,
            tokens: vec![T::MISSING_ATOM],
        };
        let missing_sep = Operator {
            name: "$MissingSeparator".to_owned(),
            left_prec: Some(0),
            right_prec: None,
            tokens: vec![T::MISSING_SEP],
        };
        let extra_sep = Operator {
            name: "$ExtraSeparator".to_owned(),
            left_prec: Some(0),
            right_prec: None,
            tokens: vec![T::EXTRA_SEP],
        };
        let lex_error = Operator {
            name: "$LexError".to_owned(),
            left_prec: Some(0),
            right_prec: None,
            tokens: vec![T::LEX_ERROR],
        };
        let juxtapose_prec = if let Some((lprec, rprec)) = juxtapose_prec {
            (lprec, rprec)
        } else {
            (0, 0)
        };
        let juxtapose = Operator {
            name: "$Juxtapose".to_owned(),
            left_prec: Some(juxtapose_prec.0),
            right_prec: Some(juxtapose_prec.1),
            tokens: vec![T::JUXTAPOSE],
        };
        for op in ops {
            assert!(!op.tokens.is_empty());
            let token = op.tokens.first().unwrap();
            let index = token.as_usize();
            token_to_op[index] = Some(op);
        }
        // TODO: unwrap -> Err
        Shunter {
            token_to_op,
            missing_atom,
            missing_sep,
            extra_sep,
            juxtapose,
            lex_error,
        }
    }
}
