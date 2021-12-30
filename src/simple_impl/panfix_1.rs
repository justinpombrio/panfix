use super::lexer::{LexError, Lexeme, Lexer, Token};
use std::fmt;
use std::iter::Peekable;

pub type Prec = u16;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Assoc {
    Left,
    Right,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Fixity {
    Nilfix,
    Prefix,
    Suffix,
    Infix,
}

#[derive(Debug, Clone)]
pub struct Op {
    name: String,
    fixity: Fixity,
    prec: Prec,
    assoc: Assoc,
    token: Token,
    // computed
    left_prec: Option<Prec>,
    right_prec: Option<Prec>,
}

impl Op {
    pub fn new(name: &str, fixity: Fixity, prec: Prec, assoc: Assoc, token: Token) -> Op {
        use Assoc::*;
        use Fixity::*;

        let (left_prec, right_prec) = match (fixity, assoc) {
            (Nilfix, _) => (None, None),
            (Prefix, Left) => (None, Some(prec - 1)),
            (Prefix, Right) => (None, Some(prec)),
            (Suffix, Left) => (Some(prec), None),
            (Suffix, Right) => (Some(prec - 1), None),
            (Infix, Left) => (Some(prec), Some(prec - 1)),
            (Infix, Right) => (Some(prec - 1), Some(prec)),
        };
        Op {
            name: name.to_owned(),
            fixity,
            prec,
            assoc,
            token,
            left_prec,
            right_prec,
        }
    }
}

// NOTICE:
// - if Nilfix, prec and assoc doesn't matter.

#[derive(Debug, Clone)]
pub struct Grammar {
    lexer: Lexer,
    ops: Vec<Option<Op>>,
}

impl Grammar {
    pub fn new(lexer: Lexer, ops: Vec<Op>) -> Grammar {
        let max_token = ops.iter().map(|op| op.token).max().unwrap_or(0);
        let mut op_table = (0..=max_token).map(|_| None).collect::<Vec<_>>();
        for op in ops {
            let token = op.token;
            op_table[token] = Some(op);
        }
        Grammar {
            lexer,
            ops: op_table,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ParseTree<'s> {
    op: &'s Op,
    lexeme: &'s str,
    children: Vec<ParseTree<'s>>,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    LexError,
    MissingExpr,
    MissingOp,
}

#[derive(Debug)]
struct Parser<'s, I: Iterator<Item = Result<Lexeme<'s>, LexError>>> {
    lexemes: Peekable<I>,
    grammar: &'s Grammar,
}

impl<'s, I: Iterator<Item = Result<Lexeme<'s>, LexError>>> Parser<'s, I> {
    /// Parse a full expression at the given precedence level.
    fn parse_expr(&mut self, prec: Prec) -> Result<ParseTree<'s>, ParseError> {
        // E.g. we've parsed "1 +" so far, and are looking for what comes after the "+".
        match self.lexemes.peek().copied() {
            Some(Ok(lexeme)) => {
                let op = self.grammar.ops[lexeme.token].as_ref().unwrap();
                if op.left_prec.is_none() {
                    // e.g. "1 + -2"
                    self.lexemes.next();
                    let expr = self.parse_op(None, op, lexeme.substr)?;
                    self.parse_suffix(expr, prec)
                } else {
                    // e.g. "1 + + 2"
                    Err(ParseError::MissingExpr)
                }
            }
            // e.g. "1 + @%!$$", if not parsing Perl
            Some(Err(_)) => Err(ParseError::LexError),
            // e.g. "1 + EOF"
            None => Err(ParseError::MissingExpr),
        }
    }

    /// Finish parsing an operator. It's left argument must be provided, and its right argument (if
    /// any) will be parsed.
    fn parse_op(
        &mut self,
        left_arg: Option<ParseTree<'s>>,
        op: &'s Op,
        lexeme: &'s str,
    ) -> Result<ParseTree<'s>, ParseError> {
        let mut expr = ParseTree {
            op,
            lexeme,
            children: vec![],
        };
        if let Some(left_arg) = left_arg {
            expr.children.push(left_arg);
        }
        if let Some(prec) = op.right_prec {
            let right_arg = self.parse_expr(prec)?;
            expr.children.push(right_arg);
        }
        Ok(expr)
    }

    /// Extend an expression to the right, staying within the given precedence level.
    fn parse_suffix(
        &mut self,
        expr: ParseTree<'s>,
        prec: Prec,
    ) -> Result<ParseTree<'s>, ParseError> {
        // E.g. we've parsed "1 + 2" so far. `expr` is 2, and `prec` is the right_prec of `+`.
        // We're looking to see if the expr 2 has a suffix, like "* 3", that should be grouped
        // together with it under the "+".

        match self.lexemes.peek().copied() {
            Some(Ok(lexeme)) => {
                let op = self.grammar.ops[lexeme.token].as_ref().unwrap();
                if let Some(left_prec) = op.left_prec {
                    if left_prec <= prec {
                        // e.g. "1 + 2 *"
                        self.lexemes.next();
                        let bigger_expr = self.parse_op(Some(expr), op, lexeme.substr)?;
                        self.parse_suffix(bigger_expr, prec)
                    } else {
                        // e.g. "1 + 2 <"
                        Ok(expr)
                    }
                } else {
                    // e.g. "1 + 2 3"
                    Err(ParseError::MissingOp)
                }
            }
            // e.g. "1 + 2 @%!$$"
            Some(Err(_)) => Err(ParseError::LexError),
            // e.g. "1 + 2 EOF"
            None => Ok(expr),
        }
    }
}

impl Grammar {
    pub fn parse<'s>(&'s self, source: &'s str) -> Result<ParseTree<'s>, ParseError> {
        let grammar = self;
        let lexemes = self.lexer.lex(source).peekable();
        let mut parser = Parser { grammar, lexemes };
        parser.parse_expr(Prec::MAX)
    }
}

impl<'s> fmt::Display for ParseTree<'s> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.children.is_empty() {
            write!(f, "({} {})", self.op.name, self.lexeme)
        } else {
            write!(f, "(")?;
            write!(f, "{}", self.op.name)?;
            for child in &self.children {
                write!(f, " {}", child)?;
            }
            write!(f, ")")
        }
    }
}

#[test]
fn test_parsing() {
    use Assoc::{Left, Right};
    use Fixity::{Infix, Nilfix, Prefix};

    fn display(result: Result<ParseTree, ParseError>) -> String {
        match result {
            Ok(expr) => format!("{}", expr),
            Err(err) => format!("{:?}", err),
        }
    }

    let mut lexer = Lexer::new("[ \t\r\n]+");
    let int_token = lexer.add_regex("[0-9]|[1-9][0-9]*");
    let var_token = lexer.add_regex("[a-zA-Z_]+");
    let neg_token = lexer.add_string("-");
    let plus_token = lexer.add_string("+");
    let less_token = lexer.add_string("<");
    let greater_token = lexer.add_string(">");
    let assign_token = lexer.add_string("=");

    let grammar = Grammar::new(
        lexer,
        vec![
            // Prec and assoc don't matter for Nilfix ops.
            Op::new("int", Nilfix, 0, Left, int_token),
            Op::new("var", Nilfix, 0, Left, var_token),
            // Unary minus. We'll see later how to _additionally_ have binary minus.
            Op::new("neg", Prefix, 10, Left, neg_token),
            // Binary operators.
            Op::new("plus", Infix, 20, Left, plus_token),
            Op::new("less", Infix, 30, Left, less_token),
            Op::new("greater", Infix, 30, Left, greater_token),
            Op::new("assign", Infix, 40, Right, assign_token),
        ],
    );

    // Basics
    assert_eq!(display(grammar.parse("3")), "(int 3)");
    assert_eq!(display(grammar.parse("- 2")), "(neg (int 2))");
    assert_eq!(display(grammar.parse("1 + 2")), "(plus (int 1) (int 2))");

    // Associativity
    assert_eq!(
        display(grammar.parse("1 + 2 + 3")),
        "(plus (plus (int 1) (int 2)) (int 3))"
    );
    assert_eq!(
        display(grammar.parse("x = y = 0")),
        "(assign (var x) (assign (var y) (int 0)))"
    );

    // Precedence
    assert_eq!(
        display(grammar.parse("1 + 1 < 3")),
        "(less (plus (int 1) (int 1)) (int 3))"
    );
    assert_eq!(
        display(grammar.parse("1 < 3 + 3")),
        "(less (int 1) (plus (int 3) (int 3)))"
    );
    assert_eq!(
        display(grammar.parse("- 1 + - 2")),
        "(plus (neg (int 1)) (neg (int 2)))"
    );
    assert_eq!(
        display(grammar.parse("1 + -2 > -2 + 3 + -4")),
        "(greater (plus (int 1) (neg (int 2))) (plus (plus (neg (int 2)) (int 3)) (neg (int 4))))"
    );

    // Nested
    assert_eq!(display(grammar.parse("---3")), "(neg (neg (neg (int 3))))");

    // Parse errors
    assert_eq!(display(grammar.parse("-")), "MissingExpr");
    assert_eq!(display(grammar.parse("+")), "MissingExpr");
    assert_eq!(display(grammar.parse("")), "MissingExpr");
    assert_eq!(display(grammar.parse("1 2")), "MissingOp");
    assert_eq!(display(grammar.parse("1 + $#$#@")), "LexError");

    // Example to give:
    let parse_tree = grammar.parse("-1 + 2 < 3").unwrap();
    assert_eq!(
        format!("{}", parse_tree),
        "(less (plus (neg (int 1)) (int 2)) (int 3))"
    );
}
