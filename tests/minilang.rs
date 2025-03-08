use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Visitor};
use std::fmt;
use std::mem;

fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
    let mut grammar = Grammar::new_with_unicode_whitespace()?;
    grammar.regex("id", "[a-zA-Z]+")?;
    grammar.regex("num", "[+-]?[0-9]+")?;
    grammar.op("block", pattern!("{" "}"))?;
    grammar.op("group", pattern!("(" ")"))?;
    grammar.left_assoc();
    grammar.op("call", pattern!(_ "(" ")"))?;
    grammar.left_assoc();
    grammar.op("plus", pattern!(_ "+" _))?;
    grammar.left_assoc();
    grammar.op("lt", pattern!(_ "<" _))?;
    grammar.op("gt", pattern!(_ ">" _))?;
    grammar.op("eq", pattern!(_ "==" _))?;
    grammar.right_assoc();
    grammar.op("if", pattern!("if" "{" "}"))?;
    grammar.op("else", pattern!(_ "else" _))?;
    grammar.left_assoc();
    grammar.op("args", pattern!(_ "," _))?;
    grammar.left_assoc();
    grammar.op("let", pattern!("let" "=" "in" _))?;
    grammar.op("func", pattern!("fn" "(" ")" "{" "}" ";" _))?;
    grammar.finish()
}

#[derive(Debug)]
enum Expr {
    Id(String),
    Num(i32),
    Binop(Binop, Box<Expr>, Box<Expr>),
    /// list of (conditional, consequence)
    IfChain(Vec<(Expr, Expr)>),
    /// Function call
    Call(Box<Expr>, Box<Expr>),
    /// let id = val in body
    Let(String, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
enum Binop {
    Plus,
    Lt,
    Gt,
    Eq,
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Id(id) => write!(f, "{}", id),
            Expr::Num(n) => write!(f, "{}", n),
            Expr::Binop(op, left, right) => {
                let op = match op {
                    Binop::Lt => "<",
                    Binop::Gt => ">",
                    Binop::Eq => "==",
                    Binop::Plus => "+",
                };
                write!(f, "({} {} {})", op, left, right)
            }
            Expr::Call(func, arg) => write!(f, "(call {} {})", func, arg),
            Expr::IfChain(clauses) => {
                write!(f, "(if")?;
                for (cond, consq) in clauses {
                    write!(f, " [{} {}]", cond, consq)?;
                }
                write!(f, ")")
            }
            Expr::Let(id, val, body) => {
                write!(f, "(let {} {} {})", id, val, body)
            }
        }
    }
}

struct Traverser<'s> {
    errors: Vec<ParseError<'s>>,
}

impl<'s> Traverser<'s> {
    fn new() -> Traverser<'s> {
        Traverser { errors: vec![] }
    }

    fn parse(
        mut self,
        visitor: Visitor<'s, '_, '_, &'static str>,
    ) -> Result<Expr, Vec<ParseError<'s>>> {
        match self.parse_expr(visitor) {
            Ok(expr) => Ok(expr),
            Err(()) => Err(mem::take(&mut self.errors)),
        }
    }

    fn parse_id(&mut self, visitor: Visitor<'s, '_, '_, &'static str>) -> Result<String, ()> {
        match visitor.token() {
            "id" => Ok(visitor.source().to_owned()),
            _ => self.error(visitor, "expected identifier", "Expected an identifier."),
        }
    }

    fn parse_expr(&mut self, visitor: Visitor<'s, '_, '_, &'static str>) -> Result<Expr, ()> {
        match visitor.token() {
            // TODO: num error
            "Blank" => self.error(
                visitor,
                "missing expression",
                "Expected an expression, but found nothing.",
            ),
            "Juxtapose" => self.error(
                visitor,
                "extra expression",
                "Found two expressions with nothing to join them.",
            ),
            "id" => Ok(Expr::Id(visitor.source().to_owned())),
            "num" => Ok(Expr::Num(visitor.source().parse::<i32>().unwrap())),
            "lt" | "gt" | "eq" | "plus" => {
                let comparison = match visitor.token() {
                    "lt" => Binop::Lt,
                    "gt" => Binop::Gt,
                    "eq" => Binop::Eq,
                    "plus" => Binop::Plus,
                    _ => unreachable!(),
                };
                let [left, right] = visitor.children();
                let left = self.parse_expr(left);
                let right = self.parse_expr(right);
                Ok(Expr::Binop(comparison, Box::new(left?), Box::new(right?)))
            }
            "group" | "block" => {
                let [child] = visitor.children();
                self.parse_expr(child)
            }
            "call" => {
                let [func, arg] = visitor.children();
                let func = self.parse_expr(func);
                let arg = self.parse_expr(arg);
                Ok(Expr::Call(Box::new(func?), Box::new(arg?)))
            }
            "if" => {
                let [cond, consq] = visitor.children();
                let cond = self.parse_expr(cond);
                let consq = self.parse_expr(consq);
                Ok(Expr::IfChain(vec![(cond?, consq?)]))
            }
            "else" => {
                let mut clauses = visitor;
                let mut chain = vec![];
                while clauses.token() == "else" {
                    let [ifclause, else_clauses] = clauses.children();
                    if ifclause.token() != "if" {
                        return self.error_at_token(
                            clauses,
                            "missing 'if'",
                            "An 'else' may only come after an 'if', but this 'else' is on its own.",
                        );
                    }
                    clauses = else_clauses;
                    let [cond, consq] = ifclause.children();
                    let cond = self.parse_expr(cond);
                    let consq = self.parse_expr(consq);
                    chain.push((cond?, consq?));
                }
                if clauses.token() != "block" {
                    return self.error(
                        clauses,
                        "needs braces",
                        "An 'else' must be followed by braces '{...}', but this 'else' was not.",
                    );
                }
                let [final_else] = clauses.children();
                let final_else = self.parse_expr(final_else);
                chain.push((Expr::Id("true".to_string()), final_else?));
                Ok(Expr::IfChain(chain))
            }
            "let" => {
                let [id, val, body] = visitor.children();
                let id = self.parse_id(id);
                let val = self.parse_expr(val);
                let body = self.parse_expr(body);
                Ok(Expr::Let(id?, Box::new(val?), Box::new(body?)))
            }
            op => panic!("missing op case {}", op),
        }
    }

    fn error<T>(
        &mut self,
        visitor: Visitor<'s, '_, '_, &'static str>,
        short_message: &str,
        message: &str,
    ) -> Result<T, ()> {
        self.errors.push(visitor.error(short_message, message));
        Err(())
    }

    fn error_at_token<T>(
        &mut self,
        visitor: Visitor<'s, '_, '_, &'static str>,
        short_message: &str,
        message: &str,
    ) -> Result<T, ()> {
        self.errors
            .push(visitor.error_at_token(short_message, message));
        Err(())
    }
}

#[track_caller]
fn parse_to_string(parser: &Parser<&'static str>, src: &str) -> String {
    use std::fmt::Write;

    let source = Source::new("testcase", src.to_owned());
    match parser.parse(&source) {
        Ok(tree) => {
            let traverser = Traverser::new();
            match traverser.parse(tree.visitor()) {
                Ok(expr) => format!("{}", expr),
                Err(errors) => {
                    let mut out = String::new();
                    let mut errors = errors.into_iter();
                    write!(
                        &mut out,
                        "{}",
                        errors.next().unwrap().display_with_color_override(false)
                    )
                    .unwrap();
                    for err in errors {
                        write!(&mut out, "\n{}", err.display_with_color_override(false)).unwrap();
                    }
                    out
                }
            }
        }
        Err(err) => format!("{}", err),
    }
}

#[track_caller]
fn test(parser: &Parser<&'static str>, src: &str, expected: &str) {
    let actual = parse_to_string(parser, src);
    if actual != expected {
        print!("ACTUAL:\n{}", actual);
        print!("EXPECTED:\n{}", expected);
        println!("END");
        panic!("test case failed");
    }
}

#[test]
fn test_parsing_parens() {
    let parser = make_parser().unwrap();

    test(&parser, "f(x)", "(call f x)");
    assert_eq!(parse_to_string(&parser, "x < y"), "(< x y)");
    assert_eq!(parse_to_string(&parser, "x < y < z"), "(< (< x y) z)");
    assert_eq!(parse_to_string(&parser, "x < (y < z)"), "(< x (< y z))");
    assert_eq!(parse_to_string(&parser, "f(x)(y)"), "(call (call f x) y)");
}

#[test]
fn test_parsing_if() {
    let parser = make_parser().unwrap();

    assert_eq!(parse_to_string(&parser, "if x { X }"), "(if [x X])");
    assert_eq!(
        parse_to_string(&parser, "if x < y { A } else if x > y { B } else { C }"),
        "(if [(< x y) A] [(> x y) B] [true C])"
    );
}

#[test]
fn test_parsing_let() {
    let parser = make_parser().unwrap();

    assert_eq!(
        parse_to_string(&parser, "let x = 5 in x + 1"),
        "(let x 5 (+ x 1))"
    );
    test(
        &parser,
        "let x + 1 = 5 in x",
        r#"Parse Error: Expected an identifier.
 --> testcase:1:5
  |
1 |let x + 1 = 5 in x
  |    ^^^^^ expected identifier
"#,
    );
}

#[test]
fn test_multiple_errors() {
    let parser = make_parser().unwrap();

    test(
        &parser,
        "(1 else { A }) + (if x > 5 { A } else B + C)",
        r#"Parse Error: An 'else' may only come after an 'if', but this 'else' is on its own.
 --> testcase:1:4
  |
1 |(1 else { A }) + (if x > 5 { A } else B + C)
  |   ^^^^ missing 'if'

Parse Error: An 'else' must be followed by braces '{...}', but this 'else' was not.
 --> testcase:1:39
  |
1 |(1 else { A }) + (if x > 5 { A } else B + C)
  |                                      ^^^^^ needs braces
"#,
    );
}

#[test]
fn test_blank_and_juxtapose_errors() {
    let parser = make_parser().unwrap();

    test(
        &parser,
        "1 2 +",
        r#"Parse Error: Found two expressions with nothing to join them.
 --> testcase:1:1
  |
1 |1 2 +
  |^^^ extra expression

Parse Error: Expected an expression, but found nothing.
 --> testcase:1:6
  |
1 |1 2 +
  |     ^ missing expression
"#,
    );
}

#[test]
fn test_long_errors() {
    let parser = make_parser().unwrap();

    test(
        &parser,
        "let
  xx +
  111 = 5 in x",
        r#"Parse Error: Expected an identifier.
 --> testcase:2:3
  |
2 |  xx +
  |  ^^^^
3 |  111 = 5 in x
  |^^^^^ expected identifier
"#,
    );

    test(
        &parser,
        "let
  xx
 +
  111 =
  5
in x",
        r#"Parse Error: Expected an identifier.
 --> testcase:2:3
  |
2 |  xx
  |  ^^
...
4 |  111 =
  |^^^^^ expected identifier
"#,
    );
}
