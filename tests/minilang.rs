use panfix::{pattern, Grammar, GrammarError, Parser, Visitor};
use std::fmt;

fn make_parser() -> Result<Parser, GrammarError> {
    let mut grammar = Grammar::new_with_unicode_whitespace()?;
    grammar.sort("Empty");
    grammar.sort("Id");
    grammar.regex("id", "[a-zA-Z]+")?;
    grammar.sort("Expr");
    grammar.regex("id", "[a-zA-Z]+")?;
    grammar.regex("num", "[+-]?[0-9]+")?;
    grammar.op("block", pattern!("{" Expr "}"))?;
    grammar.op("group", pattern!("(" Expr ")"))?;
    grammar.lgroup();
    grammar.op("call", pattern!(_ "(" Expr ")"))?;
    grammar.lgroup();
    grammar.op("plus", pattern!(_ "+" _))?;
    grammar.lgroup();
    grammar.op("lt", pattern!(_ "<" _))?;
    grammar.op("gt", pattern!(_ ">" _))?;
    grammar.op("eq", pattern!(_ "==" _))?;
    grammar.rgroup();
    grammar.op("if", pattern!("if" Expr "{" Expr "}"))?;
    grammar.op("else", pattern!(_ "else" _))?;
    grammar.lgroup();
    grammar.op("args", pattern!(_ "," _))?;
    grammar.lgroup();
    grammar.op("let", pattern!("let" Id "=" Expr "in" _))?;
    grammar.op(
        "func",
        pattern!("fn" Id "(" Expr ")" Empty "{" Expr "}" Empty ";" _),
    )?;
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

#[derive(Debug)]
enum ParseError {
    DanglingElse,
    ElseWithoutBraces,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
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

fn parse_id(visitor: Visitor) -> Result<String, ParseError> {
    match visitor.op() {
        // TODO: blank / juxt
        "id" => Ok(visitor.source().to_owned()),
        _ => unreachable!(),
    }
}

fn parse_expr(visitor: Visitor) -> Result<Expr, ParseError> {
    match visitor.op() {
        // TODO: blank / juxt
        // TODO: num error
        "id" => Ok(Expr::Id(visitor.source().to_owned())),
        "num" => Ok(Expr::Num(visitor.source().parse::<i32>().unwrap())),
        "lt" | "gt" | "eq" | "plus" => {
            let comparison = match visitor.op() {
                "lt" => Binop::Lt,
                "gt" => Binop::Gt,
                "eq" => Binop::Eq,
                "plus" => Binop::Plus,
                _ => unreachable!(),
            };
            let [left, right] = visitor.expect_children();
            Ok(Expr::Binop(
                comparison,
                Box::new(parse_expr(left)?),
                Box::new(parse_expr(right)?),
            ))
        }
        "group" | "block" => {
            let [child] = visitor.expect_children();
            parse_expr(child)
        }
        "call" => {
            let [func, arg] = visitor.expect_children();
            //let func = parse_expr(func);
            //let arg = parse_expr(arg);
            Ok(Expr::Call(
                //Box::new(func?), Box::new(arg?)
                Box::new(parse_expr(func)?),
                Box::new(parse_expr(arg)?),
            ))
        }
        "if" => {
            let [cond, consq] = visitor.expect_children();
            Ok(Expr::IfChain(vec![(parse_expr(cond)?, parse_expr(consq)?)]))
        }
        "else" => {
            let mut clauses = visitor;
            let mut chain = vec![];
            while clauses.op() == "else" {
                let [ifclause, else_clauses] = clauses.expect_children();
                clauses = else_clauses;
                if ifclause.op() != "if" {
                    return Err(ParseError::DanglingElse);
                }
                let [cond, consq] = ifclause.expect_children();
                chain.push((parse_expr(cond)?, parse_expr(consq)?));
            }
            if clauses.op() != "block" {
                return Err(ParseError::ElseWithoutBraces);
            }
            let [final_else] = clauses.expect_children();
            chain.push((Expr::Id("true".to_string()), parse_expr(final_else)?));
            Ok(Expr::IfChain(chain))
        }
        "let" => {
            let [id, val, body] = visitor.expect_children();
            Ok(Expr::Let(
                parse_id(id)?,
                Box::new(parse_expr(val)?),
                Box::new(parse_expr(body)?),
            ))
        }
        op => panic!("missing op case {}", op),
    }
}

#[track_caller]
fn parse_to_string(parser: &Parser, src: &str) -> String {
    match parser.parse("Expr", src) {
        Ok(tree) => match parse_expr(tree.visitor()) {
            Ok(expr) => format!("{}", expr),
            Err(err) => format!("{}", err),
        },
        Err(err) => format!("{}", err),
    }
}

#[test]
fn test_parsing_parens() {
    let parser = make_parser().unwrap();

    assert_eq!(parse_to_string(&parser, "f(x)"), "(call f x)");
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
    assert_eq!(
        parse_to_string(&parser, "let x + 1 = 5 in x"),
        "Parse error: While parsing 'let', expected '=' but found '+'.\nat 0:6-0:7"
    );
}
