use panfix::parsing::{Fixity, Grammar, Parsed, Parser, Visitor, WHITESPACE_REGEX};
use panfix::{infix, prefix, suffix};

fn c_subset() -> Parser {
    Grammar::new(WHITESPACE_REGEX)
        .regex("Var", "[a-zA-Z]+")
        .regex("Number", "0|[1-9][0-9]+")
        .constant("True", "true")
        .constant("False", "false")
        .rule_group(vec![
            infix!("Inc", "++"),
            suffix!("Subs", "[", "]"),
            infix!("Dot", "."),
        ])
        .build()
}

/*
fn parenthesize(out: &mut String, visitor: Visitor, parser: &Parser) {
    if visitor.name() == "$MissingAtom" {
        out.push('?');
    } else if let Some(fixity) = visitor.fixity() {
        let mut children = visitor.children();
        let mut patterns = visitor.
        out.push('(');
        if fixity != Fixity::Prefix {
            let child = children.next().unwrap();
            if child.name() != "$MissingAtom" {
                parenthesize(out, child, parser);
            }
        }
        out.push_str(
        if fixity != Fixity::Suffix {
            let child = children.next().unwrap();
            if child.name() != "$MissingAtom" {
                parenthesize(out, child, parser);
            }
        }
        assert!(children.next().is_none());
        out.push(')');
    } else {
        out.push_str(visitor.text());
    }
}
*/

fn write_sexpr(sexpr: &mut String, visitor: Visitor) {
    if visitor.name() == "$MissingAtom" {
        sexpr.push('?');
    } else if visitor.arity() == 0 {
        sexpr.push_str(visitor.text());
    } else {
        sexpr.push('(');
        sexpr.push_str(visitor.name());
        for child in visitor.children() {
            sexpr.push(' ');
            write_sexpr(sexpr, child);
        }
        sexpr.push(')');
    }
}

fn parse_c(input: &str) -> String {
    let parser = c_subset();
    let parsed = parser.parse(input);
    let mut sexpr = String::new();
    for (i, visitor) in parsed.groups().enumerate() {
        write_sexpr(&mut sexpr, visitor);
        if i + 1 != parsed.groups().len() {
            sexpr.push(' ');
        }
    }
    sexpr
}

#[test]
fn test_c_basics() {
    assert_eq!(parse_c(" x "), "x");
    assert_eq!(parse_c("44"), "44");
    //assert_eq!(parse_c("x++"), "(++ x ?)");
}
