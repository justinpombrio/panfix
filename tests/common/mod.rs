use panfix::parsing::{Fixity, ParseError, Parser, Pattern, Visitor};

pub fn run_parser(parser: &Parser, input: &str) -> String {
    use ParseError::*;

    match parser.parse(input) {
        Err(error) => match error {
            LexError { lexeme, pos } => format!("LexErr {} {}:{}", lexeme, pos.line, pos.column),
            ExtraSeparator { separator, pos } => {
                format!("ExtraSep {} {}:{}", separator, pos.line, pos.column)
            }
            MissingSeparator {
                op_name,
                separator,
                pos,
            } => format!(
                "MissingSep {} {} {}:{}",
                op_name, separator, pos.line, pos.column
            ),
        },
        Ok(parsed) => {
            let mut expr = String::new();
            for (i, visitor) in parsed.groups().enumerate() {
                parenthesize(&mut expr, visitor, &parser);
                if i + 1 != parsed.groups().len() {
                    expr.push(' ');
                }
            }
            expr
        }
    }
}

fn parenthesize(out: &mut String, visitor: Visitor, parser: &Parser) {
    if visitor.name() == "$MissingAtom" {
        ()
    }
    let fixity = visitor.fixity();
    let mut children = visitor.children();
    if children.len() == 0 {
        out.push_str(visitor.text());
        return;
    }
    let mut delims = visitor.op_patterns(&parser).into_iter();
    out.push('(');
    if fixity == Fixity::Infix || fixity == Fixity::Suffix {
        let child = children.next().unwrap();
        if child.name() != "$MissingAtom" {
            parenthesize(out, child, parser);
            out.push(' ');
        }
    }
    let delim = delims.next().unwrap_or(None);
    out.push_str(pattern_to_const(delim, visitor));
    while let Some(delim) = delims.next() {
        let child = children.next().unwrap();
        out.push(' ');
        parenthesize(out, child, parser);
        out.push(' ');
        out.push_str(pattern_to_const(delim, visitor));
    }
    if fixity == Fixity::Infix || fixity == Fixity::Prefix {
        let child = children.next().unwrap();
        if child.name() != "$MissingAtom" {
            out.push(' ');
            parenthesize(out, child, parser);
        }
    }
    assert!(children.next().is_none());
    out.push(')');
}

fn pattern_to_const<'a>(pattern: Option<&'a Pattern>, visitor: Visitor<'a>) -> &'a str {
    match pattern {
        None => "?",
        Some(Pattern::Constant(constant)) => &constant,
        Some(_) => visitor.text(),
    }
}
