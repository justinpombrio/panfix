use panfix::parsing::{Fixity, ParseError, Parser, Pattern, Visitor};

pub fn run_parser(parser: &Parser, input: &str) -> String {
    use ParseError::*;

    match parser.parse(input) {
        Err(error) => match error {
            LexError { lexeme, pos } => format!("LexErr {} {}:{}", lexeme, pos.line, pos.column),
            ExtraSeparator { separator, pos } => {
                format!("ExtraSep {} {}:{}", separator, pos.line, pos.column)
            }
            MissingSeparator { rule_name, pos } => {
                format!("MissingSep {} {}:{}", rule_name, pos.line, pos.column)
            }
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
    } else if let Some(fixity) = visitor.fixity() {
        let mut children = visitor.children();
        let mut delims = visitor.rule_patterns(&parser).into_iter();
        out.push('(');
        if fixity != Fixity::Prefix {
            let child = children.next().unwrap();
            if child.name() != "$MissingAtom" {
                parenthesize(out, child, parser);
                out.push(' ');
            }
        }
        let delim = delims.next().unwrap();
        out.push_str(pattern_to_const(delim));
        while let Some(delim) = delims.next() {
            let child = children.next().unwrap();
            out.push(' ');
            parenthesize(out, child, parser);
            out.push(' ');
            out.push_str(pattern_to_const(delim));
        }
        if fixity != Fixity::Suffix {
            let child = children.next().unwrap();
            if child.name() != "$MissingAtom" {
                out.push(' ');
                parenthesize(out, child, parser);
            }
        }
        assert!(children.next().is_none());
        out.push(')');
    } else {
        out.push_str(visitor.text());
    }
}

fn pattern_to_const(pattern: Option<&Pattern>) -> &str {
    match pattern {
        None => "?",
        Some(Pattern::Constant(constant)) => &constant,
        Some(_) => unreachable!(),
    }
}
