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

fn parenthesize(out: &mut String, tree: ParseTree<N>) {
    out.push('(');
    let num_children = tree.children().len();
    let mut at_start = true;
    let print_space = |out: &mut String| {
        if !at_start {
            out.push(' ');
        }
        at_start = false;
    };
    for (i, child) in tree.children().enumerate() {
        if let Some(token) = tree.token_before_child(i) {
            print_space(out);
            out.push_str(token);
        }
        if child.name != "$MissingAtom" {
            print_space(out);
            parenthesize(out, child);
        }
        if i + 1 == num_children {
            if let Some(token) = tree.token_after_child(i) {
                print_space(out);
                out.push_str(token);
            }
        }
    }
    out.push(')');
}

fn pattern_to_const<'a>(pattern: Option<&'a Pattern>, visitor: Visitor<'a>) -> &'a str {
    match pattern {
        None => "?",
        Some(Pattern::Constant(constant)) => &constant,
        Some(_) => visitor.text(),
    }
}
