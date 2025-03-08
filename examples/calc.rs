use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Visitor};

fn make_parser() -> Result<Parser<&'static str>, GrammarError> {
    let mut grammar = Grammar::new_with_unicode_whitespace()?;
    grammar.regex("Number", r#"[0-9]+[.]?[0-9]*([eE][+-]?[0-9]+)?"#)?;
    grammar.op("Parens", pattern!("(" ")"))?;
    grammar.left_assoc();
    grammar.op("Times", pattern!(_ "*" _))?;
    grammar.op("Divide", pattern!(_ "/" _))?;
    grammar.left_assoc();
    grammar.op("Negative", pattern!("-" _))?;
    grammar.op("Plus", pattern!(_ "+" _))?;
    grammar.op("Minus", pattern!(_ "-" _))?;
    grammar.left_assoc();
    grammar.op("Log", pattern!("log" "of" _))?;
    grammar.finish()
}

fn calc<'s>(expr: Visitor<'s, '_, '_, &'static str>) -> Result<f64, ParseError<'s>> {
    match expr.token() {
        "Blank" => Err(expr.error("missing expression", "Missing expression.")),
        "Juxtapose" => Err(expr.error(
            "extra expression",
            "Multiple expressions. There can only be one.",
        )),
        "Number" => match expr.source().parse::<f64>() {
            Ok(n) => Ok(n),
            Err(err) => Err(expr.error("invalid number", &format!("Invalid number '{}'", err))),
        },
        "Times" => Ok(calc(expr.child(0))? * calc(expr.child(1))?),
        "Divide" => Ok(calc(expr.child(0))? / calc(expr.child(1))?),
        "Parens" => calc(expr.child(0)),
        "Negative" => Ok(-calc(expr.child(0))?),
        "Plus" => Ok(calc(expr.child(0))? + calc(expr.child(1))?),
        "Minus" => Ok(calc(expr.child(0))? - calc(expr.child(1))?),
        "Log" if expr.child(0).token() == "Blank" => Ok(f64::ln(calc(expr.child(1))?)),
        "Log" => Ok(calc(expr.child(1))?.log(calc(expr.child(0))?)),
        op => panic!("Bug: missing case in parser: {}", op),
    }
}

// A cylindrical capacitor has a length of 8cm and is made of concentric rings of radius 2cm
// and 4cm. It's filled with vacuum. What's its capacitance, in F?
//
// The formula is:
//
//     2 * pi * e_0 * length / ln (outerRadius / innerRadius)
//
// Substituting, we get:
//
//     2 * 3.14159265 * 8.854187e-12 * 0.08 / log of (0.04 / 0.02)
//
// which gives 0.00000000000642086 F ~= 6.42e-12 F
//
// Try inputting the second formula: it'll give the same answer.

fn main() {
    eprintln!(
        "Reading from stdin. If you're using this interactively, end your input with Ctrl-D."
    );

    // Read input
    let parser = make_parser().unwrap();
    let source = Source::from_stdin().unwrap();

    // Parse and calculate
    match parser.parse(&source) {
        Ok(tree) => match calc(tree.visitor()) {
            Ok(answer) => println!("{}", answer),
            Err(err) => println!("{}", err),
        },
        Err(err) => println!("{}", err),
    }
}
