use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Token, Visitor};
use std::collections::HashMap;
use std::fmt;
use std::mem;

fn make_json_parser() -> Result<Parser<JsonToken>, GrammarError> {
    let mut grammar = Grammar::new("[ \n\r\t]+")?;
    grammar.regex(JsonToken::JString, r#""([^\\"]|(\\.))*""#)?;
    grammar.regex(
        JsonToken::Number,
        r#"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"#,
    )?;
    grammar.regex(JsonToken::Invalid, r#"[a-zA-Z_][a-zA-Z0-9_]*"#)?; // for catching missing qutoes
    grammar.string(JsonToken::Null, "null")?;
    grammar.string(JsonToken::True, "true")?;
    grammar.string(JsonToken::False, "false")?;
    grammar.op(JsonToken::Array, pattern!("[" "]"))?;
    grammar.op(JsonToken::Object, pattern!("{" "}"))?;
    grammar.right_assoc();
    grammar.op(JsonToken::Keyval, pattern!(_ ":" _))?;
    grammar.right_assoc();
    grammar.op(JsonToken::Comma, pattern!(_ "," _))?;
    grammar.finish()
}

#[allow(dead_code)] // I'm not dead! I'm read only through fmt::Debug.
#[derive(Debug)]
enum Json {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

#[derive(PartialEq, Eq, Clone, Debug)]
enum JsonToken {
    JString,
    Number,
    Invalid,
    Null,
    True,
    False,
    Array,
    Object,
    Keyval,
    Comma,
    LexError,
    Blank,
    Juxtapose,
}

impl fmt::Display for JsonToken {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl Token for JsonToken {
    const LEX_ERROR: JsonToken = JsonToken::LexError;
    const BLANK: JsonToken = JsonToken::Blank;
    const JUXTAPOSE: JsonToken = JsonToken::Juxtapose;
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
        visitor: Visitor<'s, '_, '_, JsonToken>,
    ) -> Result<Json, Vec<ParseError<'s>>> {
        let expr = self.parse_value(visitor);
        if self.errors.is_empty() {
            Ok(expr)
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn parse_value(&mut self, visitor: Visitor<'s, '_, '_, JsonToken>) -> Json {
        use JsonToken::*;

        match visitor.token() {
            LexError => self.error_json(
                visitor,
                "unknown token",
                "Unrecognized token in JSON value.",
            ),
            Blank => self.error_json(visitor, "missing Json value", "Missing Json value."),
            Juxtapose => self.error_json(
                visitor,
                "too many values",
                "Found two values next to each other.",
            ),
            Invalid => self.error_json(visitor, "missing quotes", "Missing quotes."),
            Null => Json::Null,
            True => Json::Boolean(true),
            False => Json::Boolean(false),
            JString => {
                let src = visitor.source();
                Json::String(src[1..src.len() - 1].to_owned())
            }
            Number => match visitor.source().parse::<f64>() {
                Ok(n) => Json::Number(n),
                Err(err) => self.error_json(
                    visitor,
                    "invalid number",
                    &format!("Invalid number '{}'", err),
                ),
            },
            Array => {
                let visitor = visitor.child(0);
                if visitor.token() == Blank {
                    return Json::Array(vec![]);
                }
                let mut elems = vec![];
                self.parse_list(visitor, &mut elems);
                Json::Array(elems)
            }
            Object => {
                let visitor = visitor.child(0);
                if visitor.token() == Blank {
                    return Json::Object(HashMap::new());
                }
                let mut object = HashMap::new();
                self.parse_object(visitor, &mut object);
                Json::Object(object)
            }
            Comma => self.error_json(
                visitor,
                "unexpected list",
                "Expected a single JSON value, not a list.",
            ),
            Keyval => self.error_json(
                visitor,
                "unexpected key:value pair",
                "Expected a JSON value here, not a key:value pair.",
            ),
        }
    }

    fn parse_list(&mut self, mut visitor: Visitor<'s, '_, '_, JsonToken>, elems: &mut Vec<Json>) {
        while visitor.token() == JsonToken::Comma {
            let [head, tail] = visitor.children();
            let head = self.parse_value(head);
            elems.push(head);
            visitor = tail;
        }
        if visitor.token() == JsonToken::Blank {
            self.error(
                visitor,
                "trailing comma",
                "JSON does not allow trailing commas.",
            );
        } else {
            elems.push(self.parse_value(visitor));
        }
    }

    fn parse_object(
        &mut self,
        mut visitor: Visitor<'s, '_, '_, JsonToken>,
        object: &mut HashMap<String, Json>,
    ) {
        while visitor.token() == JsonToken::Comma {
            let [head, tail] = visitor.children();
            self.parse_keyval(head, object);
            visitor = tail;
        }
        if visitor.token() == JsonToken::Blank {
            self.error(
                visitor,
                "trailing comma",
                "JSON does not allow trailing commas.",
            );
        } else {
            self.parse_keyval(visitor, object);
        }
    }

    fn parse_keyval(
        &mut self,
        visitor: Visitor<'s, '_, '_, JsonToken>,
        object: &mut HashMap<String, Json>,
    ) {
        if visitor.token() != JsonToken::Keyval {
            return self.error(visitor, "expected key:value", "Expected a key:value pair.");
        }
        let [key, val] = visitor.children();
        let strkey = self.parse_key(key);
        let val = self.parse_value(val);
        if object.contains_key(&strkey) {
            return self.error(key, "duplicate key", "Duplicate key in object.");
        }
        object.insert(strkey, val);
    }

    fn parse_key(&mut self, visitor: Visitor<'s, '_, '_, JsonToken>) -> String {
        if let JsonToken::JString = visitor.token() {
            let src = visitor.source();
            src[1..src.len() - 1].to_owned()
        } else {
            self.error(
                visitor,
                "expected key",
                "Expected a key (in double quotes).",
            );
            "".to_owned()
        }
    }

    fn error(
        &mut self,
        visitor: Visitor<'s, '_, '_, JsonToken>,
        short_message: &str,
        message: &str,
    ) {
        self.errors.push(visitor.error(short_message, message));
    }

    fn error_json(
        &mut self,
        visitor: Visitor<'s, '_, '_, JsonToken>,
        short_message: &str,
        message: &str,
    ) -> Json {
        self.errors.push(visitor.error(short_message, message));
        Json::Null
    }
}

fn main() {
    eprintln!(
        "Reading from stdin. If you're using this interactively, end your input with Ctrl-D."
    );

    // Read input
    let parser = make_json_parser().unwrap();
    let source = Source::from_stdin().unwrap();

    // Parse and print
    match parser.parse(&source) {
        Ok(tree) => {
            println!("{}", tree.visitor());
            let traverser = Traverser::new();
            match traverser.parse(tree.visitor()) {
                Ok(expr) => println!("{:#?}", expr),
                Err(errors) => {
                    let mut errors = errors.into_iter();
                    print!("{}", errors.next().unwrap());
                    for err in errors {
                        print!("\n{}", err);
                    }
                }
            }
        }
        Err(err) => println!("{}", err),
    }
}
