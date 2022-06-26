use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Visitor};
use std::collections::HashMap;
use std::io;
use std::mem;

fn make_json_parser() -> Result<Parser, GrammarError> {
    let mut grammar = Grammar::new("[ \n\r\t]+")?;
    grammar.regex("String", r#""([^\\"]|(\\.))*""#)?;
    grammar.regex("Number", r#"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"#)?;
    grammar.regex("Invalid", r#"[a-zA-Z_][a-zA-Z0-9_]*"#)?; // for catching missing qutoes
    grammar.string("Null", "null")?;
    grammar.string("True", "true")?;
    grammar.string("False", "false")?;
    grammar.op("Array", pattern!("[" "]"))?;
    grammar.op("Object", pattern!("{" "}"))?;
    grammar.right_assoc();
    grammar.op("Keyval", pattern!(_ ":" _))?;
    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;
    grammar.finish()
}

#[derive(Debug)]
enum Json {
    String(String),
    Number(f64),
    Boolean(bool),
    Null,
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

struct Traverser<'s> {
    errors: Vec<ParseError<'s>>,
}

impl<'s> Traverser<'s> {
    fn new() -> Traverser<'s> {
        Traverser { errors: vec![] }
    }

    fn parse(mut self, visitor: Visitor<'s, '_, '_>) -> Result<Json, Vec<ParseError<'s>>> {
        let expr = self.parse_value(visitor);
        if self.errors.is_empty() {
            Ok(expr)
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn parse_value(&mut self, visitor: Visitor<'s, '_, '_>) -> Json {
        match visitor.name() {
            "Error" => self.error_json(visitor, "Unrecognized token in JSON value."),
            "Blank" => self.error_json(visitor, "Missing Json value."),
            "Juxtapose" => self.error_json(visitor, "Found two values next to each other."),
            "Invalid" => self.error_json(visitor, "Missing quotes."),
            "Null" => Json::Null,
            "True" => Json::Boolean(true),
            "False" => Json::Boolean(false),
            "String" => Json::String(visitor.source().to_owned()),
            "Number" => match visitor.source().parse::<f64>() {
                Ok(n) => Json::Number(n),
                Err(err) => self.error_json(visitor, &format!("Invalid number '{}'", err)),
            },
            "Array" => {
                let visitor = visitor.child(0);
                if visitor.name() == "Blank" {
                    return Json::Array(vec![]);
                }
                let mut elems = vec![];
                self.parse_list(visitor, &mut elems);
                Json::Array(elems)
            }
            "Object" => {
                let visitor = visitor.child(0);
                if visitor.name() == "Blank" {
                    return Json::Object(HashMap::new());
                }
                let mut object = HashMap::new();
                self.parse_object(visitor, &mut object);
                Json::Object(object)
            }
            "Comma" => self.error_json(visitor, "Expected a single JSON value, not a list."),
            "Keyval" => {
                self.error_json(visitor, "Expected a JSON value here, not a key:value pair.")
            }
            op => panic!("Bug: missing parser handler for {}", op),
        }
    }

    fn parse_list(&mut self, mut visitor: Visitor<'s, '_, '_>, elems: &mut Vec<Json>) {
        while visitor.name() == "Comma" {
            let [head, tail] = visitor.children();
            let head = self.parse_value(head);
            elems.push(head);
            visitor = tail;
        }
        if visitor.name() == "Blank" {
            self.error(visitor, "JSON does not allow trailing commas.");
        } else {
            elems.push(self.parse_value(visitor));
        }
    }

    fn parse_object(
        &mut self,
        mut visitor: Visitor<'s, '_, '_>,
        object: &mut HashMap<String, Json>,
    ) {
        while visitor.name() == "Comma" {
            let [head, tail] = visitor.children();
            self.parse_keyval(head, object);
            visitor = tail;
        }
        if visitor.name() == "Blank" {
            self.error(visitor, "JSON does not allow trailing commas.");
        } else {
            self.parse_keyval(visitor, object);
        }
    }

    fn parse_keyval(&mut self, visitor: Visitor<'s, '_, '_>, object: &mut HashMap<String, Json>) {
        if visitor.name() != "Keyval" {
            return self.error(visitor, "Expected a key:value pair.");
        }
        let [key, val] = visitor.children();
        let strkey = self.parse_key(key);
        let val = self.parse_value(val);
        if object.contains_key(&strkey) {
            return self.error(key, "Duplicate key in object.");
        }
        object.insert(strkey, val);
    }

    fn parse_key(&mut self, visitor: Visitor<'s, '_, '_>) -> String {
        if let "String" = visitor.name() {
            visitor.source().to_owned()
        } else {
            self.error(visitor, "Expected a key (in double quotes).");
            "".to_owned()
        }
    }

    fn error(&mut self, visitor: Visitor<'s, '_, '_>, message: &str) {
        self.errors.push(visitor.error(message));
    }

    fn error_json(&mut self, visitor: Visitor<'s, '_, '_>, message: &str) -> Json {
        self.errors.push(visitor.error(message));
        Json::Null
    }
}

fn main() {
    use io::Read;

    let parser = make_json_parser().unwrap();
    let mut input = String::new();
    io::stdin().lock().read_to_string(&mut input).unwrap();
    let source = Source::new("stdin", input);
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
