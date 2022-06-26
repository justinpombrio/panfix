# Panfix Parsing

Panfix is a new approach to parsing, using a modified version of [operator
precedence grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar).

- It has its own grammar, that is not a CFG nor a PEG. There are very simple
  rules for what constitutes a legal grammar.
- It runs in linear time. Not `O(NG)` like PEG packrat parsing, but `O(N)`.
- It is a very _lax_ parser, and will happily generate a parse tree in the
  presense of a variety of errors. On one hand, this increases the number of
  cases you need to deal with when processing a parse tree. On the other hand,
  these extra cases are error cases and this is a good time to produce
  custom error messages for them.

## Example

Let's look at what it takes to parse JSON with panfix parsing. Here's a panfix
grammar for JSON:

```rust
use panfix::{Parser, Grammar, GrammarError};

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
```

The `regex` and `string` lines are defining how to parse constants like strings
and booleans. `regex` means "match this regex pattern` and `string` means "match
this exact string". The "Array" line says that if we encounter `[`, that's the
start of an "Array", and `]` is the end. The "Object" line is similar.

Next up is:

```rust
    grammar.right_assoc();
    grammar.op("Keyval", pattern!(_ ":" _))?;
    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;
```

The two `right_assoc()` calls begin two _precedence groups_, each with one
operator. Precedence groups have two effects.

First, operators in groups defined earlier bind tighter than operators defined
in later groups. For example, you would define `&&` in an earlier precedence
group than `>`, so that `x < y && y < z` parses as `(x < y) && (y < z)` instead
of `x < (y && y) < z`.

Second, each precedence group declares whether its operators are left or right
associative. For example, field access `_._` should be declared right
associative so that `person.birthday.month` is parsed as
`(person.birthday).month` instead of `person.(birthday.month)`. In the JSON
example, we declare `:` and `,` to be right associative, but in this case left
associative would work just as well.

The `pattern!(_ "," _)` declares that `,` is an operator that takes a left and
right argument, denoted by an underscore at the beginning and end. In contrast,
`pattern!("[" "]")` lacks underscores and thus says that JSON arrays do not take
a left or right argument. On the other hand, an argument is always allowed
_between_ tokens: such as between `[` and `]`.

Notice some things that this grammar does _not_ say. Nowhere does it say that
`:` is only allowed inside object `{...}`, nor that object keys have to be
Strings. Panfix parsing is quite unusual in this regard. You don't define the
exact grammar you want, you instead define a _superset_ of the grammar, and
construct context-specific parsing error messages later, as you process the
parse tree.

### Parsing some (badly formed) JSON

Let's see what happens when we parse this "JSON":

```
{
    "id": 999,
    "object_class:" "safe",
    "weight_kg": 54.5
    "disposition": "friendly",
    "diet": [
        "M&Ms",
        "Necco wafers",
        "other sweets",
    ],
    "interactions": {
        "target_id": 682,
        "effect": mixed,
    }
}
```

as so:

```
let parser = make_json_parser().unwrap();
let input = ...read that JSON file...;
let source = Source::new("bad_json", input);
match parser.parse(&source) { ... }
```

The call to `parse` will actually return `Ok` instead of `Err`! This is because
panfix parsing is purposefully very lax. The only thing it cares about is that
the operators are complete, and in this example `[` is always matched by `]` and
likewise for `{` and `}`. Thus `parse` return `Ok(tree)` for this parse tree:

```
(Object
  (Comma (Keyval "id" 999)
    (Comma (_ "object_class:" "safe")
      (Comma
        (Keyval "weight_kg"
          (Keyval (_ 54.5 "disposition") "friendly"))
        (Comma
          (Keyval "diet"
            (Array
              (Comma "M&Ms"
                (Comma "Necco wafers"
                  (Comma "other sweets" _)))))
          (Keyval "interactions"
            (Object
              (Comma (Keyval "target_id" 682)
                (Comma (Keyval "effect" mixed) _)))))))))
```

(Note: this is what the `Display` implementation for `ParseTree` outputs.)

Panfix inserts two implicit kinds of nodes.

#### Blank nodes

_Blank_ nodes are inserted where an argument is missing. Consider, for example,
this array:

    "diet": [
        "M&Ms",
        "Necco wafers",
        "other sweets",
    ],

`,` is defined to take two arguments, but the argument _after_ `"other sweets",`
is missing. Thus in the parse tree a Blank, written `_`, is inserted:

    (Array
      (Comma "M&Ms"
        (Comma "Necco wafers"
          (Comma "other sweets" _)))))

#### Juxtapose nodes

_Juxtapose_ nodes are the complement of Blank nodes: they are inserted where
there are two expressions in a row with nothing to join them. For example, this
fragment of JSON:

    "weight_kg": 54.5
    "disposition": "friendly",

turns into this fragment of the parse tree:

    (Keyval "weight_kg"
      (Keyval (_ 54.5 "disposition") "friendly"))

where the `_` is a Juxtapose node, saying that `54.5` and `"disposition"` were
juxtaposed (i.e., adjacent).

Note that while Blank and Juxtapose are errors in these examples, they're not
always indicative of a problem. Even just in JSON, the empty list `[]` would
parse as `(Array _)`.

The combination of Blank and Juxtapose nodes makes panfix parsing very lax. This
is beneficial in a way, as we'll see next.

### Parse tree -> JSON

What now? The _hard_ work of producing a parse tree (with source locations, to
boot) has already been done. What remains is the _easy_ but verbose work of
walking that tree and converting it into a `Json` type:

    #[derive(Debug)]
    enum Json {
        String(String),
        Number(f64),
        Boolean(bool),
        Null,
        Array(Vec<Json>),
        Object(HashMap<String, Json>),
    }

You can find this conversion at [examples/json.rs](examples/json.rs). It's
verbose but straightforward. Here's a snippet from it:

```rust
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
```

`visitor` is a reference to a node in the tree. You can call `.children()` to
get its children, and `.error(message)` to construct an error message at that
source location.

The fact that panfix parsing is so lax comes out to shine: our JSON example
produces a whole set of helpful error messages:

```
Parse Error: Expected a key:value pair.                                                                                                                        
At 'stdin' line 2.                                                                                                                                             
                                                                                                                                                               
    "object_class:" "safe",                                                                                                                                    
    ^^^^^^^^^^^^^^^^^^^^^^                                                                                                                                     
                                                                                                                                                               
Parse Error: Expected a JSON value here, not a key:value pair.                                                                                                 
At 'stdin' lines 3-4.                                                                                                                                          
    "weight_kg": 54.5                                                                                                                                          
                 ^^^^                                                                                                                                          
    "disposition": "friendly",
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Parse Error: JSON does not allow trailing commas.
At 'stdin' line 8.

        "other sweets",
                       ^

Parse Error: Missing quotes.
At 'stdin' line 12.

        "effect": mixed,
                  ^^^^^

Parse Error: JSON does not allow trailing commas.
At 'stdin' line 12.

        "effect": mixed,
                        ^
```

## Specification

Here I give a specification of panfix grammars and parsing.

### Panfix Grammars

A panfix grammar consists of a whitespace regex, and a set of _operators_.
Everything is an operator, from numbers to identifiers to binary operators to
function definitions. Each operator has the following properties:

- _Name_ gives the operator a name, so that you can identify it in the parse
  tree later.
- _Tokens_ say what sequence of tokens the operator matches. For example:
  identifiers might match the regex token `[a-zA-Z_]+`; parentheses for grouping
  would match the sequence of tokens "(", ")"; and function definitions might
  match "function", "(", ")", "{", "}".
- _Precedence_ says how tightly it binds. For example, field access `_._` should
  bind tighter than array indexing `_[_]`, so that `catalog.entries[0]` parses
  as `(catalog.entries)[0]` rather than `catalog.(entries[0])`.
- _Associativity_ says how it should group with itself. For example, minus
  should be _left associative_ so that `1 - 2 - 3` is equal to `((1 - 2) - 3) =
  -1 - 3 = -4` instead of _right associative_ where it would be equal to `(1 -
  (2 - 3)) = 1 - -1 = 0`.
- _Fixity_ says whether it takes a left argument and whether it takes a right
  argument. For example: field access `_._` takes both a left and right
  argument; array indexing `_[_]` takes a left argument but no right argument
  (because there's an argument _before the first token `[`_ but no argument
  _after the last token `]`_); and a number like `2.5` takes neither a left nor
  a right argument.

You _can_ specify these properties manually with `grammar.add_raw_op(name, prec,
assoc, fixity, tokens)`, but there's also a higher level interface to make this
easier:

- `grammar.regex(name: &str, regex_pattern: &str)` defines an operator with a
  single token and no arguments. Since it has no arguments, the precedence and
  associativity are irrelevant. Use this for e.g. identifiers, numbers, and
  string literals.
- `grammar.string(name: &str, string_pattern: &str)` is the same, but matches
  against a literal string pattern instead of a regex pattern.
- `grammar.left_assoc()` and `grammar.right_assoc()` introduce a new _operator
  group_. Operators defined after them belong to that operator group. Operators
  in an operator group all have the same associativity, and all have the same
  precedence as each other. Operators defined in earlier groups have tighter
  precedence than operators defined in later groups.
- `grammar.op(name: &str, pattern: Pattern)` defines an operator. It inherits
  the precedence and associativity of its group, and `pattern` says what it's
  _fixity_ and _tokens_ are. In the `pattern!` macro, fixity is declared by
  putting a leading or trailing underscore if there is a left or right argument
  respectively, and putting the tokens in the middle in quotes. For example,
  `pattern!(_ "[" "]")` has two tokens, and its fixity is that is has a left
  argument but no right argument.

For example, here is the definition of a grammar for parsing JSON:

```rust
use panfix::{Parser, Grammar, GrammarError};

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
```

A grammar is _well formed_ if it obeys these two rules:

1. At most two operators may start with the same token: one that has a left
   argument, and one that does not.
2. Operators with the same precedence must have the same associativity.  (This
   rule is enforced by the builder pattern, so you only need to think about it
   if you use `add_raw_op`.)

For example, this allows having an operator for both unary and binary minus:
they both start with the token "-", but binary minus has a left argument and
unary minus does not.

(A note on lexing. The rule for lexing is that longer matches win, or failing
that a string pattern wins over a regex pattern, or failing that the token
pattern defined earlier in the grammar wins.)

### Parsing

Given a panfix grammar and a source file, you can parse the source in linear
time, producing either a parse error or a parse tree.

There are three kinds of parse errors:

- A lexing error. For example, encountering the character `%` in JSON.
- An unexpected token. For example, encountering `)` without any `(`.
- An operator began but was not completed. For example, encountering an `(` but
  no following `)`.

If parsing is successful, it produces a _parse tree_. Each node in the tree
contains a reference to an operator and the source location of the occurrence of
the first token of that operator in the source. The operators referenced include
the operators defined in the grammar, plus two implicitly defined operators:

- The "Blank" operator denotes a missing argument. For example, in the source
  `true ||`, a "Blank" operator would be inserted to the right of `||`.
- The "Juxtapose" operator denotes that two expressions are adjacent. For
  example, in the source `2 3`, a "Jutxapose" operator would be inserted
  between `2` and `3`.

A parse tree is _well formed_ iff:

1. Each node has children in this order: (i) one child for its left argument, if
   it takes one; (ii) one child for each space "between" its tokens; and (iii)
   one child for its right argument, if any. Blank nodes have zero children and
   Juxtapose nodes have two. (For example, array indexing `_[_]` has two
   children: 1 for its left argument and 1 for the space between its first and
   second tokens.)
2. If a node has a left argument, then either it has looser precedence than its
   first child, or it is left associative and has equal precedence to its first
   child. Likewise, if it has a right argument then either it has looser
   precedence than its last child, or it is right associative and has equal
   precedence.
3. There aren't "too many" blanks and juxtaposes. For example, `1 + 2` should
   not be parsed as `(Juxtapose (+ 1 Blank) 2)`. In practice this is pretty
   obvious; technically it's somewhat annoying to define. I believe the
   definition is that following the "left argument" of a Juxtapose followed by
   any number of "right argument"s must not yield a Blank, and vice-versa.

If a grammar is well formed, then any source text has at most one well formed
parse tree, and it can be found in linear time!
