# Panfix Parsing

Panfix parsing is a new approach to parsing based on (but more expressive than)
[operator precedence grammars](https://en.wikipedia.org/wiki/Operator-precedence_grammar).

It is not based on Context Free Grammars (CFGs), nor on Parsing Expression
Grammars (PEGs). It's has a **different style of grammar**, roughly given as a
list of multifix operators. The rules for what constitute a valid grammar are
extremely simple (unlike, say, LR or LALR parsing).

Panfix parsing always runs in **linear time**, with no dependence on the size of
the grammar. That is, if `N` is the size of the text to be parsed and `G` is the
size of the grammar, then a panfix parser runs in `O(N)` (not `O(NG)` like
Packrat PEG parsing).

It gives very **lax** parsers, which will happily generate a parse tree in the
presense of a variety of errors. On one hand, this increases the number of cases
you need to deal with when processing a parse tree. On the other hand, these
extra cases are error cases and this is a good time to produce custom error
messages for them.

## Example: JSON

The best introduction might be a worked example.  Let's look at what it takes to
parse JSON with panfix parsing. Here's a panfix grammar for JSON:

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
this exact string". The "Array" line says that `[` starts an array and `]` ends
it. The "Object" line is similar.

Next up is:

```rust
    grammar.right_assoc();
    grammar.op("Keyval", pattern!(_ ":" _))?;
    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;
```

The two `right\_assoc()` calls begin two _precedence groups_, each with one
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
construct context-specific error messages later, as you process the parse tree.

### Parsing some (badly formed) JSON

Let's see what happens when we parse this badly formed "JSON":

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
let parser = make\_json\_parser().unwrap();
let input = ...read that JSON file...;
let source = Source::new("bad\_json", input);
match parser.parse(&source) { ... }
```

The call to `parse` will actually return `Ok` instead of `Err`! This is because
panfix parsing is purposefully very lax. The only thing it cares about is that
the operators are complete, and in this example `[` is always matched with `]` and
`{` is always matched with `}`. Thus `parse` returns `Ok(tree)`. When you `Display`
this tree, you get:

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

The mechanism for this laxness in parsing is that two kinds of nodes were
implicitly inserted, displayed as `_` above.

#### Blank nodes

_Blank_ nodes are inserted where an argument is missing. Consider, for example,
the "diet" portion of the JSON:

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

In JSON this is an error, though in languages that allow a trailing comma it
wouldn't be.

#### Juxtapose nodes

_Juxtapose_ nodes are the complement of Blank nodes: they are inserted where
there are two expressions in a row with nothing to join them. For example, this
fragment of the JSON:

    "weight_kg": 54.5
    "disposition": "friendly",

turns into this fragment of the parse tree:

    (Keyval "weight_kg"
      (Keyval (_ 54.5 "disposition") "friendly"))

where the `_` is a Juxtapose node, saying that `54.5` and `"disposition"` were
juxtaposed (i.e., adjacent).

Just like the Blank case isn't always an error, the Juxtapose case might be
expected. For example, even in JSON the empty list `[]` would parse as
`(Array _)`.

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

This may feel like busywork, but it's not. Good error messages are extremely
context dependent; `panfix` does not know enough to produce quality error
messages on its own. Notice, for example, the message above: "JSON does not
allow trailing commas". That can only be hand written.

`visitor` is a reference to a node in the tree. You can call `.children()` to
get its children, and `.error(message)` to construct an error message at that
source location.

The fact that panfix parsing is so lax comes out to shine: our JSON example
produces a whole set of helpful error messages for the bad JSON we've been
looking at:

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

For the full example of JSON parsing, see <examples/json.rs>. For another
example, see <examples/calc.rs>.

## Specification

Now that you have the lay of the land, let's switch to focus on the details,
requirements, and guarantees.

### Panfix Grammars

A panfix `Grammar` consists of a whitespace regex, and a set of _operators_.
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
  precedence than operators defined in later groups. (If neither of these
  methods is called, the default is left-associativity.)
- `grammar.op(name: &str, pattern: Pattern)` defines an operator. It inherits
  the precedence and associativity of its group, and `pattern` says what it's
  _fixity_ and _tokens_ are. In the `pattern!` macro, fixity is declared by
  putting a leading or trailing underscore if there is a left or right argument
  respectively, and putting the tokens in the middle in quotes. For example,
  `pattern!(_ "[" "]")` has two tokens, and its fixity is that is has a left
  argument but no right argument.

A grammar must obey three simple rules:

1. Its regexs must be well formed.
2. At most two operators may start with the same token: one that has a left
   argument, and one that does not. (For example, this allows having an
   operator for both unary and binary minus: they both start with the token "-",
   but binary minus has a left argument and unary minus does not.)
3. Operators with the same precedence must have the same associativity. (This
   rule is enforced by the builder pattern, so you only need to think about it
   if you use `add_raw_op`.)

All three rules are enforced by the `Grammar` type; you will get an error if
you violate them when you call `Grammar.finish()`.

### Lexing

When the source is being lexed, multiple regexes from the grammar might match.
The rule for which one gets used is that:

- The longer match wins,
- or failing that a string pattern wins over a regex pattern,
- or failing that the pattern defined earlier in the grammar wins.

### Parsing

Given a panfix grammar and a source file, you can parse the source in _linear
time_, producing either a parse error or a parse tree.

There are three kinds of parse errors:

- A lexing error. For example, encountering the character `%` in JSON.
- An unexpected token. For example, encountering `)` without any `(`.
- An operator began but was not completed. For example, encountering an `(` but
  no following `)`.

These are the _only_ errors. If there aren't any nonsense tokens and your
parentheses (and such) are matched, the parse will succeed!

If parsing is successful, it produces a _parse tree_. Each node in the tree
has an operator and a source location. The operators in the tree include the
operators defined in the grammar, plus two extra operators:

- The `"Blank"` operator denotes a missing argument. For example, in the source
  `true ||`, a "Blank" operator would be inserted to the right of `||`.
- The `"Juxtapose"` operator denotes that two expressions are adjacent. For
  example, in the source `2 3`, a "Jutxapose" operator would be inserted
  between `2` and `3`.

The parse tree is guaranteed to be well formed, in the sense that:

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
3. There aren't "too many" blanks and juxtaposes. For example, `1 + 2` will
   be parsed as `(+ 1 2)` and not as `(Juxtapose (+ 1 Blank) 2)`.

### Putting it all together

1. Construct a `Source`, from file or stdin or whatnot.
2. Construct a `Grammar` using the builder pattern, or `add_raw_op` if you need
   more control. Call `Grammar.finish()` to get a `Parser`.
3. Parse using `Parser.parse(Source)`. If there are any errors, give up and
   display them.
4. If that succeeded, convert the `ParseTree` into an AST (or whatever your
   internal representation will be). _This is the time to check for errors_.
   Basically: walk the tree, checking the `name()` of each node: if it's
   expected then recur, and if it's unexpected then produce a custom error
   message.

Again, to see a full example, look at <examples/json.rs> or <examples/calc.rs>.
