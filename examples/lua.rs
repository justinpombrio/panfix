use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Visitor};
use std::collections::HashMap;
use std::mem;
use std::str::FromStr;

// ORDERING:
// "," < "=" in `for`, `local`
// "=" < "," in `fieldlist`

fn make_lua_parser() -> Result<Parser, GrammarError> {
    let mut grammar = Grammar::new("[ \n\r\t]+")?;

    // Nullary Expressions
    grammar.regex("Name", r#"[a-zA-Z_][a-zA-Z0-9_]*"#)?;
    grammar.regex("Label", r#"::[a-zA-Z_][a-zA-Z0-9_]*::"#)?;
    grammar.string("Nil", "nil")?;
    grammar.string("True", "true")?;
    grammar.string("False", "false")?;
    // TODO: These are JSON numerals, which don't exactly match Lua.
    // Especially since Lua allows hex numerals starting with 0x.
    grammar.regex("Numeral", r#"-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"#)?;
    // TODO: These are JSON strings, which might not exactly match Lua.
    grammar.regex("String", r#""([^\\"]|(\\.))*""#)?;
    grammar.string("Elipses", "...")?;
    grammar.op("Parens", pattern!("(" ")"))?;
    grammar.op("Table", pattern!("{" "}"))?;

    // Nullary Statements
    grammar.string("Break", "break")?;
    grammar.op("Function", pattern!("function" "(" ")" "end"))?;
    grammar.op("Do", pattern!("do" "end"))?;
    grammar.op("While", pattern!("while" "do" "end"))?;
    grammar.op("If", pattern!("if" "then" "end"))?;
    grammar.op("For", pattern!("for" "do" "end"))?;

    // Expressions
    grammar.right_assoc();
    grammar.op("Exponentiate", pattern!(_ "^" _))?;
    grammar.left_assoc();
    grammar.op("Neg", pattern!("-" _))?;
    grammar.op("BitwiseNeg", pattern!("~" _))?;
    grammar.op("Not", pattern!("not" _))?;
    grammar.op("Len", pattern!("#" _))?;
    grammar.left_assoc();
    grammar.op("Times", pattern!(_ "*" _))?;
    grammar.op("Divide", pattern!(_ "/" _))?;
    grammar.op("FloorDivide", pattern!(_ "//" _))?;
    grammar.op("Modulo", pattern!(_ "%" _))?;
    grammar.left_assoc();
    grammar.op("Plus", pattern!(_ "+" _))?;
    grammar.op("Minus", pattern!(_ "-" _))?;
    grammar.left_assoc();
    grammar.op("Concat", pattern!(_ ".." _))?;
    grammar.left_assoc();
    grammar.op("LShift", pattern!(_ "<<" _))?;
    grammar.op("RShift", pattern!(_ ">>" _))?;
    grammar.left_assoc();
    grammar.op("BitAnd", pattern!(_ "&" _))?;
    grammar.left_assoc();
    grammar.op("BitXor", pattern!(_ "~" _))?;
    grammar.left_assoc();
    grammar.op("BitOr", pattern!(_ "|" _))?;
    grammar.left_assoc();
    grammar.op("Lt", pattern!(_ "<" _))?;
    grammar.op("Lte", pattern!(_ "<=" _))?;
    grammar.op("Gt", pattern!(_ ">" _))?;
    grammar.op("Gte", pattern!(_ ">=" _))?;
    grammar.op("Eq", pattern!(_ "==" _))?;
    grammar.op("Neq", pattern!(_ "~=" _))?;
    grammar.left_assoc();
    grammar.op("And", pattern!(_ "and" _))?;
    grammar.left_assoc();
    grammar.op("Or", pattern!(_ "or" _))?;

    // Prefix expressions
    grammar.left_assoc();
    grammar.op("Dot", pattern!(_ "." _))?;
    grammar.op("Colon", pattern!(_ ":" _))?;
    grammar.op("Brackets", pattern!(_ "[" "]"))?;
    grammar.op("FunctionCall", pattern!(_ "(" ")"))?;

    // field with brackets -- found inside `tableconstructor` expr
    grammar.left_assoc();
    grammar.op("BracketedField", pattern!("[" "]" "=" _))?;

    // Expression separator
    grammar.left_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;

    // in -- found inside `for` statement
    grammar.left_assoc();
    grammar.op("In", pattern!(_ "in" _))?;

    // else & elseif -- found inside `if` statement
    grammar.left_assoc();
    grammar.op("Else", pattern!(_ "else" _))?;
    grammar.op("ElseIf", pattern!(_ "elseif" "then" _))?;

    // Statements
    grammar.left_assoc();
    grammar.op("Equals", pattern!(_ "=" _))?;
    grammar.op("Goto", pattern!("goto" _))?;
    grammar.op("Repeat", pattern!("repeat" "until" _))?;
    grammar.op("Return", pattern!("return" _))?;
    grammar.op("Local", pattern!("local" _))?;

    // Statement separator
    grammar.left_assoc();
    grammar.op("Semi", pattern!(_ ";" _))?;
    grammar.juxtapose()?;

    grammar.finish()
}

type Name = String;
type Chunk = Block;
type ExpList = Vec<Exp>;
type VarList = Vec<Var>;
type NameList = Vec<Name>;
type FieldList = Vec<Field>;

/// $stat
/// ...
/// --or--
/// $stat
/// ...
/// return $exp, ...
#[derive(Debug)]
struct Block {
    stats: Vec<Stat>,
    retstat: Option<ExpList>,
}

/// List of $names, optionally ending with a _literal_ `...`
#[derive(Debug)]
struct ParList {
    names: NameList,
    ellipses: bool,
}

#[derive(Debug)]
enum Stat {
    /// $var, ... = $exp, ...
    Variable(VarList, ExpList),
    FunctionCall(FunctionCall),
    Label(Name),
    Break,
    /// goto $name
    Goto(Name),
    /// do $block end
    Do(Block),
    /// while $exp do $block end
    While(Box<Exp>, Block),
    /// repeat $block until $exp
    Repeat(Block, Box<Exp>),
    /// if $exp then $block
    /// elseif $exp then block
    /// ...
    /// else $block
    If {
        condition: Box<Exp>,
        then: Block,
        elifs: Vec<(Box<Exp>, Block)>,
        otherwise: Option<Block>,
    },
    /// for $name = $exp, $exp, $exp do $block end
    For {
        var: Name,
        start: Box<Exp>,
        end: Box<Exp>,
        by: Option<Exp>,
        body: Block,
    },
    // INVARIANT: this NameList is never empty
    /// for $name, ... in $exp, ... do $block end
    ForIn(NameList, ExpList, Block),
    /// function $name.$name:$name($parlist) $block end
    Function {
        name: Name,
        dots: Vec<Name>,
        colon: Option<Name>,
        params: ParList,
        body: Block,
    },
    /// local function $name($parlist) $block end
    LocalFunction {
        name: Name,
        params: ParList,
        body: Block,
    },
    /// local $name, ... = $exp, ...
    // INVARIANT: this NameList is never empty
    Local(NameList, Option<ExpList>),
}

#[derive(Debug)]
enum Var {
    /// $name
    Name(Name),
    /// $prefixexp[$exp]
    Brackets(Box<Exp>, Box<Exp>),
    /// $prefixexp.$name
    Dot(Box<Exp>, Name),
}

/*
// TODO: once all implemented, merge PrefixExp into Exp.
#[derive(Debug)]
enum PrefixExp {
    /// $var
    Var(Box<Var>),
    FunctionCall(FunctionCall),
    /// ($exp)
    Parens(Box<Exp>),
}
*/

#[derive(Debug)]
enum FunctionCall {
    /// $prefix $args
    Unqualified(Box<Exp>, Args),
    /// $prefix:$name $args
    Qualified(Box<Exp>, Name, Args),
}

#[derive(Debug)]
enum Args {
    /// ($exp, ...)
    List(ExpList),
    /// { $field, ... }
    TableConstructor(FieldList),
    /// "imastringliteral"
    String(String),
}

#[derive(Debug)]
enum Field {
    /// [$exp] = $exp
    Brackets(Box<Exp>, Box<Exp>),
    /// $name = $exp
    Equals(String, Box<Exp>),
    /// $exp
    Plain(Box<Exp>),
}

#[derive(Debug)]
enum Exp {
    /// nil
    Nil,
    /// false
    False,
    /// true
    True,
    /// numeral in decimal or hex
    Number(f64),
    /// a string literal, in quotes
    String(String),
    /// ...
    Elipses,
    /// function($parlist) $block end
    Function {
        params: ParList,
        body: Block,
    },
    /// { $field, ... }
    TableConstructor(FieldList),
    /// $exp BINOP $exp
    BinOp(BinOp, Box<Exp>, Box<Exp>),
    /// UNOP $exp
    UnOp(UnOp, Box<Exp>),
    /// $var
    Var(Box<Var>),
    FunctionCall(FunctionCall),
    /// ($exp)
    Parens(Box<Exp>),
}

#[derive(Debug)]
enum UnOp {
    /// -
    Neg,
    /// not
    Not,
    /// #
    Len,
    /// ~
    BitwiseNeg,
}

impl FromStr for UnOp {
    type Err = ();

    fn from_str(name: &str) -> Result<UnOp, ()> {
        use UnOp::*;

        match name {
            "Neg" => Ok(Neg),
            "BitwiseNeg" => Ok(BitwiseNeg),
            "Not" => Ok(Not),
            "Len" => Ok(Len),
            _ => Err(()),
        }
    }
}

#[derive(Debug)]
enum BinOp {
    /// +
    Plus,
    /// -
    Minus,
    /// *
    Times,
    /// /
    Divide,
    /// //
    FloorDivide,
    /// ^
    Exponentiate,
    /// %
    Modulo,
    /// &
    BitAnd,
    /// ~
    BitXor,
    /// |
    BitOr,
    /// >>
    RShift,
    /// <<
    LShift,
    /// ..
    Concat,
    /// <
    Lt,
    /// <=
    Lte,
    /// >
    Gt,
    /// >=
    Gte,
    /// ==
    Eq,
    /// ~=
    Neq,
    /// and
    And,
    /// or
    Or,
}

impl FromStr for BinOp {
    type Err = ();

    fn from_str(name: &str) -> Result<BinOp, ()> {
        use BinOp::*;

        match name {
            "Plus" => Ok(Plus),
            "Minus" => Ok(Minus),
            "Times" => Ok(Times),
            "Divide" => Ok(Divide),
            "FloorDivide" => Ok(FloorDivide),
            "Exponentiate" => Ok(Exponentiate),
            "Modulo" => Ok(Modulo),
            "BitAnd" => Ok(BitAnd),
            "BitXor" => Ok(BitXor),
            "BitOr" => Ok(BitOr),
            "LShift" => Ok(LShift),
            "RShift" => Ok(RShift),
            "Concat" => Ok(Concat),
            "Lt" => Ok(Lt),
            "Lte" => Ok(Lte),
            "Gt" => Ok(Gt),
            "Gte" => Ok(Gte),
            "Eq" => Ok(Eq),
            "Neq" => Ok(Neq),
            "And" => Ok(And),
            "Or" => Ok(Or),
            _ => Err(()),
        }
    }
}

struct Traverser<'s> {
    errors: Vec<ParseError<'s>>,
}

impl<'s> Traverser<'s> {
    fn new() -> Traverser<'s> {
        Traverser { errors: vec![] }
    }

    fn parse(mut self, visitor: Visitor<'s, '_, '_>) -> Result<Chunk, Vec<ParseError<'s>>> {
        let block = self.parse_block(visitor);
        if self.errors.is_empty() {
            Ok(block)
        } else {
            Err(mem::take(&mut self.errors))
        }
    }

    fn parse_block(&mut self, visitor: Visitor<'s, '_, '_>) -> Block {
        // Collect every statement that isn't blank.
        let mut stats = visitor
            .iter_left_chain("Semi")
            .filter(|v| v.name() != "Blank")
            .collect::<Vec<_>>();

        // If the last statement is a `return` statement, pull it out and parse it
        let mut retstat = None;
        let ends_in_return = stats.last().map(|v| v.name() == "Return").unwrap_or(false);
        if ends_in_return {
            let last_stat = stats.pop().unwrap();
            let [explist] = last_stat.children();
            retstat = Some(self.parse_explist(explist));
        }

        // Parse the rest of the statements
        let stats = stats
            .into_iter()
            .map(|v| self.parse_stat(v))
            .collect::<Vec<_>>();
        Block { stats, retstat }
    }

    fn parse_stat(&mut self, visitor: Visitor<'s, '_, '_>) -> Stat {
        unimplemented!();
    }

    fn parse_explist(&mut self, visitor: Visitor<'s, '_, '_>) -> ExpList {
        visitor
            .iter_left_chain("Comma")
            .map(|v| self.parse_exp(v))
            .collect::<Vec<_>>()
    }

    fn parse_exp(&mut self, visitor: Visitor<'s, '_, '_>) -> Exp {
        let name = visitor.name();
        if let Ok(unop) = UnOp::from_str(name) {
            let [right_arg] = visitor.children();
            // TODO: not needed
            if right_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(right_arg, format!("`{}` needs a right argument", op));
            }
            let right_arg = self.parse_exp(right_arg);
            Exp::UnOp(unop, Box::new(right_arg))
        } else if let Ok(binop) = BinOp::from_str(name) {
            let [left_arg] = visitor.children();
            let [right_arg] = visitor.children();
            // TODO: not needed
            if left_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(left_arg, format!("`{}` needs a left argument", op));
            }
            if right_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(right_arg, format!("`{}` needs a right argument", op));
            }
            let left_arg = self.parse_exp(left_arg);
            let right_arg = self.parse_exp(right_arg);
            Exp::BinOp(binop, Box::new(left_arg), Box::new(right_arg))
        } else {
            match visitor.name() {
                "Nil" => Exp::Nil,
                "False" => Exp::False,
                "True" => Exp::True,
                "Elipses" => Exp::Elipses,
                "Number" => match f64::from_str(visitor.source()) {
                    Ok(num) => Exp::Number(num),
                    Err(err) => {
                        self.error(visitor, err);
                        Exp::Number(0.0)
                    }
                },
                // TODO: string escapes
                "String" => Exp::String(visitor.source().to_owned()),
                "Table" => {
                    let [fields] = visitor.children();
                    let fields = self.parse_fieldlist(fields);
                    Exp::TableConstructor(fields)
                }
                "Function" => {
                    let [name, params, body] = visitor.children();
                    if name.name() != "Blank" {
                        self.error(visitor, "A function expression cannot have a name");
                    }
                    let params = self.parse_parlist(params);
                    let body = self.parse_block(body);
                    Exp::Function { params, body }
                }
                "Parens" => {
                    let [inside] = visitor.children();
                    let inside = self.parse_exp(inside);
                    Exp::Parens(Box::new(inside))
                }
                "FunctionCall" => {
                    let call = self.parse_function_call(visitor);
                    Exp::FunctionCall(call)
                }
                "Name" => Exp::Var(Box::new(Var::Name(self.parse_name(visitor)))),
                "Brackets" => {
                    let [table, field] = visitor.children();
                    let table = self.parse_exp(table);
                    let field = self.parse_exp(field);
                    Exp::Var(Box::new(Var::Brackets(Box::new(table), Box::new(field))))
                }
                "Dot" => {
                    let [table, field] = visitor.children();
                    let table = self.parse_exp(table);
                    let field = self.parse_name(field);
                    Exp::Var(Box::new(Var::Dot(Box::new(table), field)))
                }
                _ => {
                    self.unexpected_node(visitor, "expression");
                    Exp::Nil
                }
            }
        }
    }

    /// Caller must ensure that visitor.name() == "FunctionCall"
    fn parse_function_call(&mut self, visitor: Visitor<'s, '_, '_>) -> FunctionCall {
        let [func, args] = visitor.children();
        if func.name() == "Colon" {
            let [before_colon, after_colon] = func.children();
            let before_colon = self.parse_exp(func);
            let after_colon = self.parse_name(after_colon);
            let args = self.parse_args(args);
            FunctionCall::Qualified(Box::new(before_colon), after_colon, args)
        } else {
            let func = self.parse_exp(func);
            let args = self.parse_args(args);
            FunctionCall::Unqualified(Box::new(func), args)
        }
    }

    fn parse_name(&mut self, visitor: Visitor<'s, '_, '_>) -> String {
        match visitor.name() {
            "Name" => visitor.source().to_owned(),
            _ => {
                self.unexpected_node(visitor, "name");
                String::new()
            }
        }
    }

    fn parse_args(&mut self, visitor: Visitor<'s, '_, '_>) -> Args {
        match visitor.name() {
            "Parens" => {
                let [explist] = visitor.children();
                let explist = self.parse_explist(explist);
                Args::List(explist)
            }
            "Table" => {
                let [fields] = visitor.children();
                let fields = self.parse_fieldlist(fields);
                Args::TableConstructor(fields)
            }
            // TODO: String escapes
            "String" => Args::String(visitor.source().to_owned()),
            "Blank" => {
                self.error(visitor, "Missing argument list");
                Args::List(Vec::new())
            }
            bad_name => {
                self.error(
                    visitor,
                    format!("Expected argument list, but found {}", bad_name),
                );
                Args::List(Vec::new())
            }
        }
    }

    fn parse_parlist(&mut self, visitor: Visitor<'s, '_, '_>) -> ParList {
        let mut names = visitor.iter_left_chain("Comma").collect::<Vec<_>>();
        let ends_in_ellipses = names
            .last()
            .map(|v| v.name() == "Ellipses")
            .unwrap_or(false);
        if ends_in_ellipses {
            names.pop();
        }
        let names = names
            .into_iter()
            .map(|v| self.parse_name(v))
            .collect::<Vec<_>>();
        ParList {
            names,
            ellipses: ends_in_ellipses,
        }
    }

    fn parse_partial_fieldlist<'p, 't>(
        &mut self,
        fieldlist: &mut Vec<Field>,
        varname: Option<Visitor<'s, 'p, 't>>,
        visitor: Visitor<'s, 'p, 't>,
    ) -> Option<Visitor<'s, 'p, 't>> {
        match visitor.name() {
            "Semi" | "Comma" => {
                let [left, right] = visitor.children();
                let middle = self.parse_partial_fieldlist(fieldlist, varname, left);
                if let Some(middle) = middle {
                    fieldlist.push(self.parse_field(middle));
                }
                let last = self.parse_partial_fieldlist(fieldlist, None, right);
                last
            }
            "Equals" => {
                let [left, right] = visitor.children();
                let middle = self.parse_partial_fieldlist(fieldlist, varname, left);
                if middle.is_none() {
                    self.error(visitor, "Fields must be separated by ',' or ';'");
                }
                let last = self.parse_partial_fieldlist(fieldlist, middle, right);
                last
            }
            _ => {
                if let Some(varname) = varname {
                    let varname = self.parse_name(varname);
                    let value = self.parse_exp(visitor);
                    fieldlist.push(Field::Equals(varname, Box::new(value)));
                    None
                } else {
                    Some(visitor)
                }
            }
        }
    }

    fn parse_fieldlist(&mut self, visitor: Visitor<'s, '_, '_>) -> FieldList {
        let mut fieldlist = Vec::new();
        let last = self.parse_partial_fieldlist(&mut fieldlist, None, visitor);
        if let Some(last) = last {
            // Trailing field sep is allowed
            if last.name() != "Blank" {
                fieldlist.push(self.parse_field(last));
            }
        }
        fieldlist
    }

    fn parse_field(&mut self, visitor: Visitor<'s, '_, '_>) -> Field {
        match visitor.name() {
            "BracketedField" => {
                let [name, value] = visitor.children();
                let name = self.parse_exp(name);
                let value = self.parse_exp(value);
                Field::Brackets(Box::new(name), Box::new(value))
            }
            "Equals" => {
                let [name, value] = visitor.children();
                let name = self.parse_name(name);
                let value = self.parse_exp(value);
                Field::Equals(name, Box::new(value))
            }
            _ => Field::Plain(Box::new(self.parse_exp(visitor))),
        }
    }

    fn parse_varlist(&mut self, visitor: Visitor<'s, '_, '_>) -> VarList {
        visitor
            .iter_left_chain("Comma")
            .map(|v| self.parse_var(v))
            .collect::<Vec<_>>()
    }

    fn parse_var(&mut self, visitor: Visitor<'s, '_, '_>) -> Var {
        match visitor.name() {
            "Brackets" => {
                let [object, fieldexp] = visitor.children();
                let object = self.parse_exp(object);
                let fieldexp = self.parse_exp(fieldexp);
                Var::Brackets(Box::new(object), Box::new(fieldexp))
            }
            "Dot" => {
                let [object, fieldname] = visitor.children();
                let object = self.parse_exp(object);
                let fieldname = self.parse_name(fieldname);
                Var::Dot(Box::new(object), fieldname)
            }
            "Name" => {
                let name = self.parse_name(visitor);
                Var::Name(name)
            }
            _ => {
                self.unexpected_node(visitor, "variable");
                Var::Name(String::new())
            }
        }
    }

    fn unexpected_node(&mut self, visitor: Visitor<'s, '_, '_>, context: &str) {
        match visitor.name() {
            "Blank" => self.error(visitor, format!("Missing {}", context)),
            bad_name => self.error(
                visitor,
                format!("Expected {}, but found {}", context, bad_name),
            ),
        }
    }

    fn error(&mut self, visitor: Visitor<'s, '_, '_>, message: impl ToString) {
        self.errors.push(visitor.error(message));
    }
}

fn main() {
    eprintln!(
        "Reading from stdin. If you're using this interactively, end your input with Ctrl-D."
    );

    // Read input
    let parser = make_lua_parser().unwrap();
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
