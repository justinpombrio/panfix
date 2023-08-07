use panfix::{pattern, Grammar, GrammarError, ParseError, Parser, Source, Visitor};
use std::collections::HashMap;
use std::mem;
use std::str::FromStr;

// NOTE: Bugs I encountered when writing this, as a sample:
// - A couple instances of getting the precedence wrong. E.g. making "+" bind looser than ".", so
//   that `x.a + 1` would parse as `x.(a + 1)`.
// - I bungled the syntax of function calls. In fairness, it's a bit unusual.
// - A lexing mistake: `x-1` would be parsed as `x -1`, where `-1` was a number.

// ORDERING:
// "," < "=" in `for`, `local`
// "=" < "," in `fieldlist`

fn make_lua_parser() -> Result<Parser, GrammarError> {
    let mut grammar = Grammar::new("(--[^\n]*\n|[ \n\r\t])+")?;

    // Nullary Expressions
    grammar.regex("Name", r#"[a-zA-Z_][a-zA-Z0-9_]*"#)?;
    grammar.regex("Label", r#"::[a-zA-Z_][a-zA-Z0-9_]*::"#)?;
    grammar.string("Nil", "nil")?;
    grammar.string("True", "true")?;
    grammar.string("False", "false")?;
    // TODO: These are JSON numerals, which don't exactly match Lua.
    // Especially since Lua allows hex numerals starting with 0x.
    grammar.regex("Numeral", r#"(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+-]?\d+)?"#)?;
    // TODO: These are JSON strings, which might not exactly match Lua.
    grammar.regex("String", r#""([^\\"]|(\\.))*""#)?;
    grammar.string("Ellipses", "...")?;
    grammar.op("Parens", pattern!("(" ")"))?;
    grammar.op("Table", pattern!("{" "}"))?;

    // Nullary Statements
    grammar.string("Break", "break")?;
    grammar.op("Function", pattern!("function" "(" ")" "end"))?;
    grammar.op("Do", pattern!("do" "end"))?;
    grammar.op("While", pattern!("while" "do" "end"))?;
    grammar.op("If", pattern!("if" "then" "end"))?;
    grammar.op("For", pattern!("for" "do" "end"))?;

    // Prefix expressions
    grammar.right_assoc();
    grammar.op("Dot", pattern!(_ "." _))?;
    grammar.op("Brackets", pattern!(_ "[" "]"))?;
    grammar.right_assoc();
    grammar.op("Colon", pattern!(_ ":" _))?;

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

    // Function calls
    grammar.left_assoc();
    grammar.op("FunctionCallParens", pattern!(_ "(" ")"))?;
    grammar.op("FunctionCallTable", pattern!(_ "{" "}"))?;

    // field with brackets -- found inside `tableconstructor` expr
    grammar.left_assoc();
    grammar.op("BracketedField", pattern!("[" "]" "=" _))?;

    // Expression separator
    grammar.right_assoc();
    grammar.op("Comma", pattern!(_ "," _))?;

    // Statements
    grammar.left_assoc();
    grammar.op("Equals", pattern!(_ "=" _))?;
    grammar.op("Goto", pattern!("goto" _))?;
    grammar.op("Repeat", pattern!("repeat" "until" _))?;
    grammar.op("Return", pattern!("return" _))?;
    grammar.left_assoc();
    grammar.op("Local", pattern!("local" _))?;

    // in -- found inside `for` statement
    grammar.left_assoc();
    grammar.op("In", pattern!(_ "in" _))?;

    // else & elseif -- found inside `if` statement
    grammar.right_assoc();
    grammar.op("Else", pattern!(_ "else" _))?;
    grammar.op("Elseif", pattern!(_ "elseif" "then" _))?;

    // Statement separator
    grammar.right_assoc();
    grammar.op("Semi", pattern!(_ ";" _))?;
    grammar.juxtapose()?;

    grammar.finish()
}

fn human_readable_name(op_name: &str) -> &str {
    match op_name {
        "Name" => "name",
        "Label" => "label",
        "Nil" => "'nil'",
        "False" => "'false'",
        "True" => "'true'",
        "Numeral" => "number",
        "String" => "string literal",
        "Ellipses" => "'...'",
        "Parens" => "parentheses",
        "Table" => "table constructor",
        "Break" => "'break'",
        "Break" => "'break'",
        "Function" => "function",
        "Do" => "'do' statement",
        "While" => "'while' statement",
        "If" => "'if' statement",
        "If" => "'for' loop",
        "Dot" => "'.'",
        "Brackets" => "square brackets",
        "Colon" => "':'",
        "Exponentiate" => "'^'",
        "Neg" => "'-'",
        "BitwiseNeg" => "'~'",
        "Not" => "'not'",
        "Len" => "'#'",
        "Times" => "'*'",
        "Divide" => "'/'",
        "FloorDivide" => "'//'",
        "Modulo" => "'%'",
        "Plus" => "'+'",
        "Minus" => "'-'",
        "Concat" => "'..'",
        "LShift" => "'<<'",
        "RShift" => "'>>'",
        "BitAnd" => "'&'",
        "BitXor" => "'~'",
        "BitOr" => "'|'",
        "Lt" => "'<'",
        "Lte" => "'<='",
        "Gt" => "'>'",
        "Gte" => "'>='",
        "Eq" => "'=='",
        "Neq" => "'~='",
        "And" => "'and'",
        "Or" => "'or'",
        "Semi" => "';'",
        "Else" => "'else' clause",
        "Elseif" => "'elseif' clause",
        "In" => "'in'",
        "Local" => "'local'",
        "Return" => "'return' statement",
        "Repeat" => "'repeat' loop",
        "Goto" => "'goto' statement",
        "Equals" => "'='",
        "Comma" => "','",
        "BracketField" => "field access ('_[_]')",
        "FunctionCallParens" => "function call ('_(_)')",
        "FunctionCallTable" => "function call ('_{_}')",
        _ => op_name,
    }
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
        cases: Vec<(Box<Exp>, Block)>,
        else_: Option<Block>,
    },
    /// for $name = $exp, $exp, $exp do $block end
    For {
        var: Name,
        start: Box<Exp>,
        end: Box<Exp>,
        by: Option<Box<Exp>>,
        body: Block,
    },
    /// for $name, ... in $exp, ... do $block end
    ForIn(NameList, ExpList, Block),
    /// function $name.$name:$name($parlist) $block end
    Function {
        name: Name,
        dots: Vec<Name>,
        colon: Option<Name>,
        parlist: ParList,
        body: Block,
    },
    /// local function $name($parlist) $block end
    LocalFunction {
        name: Name,
        parlist: ParList,
        body: Block,
    },
    /// local $name, ... = $exp, ...
    Local(NameList, ExpList),
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
    Ellipses,
    /// function($parlist) $block end
    Function {
        parlist: ParList,
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

    fn parse_block(&mut self, mut visitor: Visitor<'s, '_, '_>) -> Block {
        // Collect every statement that isn't blank.
        let mut stats = Vec::new();
        while visitor.name() == "Semi" || visitor.name() == "Juxtapose" {
            let [stat, rest] = visitor.children();
            if stat.name() != "Blank" {
                stats.push(stat);
            }
            visitor = rest;
        }
        if visitor.name() != "Blank" {
            stats.push(visitor);
        }

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

    fn parse_explist(&mut self, visitor: Visitor<'s, '_, '_>) -> ExpList {
        visitor
            .iter_right_chain("Comma")
            .map(|v| self.parse_exp(v))
            .collect::<Vec<_>>()
    }

    fn parse_exp(&mut self, visitor: Visitor<'s, '_, '_>) -> Exp {
        let name = visitor.name();

        if let Ok(unop) = UnOp::from_str(name) {
            let [right_arg] = visitor.children();
            let right_arg = if right_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(right_arg, format!("`{}` needs a right argument", op));
                Exp::Nil
            } else {
                self.parse_exp(right_arg)
            };
            return Exp::UnOp(unop, Box::new(right_arg));
        }

        if let Ok(binop) = BinOp::from_str(name) {
            let [left_arg, right_arg] = visitor.children();
            // TODO: not needed
            let left_arg = if left_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(left_arg, format!("`{}` needs a left argument", op));
                Exp::Nil
            } else {
                self.parse_exp(left_arg)
            };
            let right_arg = if right_arg.name() == "Blank" {
                let op = visitor.token_source();
                self.error(right_arg, format!("`{}` needs a right argument", op));
                Exp::Nil
            } else {
                self.parse_exp(right_arg)
            };
            return Exp::BinOp(binop, Box::new(left_arg), Box::new(right_arg));
        }

        match visitor.name() {
            "Nil" => Exp::Nil,
            "False" => Exp::False,
            "True" => Exp::True,
            "Ellipses" => Exp::Ellipses,
            "Numeral" => match f64::from_str(visitor.source()) {
                Ok(num) => Exp::Number(num),
                Err(err) => {
                    self.error(visitor, err);
                    Exp::Number(0.0)
                }
            },
            "String" => Exp::String(self.parse_string_literal(visitor)),
            "Table" => {
                let [fields] = visitor.children();
                let fields = self.parse_fieldlist(fields);
                Exp::TableConstructor(fields)
            }
            "Function" => {
                let [name, parlist, body] = visitor.children();
                if name.name() != "Blank" {
                    self.error(visitor, "A function expression cannot have a name");
                }
                let parlist = self.parse_parlist(parlist);
                let body = self.parse_block(body);
                Exp::Function { parlist, body }
            }
            "Parens" => {
                let [inside] = visitor.children();
                let inside = self.parse_exp(inside);
                Exp::Parens(Box::new(inside))
            }
            "FunctionCallParens" | "FunctionCallTable" | "Juxtapose" => {
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

    fn parse_function_call(&mut self, visitor: Visitor<'s, '_, '_>) -> FunctionCall {
        let (func, args) = match visitor.name() {
            "FunctionCallParens" => {
                let [func, explist] = visitor.children();
                let explist = self.parse_explist(explist);
                (func, Args::List(explist))
            }
            "FunctionCallTable" => {
                let [func, fieldlist] = visitor.children();
                let fieldlist = self.parse_fieldlist(fieldlist);
                (func, Args::TableConstructor(fieldlist))
            }
            "Juxtapose" => {
                let [func, string] = visitor.children();
                let string = self.parse_string_literal(string);
                (func, Args::String(string))
            }
            _ => panic!("bug: bad invocation of parse_function_call"), // ensured by callers
        };

        if func.name() == "Colon" {
            let [before_colon, after_colon] = func.children();
            let before_colon = self.parse_exp(before_colon);
            let after_colon = self.parse_name(after_colon);
            FunctionCall::Qualified(Box::new(before_colon), after_colon, args)
        } else {
            let func = self.parse_exp(func);
            FunctionCall::Unqualified(Box::new(func), args)
        }
    }

    fn parse_string_literal(&mut self, visitor: Visitor<'s, '_, '_>) -> String {
        // TODO: string escapes, and remove quotes
        visitor.source().to_owned()
    }

    fn parse_namelist(&mut self, visitor: Visitor<'s, '_, '_>) -> NameList {
        visitor
            .iter_right_chain("Comma")
            .map(|v| self.parse_name(v))
            .collect::<Vec<_>>()
    }

    fn parse_name(&mut self, visitor: Visitor<'s, '_, '_>) -> Name {
        match visitor.name() {
            "Name" => visitor.source().to_owned(),
            _ => {
                self.unexpected_node(visitor, "name");
                String::new()
            }
        }
    }

    fn parse_parlist(&mut self, visitor: Visitor<'s, '_, '_>) -> ParList {
        let mut names = visitor.iter_right_chain("Comma").collect::<Vec<_>>();
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
            .iter_right_chain("Comma")
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

    fn parse_stat(&mut self, visitor: Visitor<'s, '_, '_>) -> Stat {
        match visitor.name() {
            // TODO: string ::s
            "Label" => Stat::Label(visitor.source().to_string()),
            "Break" => Stat::Break,
            "Goto" => {
                let [name] = visitor.children();
                let name = self.parse_name(name);
                Stat::Goto(name)
            }
            "Do" => {
                let [block] = visitor.children();
                let block = self.parse_block(block);
                Stat::Do(block)
            }
            "FunctionCallParens" | "FunctionCallTable" => {
                let call = self.parse_function_call(visitor);
                Stat::FunctionCall(call)
            }
            "Equals" => {
                let [varlist, explist] = visitor.children();
                let varlist = self.parse_varlist(varlist);
                let explist = self.parse_explist(explist);
                Stat::Variable(varlist, explist)
            }
            "While" => {
                let [cond, block] = visitor.children();
                let cond = self.parse_exp(cond);
                let block = self.parse_block(block);
                Stat::While(Box::new(cond), block)
            }
            "Repeat" => {
                let [block, cond] = visitor.children();
                let block = self.parse_block(block);
                let cond = self.parse_exp(cond);
                Stat::Repeat(block, Box::new(cond))
            }
            "If" => {
                // First, gather all of the condition->consequence pairs, and the optional final
                // 'else'. We marked the 'else' and 'elseif's as right-associative in the grammar,
                // which means that we'll encounter them in reverse order, but it also makes this
                // code a bit cleaner.
                let mut cases = Vec::new();
                let mut else_ = None;

                let [first_cond, mut body] = visitor.children();
                if body.name() == "Else" {
                    let [rest, else_block] = body.children();
                    body = rest;
                    else_ = Some(else_block);
                }
                while body.name() == "Elseif" {
                    let [rest, cond, then] = body.children();
                    body = rest;
                    cases.push((cond, then));
                }
                cases.push((first_cond, body));
                cases.reverse();

                // Now parse everything.
                let cases = cases
                    .into_iter()
                    .map(|(cond, consq)| (Box::new(self.parse_exp(cond)), self.parse_block(consq)))
                    .collect::<Vec<_>>();
                let else_ = else_.map(|v| self.parse_block(v));

                Stat::If { cases, else_ }
            }
            "For" => {
                let [iteration, body] = visitor.children();
                if iteration.name() == "In" {
                    let [namelist, explist] = iteration.children();
                    if namelist.name() == "Blank" {
                        self.error(namelist, "The variable list in a `for` cannot be empty");
                    }
                    if explist.name() == "Blank" {
                        self.error(namelist, "The expression list in a `for` cannot be empty");
                    }
                    let namelist = self.parse_namelist(namelist);
                    let explist = self.parse_explist(explist);
                    let body = self.parse_block(body);
                    Stat::ForIn(namelist, explist, body)
                } else if iteration.name() == "Equals" {
                    let [var, bounds] = iteration.children();
                    let bounds_list = bounds.iter_right_chain("Comma").collect::<Vec<_>>();
                    if bounds_list.len() != 2 && bounds_list.len() != 3 {
                        self.error(bounds, "A for loop iteration bounds must be either `min, max` or `min, max, by`");
                        return self.empty_stat();
                    }
                    let var = self.parse_name(var);
                    let mut bounds_iter = bounds_list.into_iter();
                    let start = self.parse_exp(bounds_iter.next().unwrap());
                    let end = self.parse_exp(bounds_iter.next().unwrap());
                    let by = bounds_iter.next().map(|v| self.parse_exp(v));
                    let body = self.parse_block(body);
                    Stat::For {
                        var,
                        start: Box::new(start),
                        end: Box::new(end),
                        by: by.map(|by| Box::new(by)),
                        body,
                    }
                } else {
                    self.error(iteration, "A `for` loop must have either the form `for $vars = $exps do $block end` or `for $name in $exp do $block end`");
                    self.empty_stat()
                }
            }
            "Function" => {
                let [mut fullname, parlist, body] = visitor.children();
                // Gather optional part of name after colon
                let mut colon = None;
                if fullname.name() == "Colon" {
                    let [rest, after_colon] = fullname.children();
                    fullname = rest;
                    colon = Some(after_colon);
                }
                // Parse dotted name
                let mut name_iter = fullname.iter_right_chain("Dot");
                let name = if let Some(name) = name_iter.next() {
                    self.parse_name(name)
                } else {
                    self.error(fullname, "Missing name");
                    String::new()
                };
                let dots = name_iter.map(|v| self.parse_name(v)).collect::<Vec<_>>();
                let colon = colon.map(|v| self.parse_name(v));
                // Parse parameter list and body
                let parlist = self.parse_parlist(parlist);
                let body = self.parse_block(body);

                Stat::Function {
                    name,
                    dots,
                    colon,
                    parlist,
                    body,
                }
            }
            "Local" => {
                let [visitor] = visitor.children();
                match visitor.name() {
                    "Equals" => {
                        let [namelist, explist] = visitor.children();
                        if namelist.name() == "Blank" {
                            self.error(
                                namelist,
                                "The variable list in a `local` definition cannot be empty",
                            );
                        }
                        if explist.name() == "Blank" {
                            self.error(
                                namelist,
                                "The expression list in a `local` definition cannot be empty",
                            );
                        }
                        let namelist = self.parse_namelist(namelist);
                        let explist = self.parse_explist(explist);
                        Stat::Local(namelist, explist)
                    }
                    "Function" => {
                        let [name, parlist, body] = visitor.children();
                        let name = self.parse_name(name);
                        let parlist = self.parse_parlist(parlist);
                        let body = self.parse_block(body);
                        Stat::LocalFunction {
                            name,
                            parlist,
                            body,
                        }
                    }
                    _ => {
                        self.unexpected_node(visitor, "local definition");
                        self.empty_stat()
                    }
                }
            }
            _ => {
                self.unexpected_node(visitor, "statement");
                self.empty_stat()
            }
        }
    }

    fn empty_stat(&self) -> Stat {
        Stat::Do(Block {
            stats: Vec::new(),
            retstat: None,
        })
    }

    fn unexpected_node(&mut self, visitor: Visitor<'s, '_, '_>, context: &str) {
        match visitor.name() {
            "Blank" => self.error(visitor, format!("Missing {}", context)),
            bad_name => self.error(
                visitor,
                format!(
                    "Expected {}, but found {}",
                    context,
                    human_readable_name(bad_name)
                ),
            ),
        }
    }

    fn error(&mut self, visitor: Visitor<'s, '_, '_>, message: impl ToString) {
        self.errors.push(visitor.error(message));
    }
}

fn main() {
    eprintln!(
        "Reading from stdin. If you're using this interactively, end your input with Ctrl-D.\n"
    );

    // Read input
    let parser = make_lua_parser().unwrap();
    let source = Source::from_stdin().unwrap();

    // Parse and print
    match parser.parse(&source) {
        Ok(tree) => {
            println!("{}\n", tree.visitor());
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
