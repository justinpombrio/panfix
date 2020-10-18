// TODO: Work in progress
#![allow(dead_code)]

mod common;

#[cfg(test)]
mod lua_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, Parser, WHITESPACE_REGEX};
    use panfix::{juxtapose, op};

    fn lua_parser() -> Parser {
        Grammar::new(WHITESPACE_REGEX)
            .regex("Name", "[a-zA-Z_][a-zA-Z0-9_]*")
            .regex("Number", "(0|[1-9][0-9]*)(\\.[0-9]*)?")
            .regex("String", "\"[^\"]*\"")
            .constant("Nil", "nil")
            .constant("True", "true")
            .constant("False", "false")
            .constant("Ellipses", "...")
            .constant("Break", "break")
            .op(op!(Do: "do" "end"))
            .op(op!(While: "while" "do" "end"))
            .op(op!(If: "if" "then" "end"))
            .op(op!(For: "for" "do" "end"))
            .op(op!(Function: "function" "(" ")" "end"))
            .op(op!(Table: "{" "}"))
            .op(op!(Parens: "(" ")"))
            .op_r(op!(Exp: _ "^" _))
            .ops_r(vec![op!(Not: "not" _), op!(Hash: "#" _), op!(Neg: "-" _)])
            .ops_l(vec![
                op!(Mul: _ "*" _),
                op!(Div: _ "/" _),
                op!(Mod: _ "%" _),
            ])
            .ops_l(vec![op!(Add: _ "+" _), op!(Sub: _ "-" _)])
            .op_l(op!(Range: _ ".." _))
            .ops_l(vec![
                op!(Lt: _ "<" _),
                op!(Gt: _ ">" _),
                op!(Lte: _ "<=" _),
                op!(Gte: _ ">=" _),
                op!(Neq: _ "~=" _),
                op!(Eq: _ "==" _),
            ])
            .op_r(op!(And: _ "and" _))
            .op_r(op!(Or: _ "or" _))
            .op_r(op!(Colon: _ ":" _))
            .ops_l(vec![
                op!(Dot: _ "." _),
                op!(Get: _ "[" "]"),
                op!(Call: _ "(" ")"),
                op!(CallTable: _ "{" "}"),
                juxtapose!(),
            ])
            .op_r(op!(Comma: _ "," _))
            .op_r(op!(Equals: _ "=" _))
            .op(op!(Return: "return" _))
            .ops_r(vec![
                op!(Else: _ "else" _),
                op!(ElseIf: _ "elseif" "then" _),
                op!(Repeat: _ "repeat" "until" _),
                op!(Local: _ "local" _),
            ])
            .op_r(op!(Semi: _ ";" _))
            .build()
    }

    #[test]
    fn test_lua_parsing() {
        let parser = lua_parser();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("  3 "), "3");
        /*
        assert_eq!(parse("\"hello, world\""), "\"hello, world\"");
        assert_eq!(parse("  3 "), "3");
        assert_eq!(parse("Foobar"), "Foobar");
        assert_eq!(parse("..."), "...");
        assert_eq!(parse("do nil end"), "(do nil end)");
        assert_eq!(
            parse("function...(false)break end"),
            "(function ... ( false ) break end)"
        );
        assert_eq!(parse("(2+3)"), "(( (2 + 3) ))");
        assert_eq!(parse("f(2+3)"), "(f ( (2 + 3) ))");
        assert_eq!(parse("-2*3-2*3"), "(((- 2) * 3) - (2 * 3))");
        assert_eq!(parse("a.b\"foo\""), "((a . b) ? \"foo\")");
        */
    }

    struct LuaProgram(Vec<Stmt>);
    type Block = Vec<Stmt>;
    type Name = String;
    enum Stmt {
        Block(Block),
        Assignment(Vec<Var>, Vec<Expr>),
        If {
            cond: Box<Expr>,
            r#then: Box<Expr>,
            elseifs: Vec<(Expr, Expr)>,
            r#else: Option<Box<Expr>>,
        },
        While(Box<Expr>, Block),
        Repeat(Block, Box<Expr>),
        For {
            name: Name,
            var: Box<Expr>,
            limit: Box<Expr>,
            step: Option<Box<Expr>>,
            block: Block,
        },
        Call(Call),
        Local(Vec<Name>, Vec<Expr>),
    }
    enum Expr {
        Binop(Box<Expr>, Binop, Box<Expr>),
        Unop(Unop, Box<Expr>),
        Table(Vec<Field>),
        Call(Call),
    }
    enum Field {
        NamedField(Name, Box<Expr>),
        NumberedField(Box<Expr>),
        ExprField(Box<Expr>, Box<Expr>),
    }
    enum Var {}
    struct Call();

    enum Binop {
        Exp,
        Mul,
        Div,
        Mod,
        Add,
        Sub,
        Lt,
        Gt,
        Lte,
        Gte,
        Eq,
        Neq,
        And,
        Or,
        Concat,
    }
    enum Unop {
        Not,
        Neg,
        Len,
    }
}
