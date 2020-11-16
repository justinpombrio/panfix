// TODO: Work in progress
#![allow(dead_code)]

mod common;

#[cfg(test)]
mod lua_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, Parser};
    use panfix::{juxtapose, op};

    fn lua_parser() -> Parser {
        Grammar::new("LuaTest")
            .regex("Name", "[a-zA-Z_][a-zA-Z0-9_]*")
            .regex("Number", "(0|[1-9][0-9]*)(\\.[0-9]*)?")
            .regex("String", "\"[^\"]*\"")
            .constant("Nil", "nil")
            .constant("True", "true")
            .constant("False", "false")
            .constant("Ellipses", "...")
            .constant("Break", "break")
            .subgrammar("FuncName", |builder| {
                builder.op(op!(Colon: _ ":" _)).op(op!(Dot: _ "." _))
            })
            .subgrammar("Expr", |builder| {
                builder
                    .op(op!(Do: "do" Expr "end"))
                    .op(op!(While: "while" Expr "do" Expr "end"))
                    .op(op!(If: "if" Expr "then" Expr "end"))
                    .op(op!(For: "for" Expr "do" Expr "end"))
                    .op(op!(Function: "function" FuncName "(" Expr ")" Expr "end"))
                    .op(op!(Table: "{" Expr "}"))
                    .op(op!(Parens: "(" Expr ")"))
                    .op(op!(Exp: _ "^" _)) // right assoc
                    .ops(
                        "Unary",
                        vec![op!(Not: "not" _), op!(Hash: "#" _), op!(Neg: "-" _)],
                    )
                    .ops(
                        "Multiplication",
                        vec![op!(Mul: _ "*" _), op!(Div: _ "/" _), op!(Mod: _ "%" _)],
                    )
                    .ops("Addition", vec![op!(Add: _ "+" _), op!(Sub: _ "-" _)])
                    .op(op!(Range: _ ".." _))
                    .ops(
                        "Comparison",
                        vec![
                            op!(Lt: _ "<" _),
                            op!(Gt: _ ">" _),
                            op!(Lte: _ "<=" _),
                            op!(Gte: _ ">=" _),
                            op!(Neq: _ "~=" _),
                            op!(Eq: _ "==" _),
                        ],
                    )
                    .op(op!(And: _ "and" _)) // right assoc
                    .op(op!(Or: _ "or" _)) // right assoc
                    .op(op!(Colon: _ ":" _)) // right assoc
                    .ops(
                        "Dotting",
                        vec![
                            op!(Dot: _ "." _),
                            op!(Get: _ "[" Expr "]"),
                            op!(Call: _ "(" Expr ")"),
                            op!(CallTable: _ "{" Expr "}"),
                            juxtapose!(),
                        ],
                    )
                    .op(op!(Comma: _ "," _)) // right_assoc
                    .op(op!(Equals: _ "=" _)) // right assoc
                    .op(op!(Return: "return" _))
                    .ops(
                        "Conditional",
                        vec![
                            op!(Else: _ "else" _),
                            op!(ElseIf: _ "elseif" Expr "then" _),
                            op!(Repeat: _ "repeat" Expr "until" _),
                            op!(Local: _ "local" _),
                        ],
                    ) // right assoc
                    .op(op!(Semi: _ ";" _)) // right assoc
            })
            .build("Expr")
    }

    #[test]
    fn test_lua_parsing() {
        let parser = lua_parser();
        let parse = |input: &str| run_parser(&parser, input);

        assert_eq!(parse("  3 "), "3");
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
