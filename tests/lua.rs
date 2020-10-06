// TODO: Work in progress
#![allow(dead_code)]

mod common;

#[cfg(test)]
mod lua_tests {
    use super::common::run_parser;
    use panfix::parsing::{Grammar, Parser, WHITESPACE_REGEX};
    use panfix::{circumfix, infix, juxtapose, prefix, suffix};

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
            .ops(vec![
                circumfix!("Do", "do", "end"),
                circumfix!("While", "while", "do", "end"),
                circumfix!("If", "if", "then", "end"),
                circumfix!("For", "for", "do", "end"),
                circumfix!("Function", "function", "(", ")", "end"),
                circumfix!("Table", "{", "}"),
                circumfix!("Parens", "(", ")"),
            ])
            .op_r(infix!("Exp", "^"))
            .ops_r(vec![
                prefix!("Not", "not"),
                prefix!("?", "#"),
                prefix!("Neg", "-"),
            ])
            .ops_l(vec![
                infix!("Mul", "*"),
                infix!("Div", "/"),
                infix!("Mod", "%"),
            ])
            .ops_l(vec![infix!("Add", "+"), infix!("Sub", "-")])
            .op_l(infix!("Range", ".."))
            .ops_l(vec![
                infix!("Lt", "<"),
                infix!("Gt", ">"),
                infix!("Lte", "<="),
                infix!("Gte", ">="),
                infix!("Neq", "~="),
                infix!("Eq", "=="),
            ])
            .op_r(infix!("And", "and"))
            .op_r(infix!("Or", "or"))
            .op_r(infix!("Colon", ":"))
            .ops_l(vec![
                infix!("Dot", "."),
                suffix!("Get", "[", "]"),
                suffix!("Call", "(", ")"),
                suffix!("CallTable", "{", "}"),
                juxtapose!(),
            ])
            .op_r(infix!("Comma", ","))
            .op_r(infix!("Equals", "="))
            .op(prefix!("Return", "return"))
            .ops_r(vec![
                infix!("Else", "else"),
                infix!("ElseIf", "elseif", "then"),
                infix!("Repeat", "repeat", "until"),
                infix!("Local", "local"),
            ])
            .op_r(infix!("Semi", ";"))
            .build()
    }

    #[test]
    fn test_lua_parsing() {
        let parser = lua_parser();
        let parse = |input: &str| run_parser(&parser, input);

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
