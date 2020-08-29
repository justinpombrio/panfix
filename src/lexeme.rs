#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    LexError,
    Missing,
    Juxtapose,
    Normal(u32),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Lexeme<'s> {
    pub token: Token,
    pub span: &'s str,
}
