#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Word(&'a str),
    Space(&'a str),
    Number(&'a str),
    Unknown(&'a str),
    EOF,
}

pub struct Lexer<'a> {
    input: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer { input }
    }
}
