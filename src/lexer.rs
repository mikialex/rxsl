use std::ops::Range;

use crate::ast::ParseError;

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Keyword {
    If,
    ElseIf,
    Else,
    For,
    While,
    Return,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Separator(char),
    DoubleColon,
    Paren(char),
    DoubleParen(char),
    Number {
        value: &'a str,
        ty: char,
        width: &'a str,
    },
    Bool(bool),
    String(&'a str),
    Word(&'a str),
    Keyword(Keyword),
    Operation(char),
    LogicalOperation(char),
    ShiftOperation(char),
    Arrow,
    Unknown(char),
    UnterminatedString,
    Trivia,
    End,
}

pub type TokenSpan<'a> = (Token<'a>, Range<usize>);

#[derive(Clone, Debug)]
pub struct Lexer<'a> {
    input: &'a str,
    source: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Lexer {
            input,
            source: input,
        }
    }

    fn peek_token_and_rest(&self) -> (TokenSpan<'a>, &'a str) {
        let mut cloned = self.clone();
        let token = cloned.next();
        let rest = cloned.input;
        (token, rest)
    }

    fn current_byte_offset(&self) -> usize {
        self.source.len() - self.input.len()
    }

    #[must_use]
    pub fn next(&mut self) -> TokenSpan<'a> {
        let mut start_byte_offset = self.current_byte_offset();
        loop {
            let (token, rest) = consume_token(self.input, false);
            self.input = rest;
            match token {
                Token::Trivia => start_byte_offset = self.current_byte_offset(),
                _ => return (token, start_byte_offset..self.current_byte_offset()),
            }
        }
    }

    #[must_use]
    pub fn next_generic(&mut self) -> TokenSpan<'a> {
        let mut start_byte_offset = self.current_byte_offset();
        loop {
            let (token, rest) = consume_token(self.input, true);
            self.input = rest;
            match token {
                Token::Trivia => start_byte_offset = self.current_byte_offset(),
                _ => return (token, start_byte_offset..self.current_byte_offset()),
            }
        }
    }

    #[must_use]
    pub fn peek(&self) -> TokenSpan<'a> {
        let (token, _) = self.peek_token_and_rest();
        token
    }

    pub fn expect(&mut self, expected: Token<'a>) -> Result<(), ParseError<'a>> {
        let next = self.next();
        if next.0 == expected {
            Ok(())
        } else {
            let description = match expected {
                Token::Separator(_) => "separator",
                Token::DoubleColon => "::",
                Token::Paren(_) => "paren",
                Token::DoubleParen(_) => "double paren",
                Token::Number { .. } => "number",
                Token::String(string) => string,
                Token::Word(word) => word,
                Token::Keyword(_) => "Keyword",
                Token::Operation(_) => "operation",
                Token::LogicalOperation(_) => "logical op",
                Token::ShiftOperation(_) => "shift op",
                Token::Arrow => "->",
                Token::Unknown(_) => "unknown",
                Token::UnterminatedString => "string",
                Token::Trivia => "trivia",
                Token::Bool(_) => "boolean",
                Token::End => "",
            };
            Err(ParseError::Unexpected(next, description))
        }
    }

    pub fn skip(&mut self, what: Token<'_>) -> bool {
        let (peeked_token, rest) = self.peek_token_and_rest();
        if peeked_token.0 == what {
            self.input = rest;
            true
        } else {
            false
        }
    }
}

fn consume_any(input: &str, what: impl Fn(char) -> bool) -> (&str, &str) {
    let pos = input.find(|c| !what(c)).unwrap_or_else(|| input.len());
    input.split_at(pos)
}

fn consume_number(input: &str) -> (Token, &str) {
    //Note: I wish this function was simpler and faster...
    let mut is_first_char = true;
    let mut right_after_exponent = false;

    let mut what = |c| {
        if is_first_char {
            is_first_char = false;
            c == '-' || ('0'..='9').contains(&c) || c == '.'
        } else if c == 'e' || c == 'E' {
            right_after_exponent = true;
            true
        } else if right_after_exponent {
            right_after_exponent = false;
            ('0'..='9').contains(&c) || c == '-'
        } else {
            ('0'..='9').contains(&c) || c == '.'
        }
    };
    let pos = input.find(|c| !what(c)).unwrap_or_else(|| input.len());
    let (value, rest) = input.split_at(pos);

    let mut rest_iter = rest.chars();
    let ty = rest_iter.next().unwrap_or(' ');
    match ty {
        'u' | 'i' | 'f' => {
            let width_end = rest_iter
                .position(|c| !('0'..='9').contains(&c))
                .unwrap_or_else(|| rest.len() - 1);
            let (width, rest) = rest[1..].split_at(width_end);
            (Token::Number { value, ty, width }, rest)
        }
        // default to `i32` or `f32`
        _ => (
            Token::Number {
                value,
                ty: if value.contains('.') { 'f' } else { 'i' },
                width: "",
            },
            rest,
        ),
    }
}

fn consume_token(mut input: &str, generic: bool) -> (Token<'_>, &str) {
    let mut chars = input.chars();
    let cur = match chars.next() {
        Some(c) => c,
        None => return (Token::End, input),
    };
    match cur {
        ':' => {
            input = chars.as_str();
            if chars.next() == Some(':') {
                (Token::DoubleColon, chars.as_str())
            } else {
                (Token::Separator(cur), input)
            }
        }
        ';' | ',' => (Token::Separator(cur), chars.as_str()),
        '.' => {
            let og_chars = chars.as_str();
            match chars.next() {
                Some('0'..='9') => consume_number(input),
                _ => (Token::Separator(cur), og_chars),
            }
        }
        '(' | ')' | '{' | '}' => (Token::Paren(cur), chars.as_str()),
        '<' | '>' => {
            input = chars.as_str();
            let next = chars.next();
            if next == Some('=') && !generic {
                (Token::LogicalOperation(cur), chars.as_str())
            } else if next == Some(cur) && !generic {
                (Token::ShiftOperation(cur), chars.as_str())
            } else {
                (Token::Paren(cur), input)
            }
        }
        '[' | ']' => {
            input = chars.as_str();
            if chars.next() == Some(cur) {
                (Token::DoubleParen(cur), chars.as_str())
            } else {
                (Token::Paren(cur), input)
            }
        }
        '0'..='9' => consume_number(input),
        'a'..='z' | 'A'..='Z' | '_' => {
            let (word, rest) = consume_any(input, |c| c.is_ascii_alphanumeric() || c == '_');
            match word {
                "true" => (Token::Bool(true), rest),
                "false" => (Token::Bool(false), rest),
                "if" => (Token::Keyword(Keyword::If), rest),
                "elseif" => (Token::Keyword(Keyword::ElseIf), rest),
                "else" => (Token::Keyword(Keyword::Else), rest),
                "while" => (Token::Keyword(Keyword::While), rest),
                "for" => (Token::Keyword(Keyword::For), rest),
                "return" => (Token::Keyword(Keyword::Return), rest),
                _ => (Token::Word(word), rest),
            }
        }
        '"' => {
            let mut iter = chars.as_str().splitn(2, '"');

            // splitn returns an iterator with at least one element, so unwrapping is fine
            let quote_content = iter.next().unwrap();
            if let Some(rest) = iter.next() {
                (Token::String(quote_content), rest)
            } else {
                (Token::UnterminatedString, quote_content)
            }
        }
        '/' if chars.as_str().starts_with('/') => {
            let _ = chars.position(|c| c == '\n' || c == '\r');
            (Token::Trivia, chars.as_str())
        }
        '-' => {
            let og_chars = chars.as_str();
            match chars.next() {
                Some('>') => (Token::Arrow, chars.as_str()),
                Some('0'..='9') | Some('.') => consume_number(input),
                _ => (Token::Operation(cur), og_chars),
            }
        }
        '+' | '*' | '/' | '%' | '^' => (Token::Operation(cur), chars.as_str()),
        '!' => {
            input = chars.as_str();
            if chars.next() == Some('=') {
                (Token::LogicalOperation(cur), chars.as_str())
            } else {
                (Token::Operation(cur), input)
            }
        }
        '=' | '&' | '|' => {
            input = chars.as_str();
            if chars.next() == Some(cur) {
                (Token::LogicalOperation(cur), chars.as_str())
            } else {
                (Token::Operation(cur), input)
            }
        }
        ' ' | '\n' | '\r' | '\t' => {
            let (_, rest) = consume_any(input, |c| c == ' ' || c == '\n' || c == '\r' || c == '\t');
            (Token::Trivia, rest)
        }
        _ => (Token::Unknown(cur), chars.as_str()),
    }
}
