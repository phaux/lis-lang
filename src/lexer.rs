//! This module defines the [Lexer].
//! It transforms a string of characters into a stream of [Token]s.

use std::str::FromStr as _;

use crate::{
    span::{Pos, Span},
    token::{Keyword, Token},
};

pub struct Lexer<'a> {
    input: &'a str,
    /// Current position in the input string.
    pos: Pos,
}

impl<'a> Lexer<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            pos: Pos::default(),
        }
    }

    fn peek_char(&self) -> Option<char> {
        self.input[self.pos.offset..].chars().next()
    }

    fn next_char(&mut self) -> Option<char> {
        let ch = self.input[self.pos.offset..].chars().next()?;
        if ch == '\n' {
            self.pos.line += 1;
            self.pos.col = 0;
        } else {
            self.pos.col += 1;
        }
        self.pos.offset += ch.len_utf8();
        Some(ch)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Span<Token>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let pos_start = self.pos;
            let ch = self.next_char()?;
            let sigil = match ch {
                ch if ch.is_whitespace() => continue,
                // Comments: // line and /* block */
                '/' => match self.peek_char() {
                    Some('/') => self.parse_line_comment(),
                    Some('*') => self.parse_block_comment(),
                    _ => Token::Slash,
                },
                '=' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::EqEq
                    }
                    _ => Token::Eq,
                },
                '+' => match self.peek_char() {
                    Some('+') => {
                        self.next_char();
                        Token::PlusPlus
                    }
                    _ => Token::Plus,
                },
                '!' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::BangEq
                    }
                    _ => Token::Bang,
                },
                ';' => Token::Semi,
                '-' => Token::Minus,
                '*' => Token::Star,
                '.' => Token::Dot,
                ':' => Token::Colon,
                ',' => Token::Comma,
                '(' => Token::ParenL,
                ')' => Token::ParenR,
                '{' => Token::CurlyL,
                '}' => Token::CurlyR,
                '|' => Token::Pipe,
                '<' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::LessEq
                    }
                    _ => Token::Less,
                },
                '>' => match self.peek_char() {
                    Some('=') => {
                        self.next_char();
                        Token::GreaterEq
                    }
                    _ => Token::Greater,
                },
                '"' => self.parse_str(),
                ch if ch.is_alphabetic() || ch == '_' => self.parse_word(ch),
                ch if ch.is_ascii_digit() => self.parse_num(ch),
                _ => Token::Invalid,
            };
            return Some(Span {
                node: sigil,
                range: pos_start..self.pos,
            });
        }
    }
}

impl Lexer<'_> {
    fn parse_num(&mut self, first: char) -> Token {
        let mut val_str = String::new();
        val_str.push(first);
        while let Some(ch) = self.peek_char()
            && (ch.is_ascii_digit() || ch == '.')
        {
            self.next_char();
            val_str.push(ch);
        }
        let Ok(val) = val_str.parse::<f64>() else {
            return Token::Invalid;
        };
        Token::Num { val }
    }

    fn parse_word(&mut self, first: char) -> Token {
        let mut word = String::new();
        word.push(first);
        while let Some(ch) = self.peek_char()
            && (ch.is_alphanumeric() || ch == '_')
        {
            self.next_char();
            word.push(ch);
        }
        match Keyword::from_str(&word) {
            Ok(keyword) => Token::Keyword(keyword),
            Err(()) => Token::Ident { name: word },
        }
    }

    fn parse_str(&mut self) -> Token {
        let mut val = String::new();
        while let Some(ch) = self.next_char() {
            match ch {
                '"' => break,
                '\\' => match self.next_char() {
                    Some('n') => val.push('\n'),
                    Some('t') => val.push('\t'),
                    Some('r') => val.push('\r'),
                    Some('\\') => val.push('\\'),
                    Some('"') => val.push('"'),
                    Some(other) => val.push(other),
                    None => {}
                },
                ch => val.push(ch),
            }
        }
        Token::Str { val }
    }

    fn parse_line_comment(&mut self) -> Token {
        // Consume the second '/'
        self.next_char();
        let mut body = String::new();
        while let Some(ch) = self.peek_char() {
            if ch == '\n' {
                break;
            }
            self.next_char();
            body.push(ch);
        }
        Token::Comment { body }
    }

    fn parse_block_comment(&mut self) -> Token {
        // Consume the '*'
        self.next_char();
        let mut body = String::new();
        // Read until closing */ or EOF
        while let Some(ch) = self.next_char() {
            if ch == '*'
                && let Some('/') = self.peek_char()
            {
                self.next_char();
                break;
            }
            body.push(ch);
        }
        Token::Comment { body }
    }
}

#[cfg(test)]
#[path = "lexer.tests.rs"]
mod tests;
