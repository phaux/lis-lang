use std::iter::Peekable;
use std::str::{Chars, FromStr};

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Ident(String),
    Num(f64),
    Str(String),
    Eq,
    EqEq,
    Bang,
    BangEq,
    Semi,
    Plus,
    PlusPlus,
    Minus,
    Star,
    Slash,
    Dot,
    Colon,
    Comma,
    ParenL,
    ParenR,
    CurlyL,
    CurlyR,
    Invalid(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Keyword {
    True,
    False,
    Nil,
    Let,
    Fn,
    Print,
    If,
    Then,
    Else,
    For,
    While,
    Do,
    Break,
    Continue,
    Return,
    And,
    Or,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "true" => Ok(Keyword::True),
            "false" => Ok(Keyword::False),
            "nil" => Ok(Keyword::Nil),
            "let" => Ok(Keyword::Let),
            "fn" => Ok(Keyword::Fn),
            "print" => Ok(Keyword::Print),
            "if" => Ok(Keyword::If),
            "then" => Ok(Keyword::Then),
            "else" => Ok(Keyword::Else),
            "for" => Ok(Keyword::For),
            "while" => Ok(Keyword::While),
            "do" => Ok(Keyword::Do),
            "break" => Ok(Keyword::Break),
            "continue" => Ok(Keyword::Continue),
            "return" => Ok(Keyword::Return),
            "and" => Ok(Keyword::And),
            "or" => Ok(Keyword::Or),
            _ => Err(()),
        }
    }
}

pub struct Tokens<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Tokens<'a> {
    #[must_use]
    pub fn new(input: &'a str) -> Self {
        Tokens {
            input: input.chars().peekable(),
        }
    }
}

impl Iterator for Tokens<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return match self.input.next()? {
                ch if ch.is_whitespace() => continue,
                '=' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Some(Token::EqEq)
                    } else {
                        Some(Token::Eq)
                    }
                }
                '+' => {
                    if self.input.next_if_eq(&'+').is_some() {
                        Some(Token::PlusPlus)
                    } else {
                        Some(Token::Plus)
                    }
                }
                '!' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Some(Token::BangEq)
                    } else {
                        Some(Token::Bang)
                    }
                }
                ';' => Some(Token::Semi),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Star),
                '/' => Some(Token::Slash),
                '.' => Some(Token::Dot),
                ':' => Some(Token::Colon),
                ',' => Some(Token::Comma),
                '(' => Some(Token::ParenL),
                ')' => Some(Token::ParenR),
                '{' => Some(Token::CurlyL),
                '}' => Some(Token::CurlyR),
                '"' => Some(self.parse_str()),
                ch if ch.is_alphabetic() || ch == '_' => Some(self.parse_word(ch)),
                ch if ch.is_ascii_digit() => Some(self.parse_num(ch)),
                ch => Some(Token::Invalid(ch.to_string())),
            };
        }
    }
}

impl Tokens<'_> {
    fn parse_num(&mut self, first: char) -> Token {
        let mut num_str = String::new();
        num_str.push(first);
        while let Some(ch) = self.input.next_if(|ch| ch.is_ascii_digit() || *ch == '.') {
            num_str.push(ch);
        }
        let Ok(num) = num_str.parse::<f64>() else {
            return Token::Invalid(num_str);
        };
        Token::Num(num)
    }

    fn parse_word(&mut self, first: char) -> Token {
        let mut ident = String::new();
        ident.push(first);
        while let Some(ch) = self.input.next_if(|ch| ch.is_alphanumeric() || *ch == '_') {
            ident.push(ch);
        }
        match Keyword::from_str(&ident) {
            Ok(keyword) => Token::Keyword(keyword),
            Err(()) => Token::Ident(ident),
        }
    }

    fn parse_str(&mut self) -> Token {
        let mut string_content = String::new();
        while let Some(ch) = self.input.next() {
            match ch {
                '"' => break,
                '\\' => {
                    if let Some(escaped) = self.input.next() {
                        match escaped {
                            'n' => string_content.push('\n'),
                            't' => string_content.push('\t'),
                            'r' => string_content.push('\r'),
                            '\\' => string_content.push('\\'),
                            '"' => string_content.push('"'),
                            _ => string_content.push(escaped),
                        }
                    } else {
                        return Token::Invalid("\\".to_string());
                    }
                }
                _ => string_content.push(ch),
            }
        }
        Token::Str(string_content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let input = "let x = 5;";
        let mut tokenizer = Tokens::new(input);
        assert_eq!(tokenizer.next(), Some(Token::Keyword(Keyword::Let)));
        assert_eq!(tokenizer.next(), Some(Token::Ident("x".to_string())));
        assert_eq!(tokenizer.next(), Some(Token::Eq));
        assert_eq!(tokenizer.next(), Some(Token::Num(5.0)));
        assert_eq!(tokenizer.next(), Some(Token::Semi));
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn string_with_escapes() {
        let input = r#""hello\nworld\twith\\quotes\"""#;
        let mut tokenizer = Tokens::new(input);
        assert_eq!(
            tokenizer.next(),
            Some(Token::Str("hello\nworld\twith\\quotes\"".to_string()))
        );
        assert_eq!(tokenizer.next(), None);
    }

    #[test]
    fn string_concat_operator() {
        let input = "hello ++ world";
        let mut tokenizer = Tokens::new(input);
        assert_eq!(tokenizer.next(), Some(Token::Ident("hello".to_string())));
        assert_eq!(tokenizer.next(), Some(Token::PlusPlus));
        assert_eq!(tokenizer.next(), Some(Token::Ident("world".to_string())));
        assert_eq!(tokenizer.next(), None);
    }
}
