use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let,
    Fn,
    Print,
    Ident(String),
    Num(f64),
    Str(String),
    If,
    Then,
    Else,
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
    Invalid(char),
}

pub struct Tokens<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Tokens<'a> {
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
                '"' => {
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
                                    return Some(Token::Invalid('\\'));
                                }
                            }
                            _ => string_content.push(ch),
                        }
                    }
                    Some(Token::Str(string_content))
                }
                ch if ch.is_alphabetic() => {
                    let mut ident = String::new();
                    ident.push(ch);
                    while let Some(ch) = self.input.next_if(|ch| ch.is_alphanumeric()) {
                        ident.push(ch);
                    }
                    match ident.as_str() {
                        "let" => Some(Token::Let),
                        "fn" => Some(Token::Fn),
                        "print" => Some(Token::Print),
                        "if" => Some(Token::If),
                        "then" => Some(Token::Then),
                        "else" => Some(Token::Else),
                        _ => Some(Token::Ident(ident)),
                    }
                }
                ch if ch.is_ascii_digit() => {
                    let mut num_str = String::new();
                    num_str.push(ch);
                    while let Some(ch) = self.input.next_if(|ch| ch.is_ascii_digit() || *ch == '.')
                    {
                        num_str.push(ch);
                    }
                    let num = num_str.parse().unwrap();
                    Some(Token::Num(num))
                }
                ch => Some(Token::Invalid(ch)),
            };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple() {
        let input = "let x = 5;";
        let mut tokenizer = Tokens::new(input);
        assert_eq!(tokenizer.next(), Some(Token::Let));
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
