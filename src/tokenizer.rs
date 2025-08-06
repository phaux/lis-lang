use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let,
    Print,
    Ident(String),
    Num(f64),
    If,
    Then,
    Else,
    Eq,
    EqEq,
    Bang,
    BangEq,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    ParenL,
    ParenR,
    CurlyL,
    CurlyR,
    Invalid(char),
}

pub struct Tokenizer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Tokenizer {
            input: input.chars().peekable(),
        }
    }
}

impl Iterator for Tokenizer<'_> {
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
                '!' => {
                    if self.input.next_if_eq(&'=').is_some() {
                        Some(Token::BangEq)
                    } else {
                        Some(Token::Bang)
                    }
                }
                ';' => Some(Token::Semi),
                '+' => Some(Token::Plus),
                '-' => Some(Token::Minus),
                '*' => Some(Token::Star),
                '/' => Some(Token::Slash),
                '(' => Some(Token::ParenL),
                ')' => Some(Token::ParenR),
                '{' => Some(Token::CurlyL),
                '}' => Some(Token::CurlyR),
                ch if ch.is_alphabetic() => {
                    let mut ident = String::new();
                    ident.push(ch);
                    while let Some(ch) = self.input.next_if(|ch| ch.is_alphanumeric()) {
                        ident.push(ch);
                    }
                    match ident.as_str() {
                        "let" => Some(Token::Let),
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

#[test]
fn test_tokenizer() {
    let input = "let x = 5;";
    let mut tokenizer = Tokenizer::new(input);
    assert_eq!(tokenizer.next(), Some(Token::Let));
    assert_eq!(tokenizer.next(), Some(Token::Ident("x".to_string())));
    assert_eq!(tokenizer.next(), Some(Token::Eq));
    assert_eq!(tokenizer.next(), Some(Token::Num(5.0)));
    assert_eq!(tokenizer.next(), Some(Token::Semi));
    assert_eq!(tokenizer.next(), None);
}
