use std::iter::Peekable;
use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Let,
    Print,
    Ident(String),
    Num(f64),
    Eq,
    Semi,
    Plus,
    Minus,
    Star,
    Slash,
    ParenL,
    ParenR,
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
            let c = self.input.peek()?;

            match c {
                c if c.is_whitespace() => {
                    self.input.next();
                    continue;
                }
                '=' => {
                    self.input.next();
                    return Some(Token::Eq);
                }
                ';' => {
                    self.input.next();
                    return Some(Token::Semi);
                }
                '+' => {
                    self.input.next();
                    return Some(Token::Plus);
                }
                '-' => {
                    self.input.next();
                    return Some(Token::Minus);
                }
                '*' => {
                    self.input.next();
                    return Some(Token::Star);
                }
                '/' => {
                    self.input.next();
                    return Some(Token::Slash);
                }
                '(' => {
                    self.input.next();
                    return Some(Token::ParenL);
                }
                ')' => {
                    self.input.next();
                    return Some(Token::ParenR);
                }
                c if c.is_alphabetic() => {
                    let mut ident = String::new();
                    while let Some(&c) = self.input.peek() {
                        if c.is_alphanumeric() {
                            ident.push(self.input.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    return match ident.as_str() {
                        "let" => Some(Token::Let),
                        "print" => Some(Token::Print),
                        _ => Some(Token::Ident(ident)),
                    };
                }
                c if c.is_ascii_digit() => {
                    let mut num_str = String::new();
                    while let Some(&c) = self.input.peek() {
                        if c.is_ascii_digit() || c == '.' {
                            num_str.push(self.input.next().unwrap());
                        } else {
                            break;
                        }
                    }
                    let num = num_str.parse().unwrap();
                    return Some(Token::Num(num));
                }
                _ => panic!("invalid character"),
            }
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
