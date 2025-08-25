//! This module defines types which represent [Token]s.

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Keyword(Keyword),
    Ident { name: String },
    Num { val: f64 },
    Str { val: String },
    Comment { body: String },
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
    Less,
    LessEq,
    Greater,
    Pipe,
    GreaterEq,
    Invalid,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Keyword(keyword) => write!(f, "{keyword}"),
            Token::Ident { name } => write!(f, "{name}"),
            Token::Num { val } => write!(f, "{val:?}"),
            Token::Str { val } => write!(f, "{val:?}"),
            Token::Comment { .. } => write!(f, "comment"),
            Token::Eq => write!(f, "="),
            Token::EqEq => write!(f, "=="),
            Token::Bang => write!(f, "!"),
            Token::BangEq => write!(f, "!="),
            Token::Semi => write!(f, ";"),
            Token::Plus => write!(f, "+"),
            Token::PlusPlus => write!(f, "++"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Dot => write!(f, "."),
            Token::Colon => write!(f, ":"),
            Token::Comma => write!(f, ","),
            Token::ParenL => write!(f, "("),
            Token::ParenR => write!(f, ")"),
            Token::CurlyL => write!(f, "{{"),
            Token::CurlyR => write!(f, "}}"),
            Token::Less => write!(f, "<"),
            Token::LessEq => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::Pipe => write!(f, "|"),
            Token::GreaterEq => write!(f, ">="),
            Token::Invalid => write!(f, "invalid token"),
        }
    }
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

impl std::str::FromStr for Keyword {
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

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Keyword::True => write!(f, "true"),
            Keyword::False => write!(f, "false"),
            Keyword::Nil => write!(f, "nil"),
            Keyword::Let => write!(f, "let"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Print => write!(f, "print"),
            Keyword::If => write!(f, "if"),
            Keyword::Then => write!(f, "then"),
            Keyword::Else => write!(f, "else"),
            Keyword::For => write!(f, "for"),
            Keyword::While => write!(f, "while"),
            Keyword::Do => write!(f, "do"),
            Keyword::Break => write!(f, "break"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Return => write!(f, "return"),
            Keyword::And => write!(f, "and"),
            Keyword::Or => write!(f, "or"),
        }
    }
}
