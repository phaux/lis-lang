//! This module defines types which represent [Token]s.

#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    pub sigil: Sigil,
    /// Byte offset of the token in the input string.
    pub offset: std::ops::Range<usize>,
    /// Position of the token in the input string.
    pub pos: std::ops::Range<Pos>,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}` at {}", self.sigil, self.pos.start)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Sigil {
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
    Invalid,
}

impl std::fmt::Display for Sigil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Sigil::Keyword(keyword) => write!(f, "{keyword}"),
            Sigil::Ident { name } => write!(f, "{name}"),
            Sigil::Num { val } => write!(f, "{val:?}"),
            Sigil::Str { val } => write!(f, "{val:?}"),
            Sigil::Comment { .. } => write!(f, "comment"),
            Sigil::Eq => write!(f, "="),
            Sigil::EqEq => write!(f, "=="),
            Sigil::Bang => write!(f, "!"),
            Sigil::BangEq => write!(f, "!="),
            Sigil::Semi => write!(f, ";"),
            Sigil::Plus => write!(f, "+"),
            Sigil::PlusPlus => write!(f, "++"),
            Sigil::Minus => write!(f, "-"),
            Sigil::Star => write!(f, "*"),
            Sigil::Slash => write!(f, "/"),
            Sigil::Dot => write!(f, "."),
            Sigil::Colon => write!(f, ":"),
            Sigil::Comma => write!(f, ","),
            Sigil::ParenL => write!(f, "("),
            Sigil::ParenR => write!(f, ")"),
            Sigil::CurlyL => write!(f, "{{"),
            Sigil::CurlyR => write!(f, "}}"),
            Sigil::Invalid => write!(f, "invalid token"),
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

/// Line and column.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone, Copy)]
pub struct Pos {
    /// 0-based line number.
    pub line: usize,
    /// 0-based column number.
    pub col: usize,
}

impl std::fmt::Display for Pos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}
