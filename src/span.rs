use std::ops::Range;

/// A wrapper type which adds information about node's position in source code.
#[derive(Debug, PartialEq, Clone)]
pub struct Span<T> {
    pub range: Range<Pos>,
    pub node: T,
}

impl<T: std::fmt::Display> std::fmt::Display for Span<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`{}` at {}", self.node, self.range.start)
    }
}

impl<T> Span<T> {
    #[must_use]
    pub fn without_node(&self) -> Span<()> {
        Span {
            range: self.range.clone(),
            node: (),
        }
    }

    #[must_use]
    pub fn with_node<U>(&self, node: U) -> Span<U> {
        Span {
            range: self.range.clone(),
            node,
        }
    }
}

/// Position in the input string.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Default, Clone, Copy)]
pub struct Pos {
    /// Byte offset.
    pub offset: usize,
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
