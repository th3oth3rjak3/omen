#[derive(Debug, Clone, Copy)]
pub struct Position {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: 1,
            column: 0,
            offset: 0,
        }
    }
}

impl Position {
    pub fn new(row: usize, column: usize, offset: usize) -> Self {
        Self {
            line: row,
            column,
            offset,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: Default::default(),
            end: None,
        }
    }
}

impl Span {
    pub fn new(start: Position, end: Option<Position>) -> Self {
        Self { start, end }
    }
}
