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
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self {
            line,
            column,
            offset,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[derive(Default)]
pub struct Span {
    pub start: Position,
    pub end: Option<Position>,
}


impl Span {
    pub fn new(start: Position, end: Option<Position>) -> Self {
        Self { start, end }
    }
}
