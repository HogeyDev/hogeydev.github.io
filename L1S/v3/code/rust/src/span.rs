#[derive(Clone, Copy, Debug, Default)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn combine(self, other: Span) -> Span {
        Span::new(
            self.start.min(other.start),
            self.end.max(other.end),
        )
    }
}
