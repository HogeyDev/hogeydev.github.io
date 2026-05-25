#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

pub struct SourceFile {
    pub source: String,
    line_starts: Vec<usize>,
}

impl SourceFile {
    pub fn new(source: String) -> Self {
        let mut line_starts = vec![0usize];
        for (i, c) in source.char_indices() {
            if c == '\n' {
                line_starts.push(i + 1);
            }
        }
        SourceFile { source, line_starts }
    }

    pub fn line_col(&self, pos: usize) -> (usize, usize) {
        if pos >= self.source.len() {
            let last = self.line_starts.len();
            return (last, 1);
        }
        match self.line_starts.binary_search(&pos) {
            Ok(line) => (line + 1, 1),
            Err(line) => {
                let line = if line > 0 { line - 1 } else { 0 };
                let col = pos - self.line_starts[line] + 1;
                (line + 1, col)
            }
        }
    }

    pub fn get_line(&self, line: usize) -> &str {
        let start = self.line_starts[line - 1];
        let end = if line < self.line_starts.len() {
            self.line_starts[line]
        } else {
            self.source.len()
        };
        let end = end.min(self.source.len());
        if end > start && self.source.as_bytes()[end - 1] == b'\n' {
            if end > start + 1 && self.source.as_bytes()[end - 2] == b'\r' {
                &self.source[start..end - 2]
            } else {
                &self.source[start..end - 1]
            }
        } else {
            &self.source[start..end]
        }
    }
}
