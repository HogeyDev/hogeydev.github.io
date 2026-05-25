use crate::span::Span;

#[derive(Clone, Debug, PartialEq)]
pub enum Severity { Error, Warning }

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub span: Span,
    pub message: String,
    pub severity: Severity,
}

#[derive(Clone, Debug, Default)]
pub struct Diagnostics {
    pub diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self { diagnostics: vec![] }
    }

    pub fn error(&mut self, span: Span, msg: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            span, message: msg.into(), severity: Severity::Error,
        });
    }

    pub fn warn(&mut self, span: Span, msg: impl Into<String>) {
        self.diagnostics.push(Diagnostic {
            span, message: msg.into(), severity: Severity::Warning,
        });
    }

    pub fn has_errors(&self) -> bool {
        self.diagnostics.iter().any(|d| d.severity == Severity::Error)
    }

    pub fn merge(&mut self, other: Diagnostics) {
        self.diagnostics.extend(other.diagnostics);
    }

    pub fn emit(&self, source: &str) {
        for d in &self.diagnostics {
            let sev = if d.severity == Severity::Error { "error" } else { "warning" };
            let line = source[..d.span.start].matches('\n').count() + 1;
            let col = d.span.start - source[..d.span.start].rfind('\n').map(|i| i + 1).unwrap_or(0);
            eprintln!("{}:{}:{}: {}: {}", line, col, d.span.end - d.span.start, sev, d.message);
        }
    }
}
