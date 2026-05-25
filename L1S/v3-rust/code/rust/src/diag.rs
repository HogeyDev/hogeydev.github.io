use crate::span::{Span, SourceFile};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub notes: Vec<String>,
}

pub struct Diagnostics {
    pub diags: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Diagnostics { diags: Vec::new() }
    }

    pub fn error(&mut self, msg: impl Into<String>, span: Option<Span>) {
        self.diags.push(Diagnostic {
            severity: Severity::Error,
            message: msg.into(),
            span,
            notes: Vec::new(),
        });
    }

    pub fn warn(&mut self, msg: impl Into<String>, span: Option<Span>) {
        self.diags.push(Diagnostic {
            severity: Severity::Warning,
            message: msg.into(),
            span,
            notes: Vec::new(),
        });
    }

    pub fn has_errors(&self) -> bool {
        self.diags.iter().any(|d| d.severity == Severity::Error)
    }

    pub fn emit(&self, source: &SourceFile) {
        for diag in &self.diags {
            let sev = match diag.severity {
                Severity::Error => "error",
                Severity::Warning => "warning",
                Severity::Note => "note",
            };
            if let Some(span) = &diag.span {
                let (line, col) = source.line_col(span.start);
                eprintln!("{}[{}:{}]: {}", sev, line, col, diag.message);
                let line_str = source.get_line(line);
                eprintln!(" {} | {}", line, line_str);
                let underline_len = std::cmp::max(1, span.end.saturating_sub(span.start));
                eprintln!("   | {}{}", " ".repeat(col.saturating_sub(1)), "^".repeat(underline_len));
            } else {
                eprintln!("{}: {}", sev, diag.message);
            }
            for note in &diag.notes {
                eprintln!("  note: {}", note);
            }
        }
    }
}
