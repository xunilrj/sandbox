pub mod reporter;
mod utils;

use std::path::PathBuf;

use auto_from::From;
use proc_macro2::Span;

#[derive(Debug, From)]
pub enum ScanError {
    NoScannerFound,
    IO(std::io::Error),
}

pub trait Scanner {
    fn can_scan(&self, root: &str) -> bool;
    fn scan(&self, root: &str, result: &mut ScanContext) -> std::result::Result<(), ScanError>;
}

#[derive(Debug)]
pub enum ScanType {
    Security,
}

#[derive(Debug)]
pub struct ScanFinding {
    pub t: ScanType,
    pub source_file: PathBuf,
    pub at: Span,
    pub description: String,
}

impl ScanFinding {
    pub fn fmt_at(&self) -> String {
        format!(
            "{:?} ({:?} {:?})",
            self.source_file,
            self.at.start(),
            self.at.end()
        )
    }
}

#[derive(Debug)]
pub struct ScanResult {
    pub findings: Vec<ScanFinding>,
}

impl ScanResult {
    pub fn new() -> Self {
        Self {
            findings: Vec::new(),
        }
    }

    pub fn push_security(
        &mut self,
        source_file: &std::path::Path,
        at: proc_macro2::Span,
        description: &str,
    ) {
        let source_file = crate::utils::normalize_path(source_file);
        self.findings.push(ScanFinding {
            t: ScanType::Security,
            source_file,
            at: at.into(),
            description: description.to_string(),
        });
    }
}

pub struct ScanContext {
    pub path: Option<PathBuf>,
    pub result: ScanResult,
}

impl ScanContext {
    pub fn new() -> Self {
        Self {
            path: None,
            result: ScanResult::new(),
        }
    }
}
