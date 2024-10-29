#![allow(dead_code)]

#[derive(Debug)]
pub struct ScanError {
    /// offset in the source of the tokenizer error
    loc: usize,
    kind: ScanErrorKind,
}

#[derive(Debug)]
pub enum ScanErrorKind {
    UnexpectedChar(u8),
}

impl ScanError {
    pub fn new(loc: usize, kind: ScanErrorKind) -> Self {
        Self { loc, kind }
    }
}
