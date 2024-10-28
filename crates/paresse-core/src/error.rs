use crate::prelude::{ScanError, Token};

#[derive(Debug)]
pub enum Error {
    Scan(ScanError),
    Parse(ParseError),
}

impl From<ScanError> for Error {
    fn from(e: ScanError) -> Self {
        Self::Scan(e)
    }
}

impl From<ParseError> for Error {
    fn from(e: ParseError) -> Self {
        Self::Parse(e)
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEof,
    Expected {
        expected: &'static [u16],
        found: Token,
    },
}
