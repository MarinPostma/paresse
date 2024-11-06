mod bitset;
mod either;
mod error;
pub mod grammar;
pub mod lexer;

pub mod prelude {
    pub use crate::error::{Error, ParseError};
    pub use crate::lexer::error::{ScanError, ScanErrorKind};
    pub use crate::lexer::{Span, Token, Unit, ByteSet};
}
