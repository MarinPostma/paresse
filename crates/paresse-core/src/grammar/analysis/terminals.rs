use std::ops::{Deref, DerefMut};

use crate::grammar::SymbolSet;

use super::NonTerminals;

#[derive(Debug)]
pub struct Terminals {
    inner: SymbolSet,
}

impl Deref for Terminals {
    type Target = SymbolSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Terminals {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl Terminals {
    pub fn compute(non_terminals: &NonTerminals, symbols: &SymbolSet) -> Self {
        let mut inner = symbols.difference(non_terminals);
        inner.remove_epsilon();
        inner.add_eof();
        Self { inner }
    }
}
