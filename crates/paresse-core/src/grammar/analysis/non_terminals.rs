use std::ops::Deref;

use crate::grammar::{rule::Rule, SymbolSet};

#[derive(Debug)]
pub struct NonTerminals {
    inner: SymbolSet,
}

impl NonTerminals {
    pub fn compute(rules: &[Rule]) -> Self {
        let mut inner = SymbolSet::new();
        for rule in rules {
            inner.add(rule.lhs());
        }

        Self { inner }
    }
}

impl Deref for NonTerminals {
    type Target = SymbolSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}
