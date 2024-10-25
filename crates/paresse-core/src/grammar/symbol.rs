use std::ops::{BitOr, Deref, DerefMut};

use crate::bitset::{BitSet, BitSetLike as _, IntoU32};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Symbol(u32);

// impement only into because, it's always ok to convert a Symbol to u32, but not the other way
// around.
#[allow(clippy::from_over_into)]
impl IntoU32 for Symbol {
    #[inline]
    fn into_u32(self) -> u32 {
        self.as_u32()
    }
}

impl Symbol {
    #[inline]
    pub const fn as_u32(&self) -> u32 {
        self.0
    }

    pub const fn epsilon() -> Self {
        Self(0)
    }

    pub const fn is_epsilon(&self) -> bool {
        self.0 == 0
    }

    pub const fn eof() -> Self {
        Self(1)
    }

    pub const fn is_eof(&self) -> bool {
        self.0 == 1
    }

    pub const fn from_u32(u: u32) -> Self {
        Self(u)
    }
}

#[derive(Debug)]
pub struct SymbolSource {
    current: u32,
}

impl Default for SymbolSource {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolSource {
    pub fn new() -> Self {
        Self { current: Symbol::eof().as_u32() }
    }
}

impl Iterator for SymbolSource {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.current += 1;
        Some(Symbol(self.current))
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SymbolSet {
    inner: BitSet,
}

impl SymbolSet {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_epsilon(&mut self) {
        self.inner.insert(Symbol::epsilon());
    }

    pub fn remove_epsilon(&mut self) {
        self.inner.remove(Symbol::epsilon());
    }

    pub fn contains_epsilon(&self) -> bool {
        self.inner.contains(Symbol::epsilon())
    }
    
    pub fn add(&mut self, s: Symbol) {
        self.inner.insert(s.as_u32())
    }

    pub fn difference(&self, other: &Self) -> Self {
        Self { inner: self.inner.difference(&other.inner).collect() }
    }

    pub fn contains(&self, s: Symbol) -> bool {
        self.inner.contains(s.as_u32())
    }

    pub fn iter(&self) -> Iter {
        Iter(self.inner.iter())
    }

    pub(crate) fn add_eof(&mut self) {
        self.inner.insert(Symbol::eof())
    }
}

impl Deref for SymbolSet {
    type Target = BitSet;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for SymbolSet {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

pub struct Iter<'a>(crate::bitset::Iter<'a>);

impl<'a> Iterator for Iter<'a> {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next().map(Symbol::from_u32)
    }
}

impl<'a> IntoIterator for &'a SymbolSet {
    type Item = Symbol;
    type IntoIter = Iter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl BitOr for &SymbolSet {
    type Output = SymbolSet;

    fn bitor(self, rhs: Self) -> Self::Output {
        SymbolSet { inner: &self.inner | &rhs.inner } 
    }
}
