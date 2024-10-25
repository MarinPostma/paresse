use std::fmt::{self, Display, Write};

use crate::either::Either;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum Unit {
    Boi,
    Eoi,
    Byte(u8),
}

impl Unit {
    pub fn all() -> impl Iterator<Item = Self> {
        let mut current = Self::Boi;
        std::iter::from_fn(move || {
            let next = current;
            current = match next {
                Unit::Boi => Unit::Eoi,
                Unit::Eoi => Unit::Byte(0),
                Unit::Byte(n) if n < u8::MAX => Unit::Byte(n + 1),
                _ => return None,
            };

            Some(next)
        })
    }
}

impl fmt::Debug for Unit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self}")
    }
}

impl Display for Unit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Unit::Boi => f.write_str("BOI"),
            Unit::Eoi => f.write_str("EOI"),
            Unit::Byte(c) => f.write_char(*c as char),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct ByteSet([u128; 2]);

impl ByteSet {
    pub(crate) fn empty() -> Self {
        Self([0, 0])
    }

    pub(crate) fn add(&mut self, byte: u8) {
        let bucket = byte / 128;
        let bit = byte % 128;
        self.0[bucket as usize] |= 1 << bit as u128
    }

    pub(crate) fn contains(&self, byte: u8) -> bool {
        let bucket = byte / 128;
        let bit = byte % 128;
        self.0[usize::from(bucket)] & (1 << bit) > 0
    }

    pub(crate) fn negate(&mut self) {
        self.0[0] = !self.0[0];
        self.0[1] = !self.0[1];
    }

    pub(crate) fn iter(&self) -> ByteSetIter {
        ByteSetIter::new(self)
    }

    pub(crate) fn len(&self) -> u32 {
        self.0[0].count_ones() + self.0[1].count_ones()
    }
}

pub(crate) struct ByteSetIter<'a> {
    inner: &'a ByteSet,
    c: usize,
}

impl<'a> ByteSetIter<'a> {
    pub(crate) fn new(inner: &'a ByteSet) -> Self {
        Self { inner, c: 0 }
    }
}

impl Iterator for ByteSetIter<'_> {
    type Item = u8;

    fn next(&mut self) -> Option<Self::Item> {
        while self.c <= 255 {
            let c = self.c as u8;
            self.c += 1;
            if self.inner.contains(c) {
                return Some(c);
            }
        }

        None
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Class {
    Boi,
    Eoi,
    Chars(ByteSet),
}

impl Class {
    pub(crate) fn matches(&self, unit: Unit) -> bool {
        match (self, unit) {
            (Class::Eoi, Unit::Eoi) | (Class::Boi, Unit::Boi) => true,
            (Class::Chars(s), Unit::Byte(c)) => s.contains(c),
            _ => false,
        }
    }

    /// Returns the units for that class
    pub(crate) fn units(&self) -> impl Iterator<Item = Unit> + '_ {
        match self {
            Class::Boi => Either::A(std::iter::once(Unit::Boi)),
            Class::Eoi => Either::A(std::iter::once(Unit::Eoi)),
            Class::Chars(c) => Either::B(c.iter().map(Unit::Byte)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn byteset() {
        let mut s = ByteSet::empty();
        assert!(!s.contains(b'a'));
        s.add(b'a');
        assert!(s.contains(b'a'));
        assert!(!s.contains(b'b'));
    }
}
