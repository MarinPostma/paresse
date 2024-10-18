use std::ops::{BitOr, BitOrAssign};

use crate::bitset::{BitSet, BlockRepr};
use crate::bitset::bitset_like::BitSetLike;

pub struct Union<A, B> {
    a: A,
    b: B,
}

impl<A, B> Union<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A, B> BitSetLike for Union<A, B>
where
    A: BitSetLike,
    B: BitSetLike,
{
    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        let mut iter_a = self.a.blocks();
        let mut iter_b = self.b.blocks();

        std::iter::from_fn(move || match (iter_a.next(), iter_b.next()) {
            (Some(a), Some(b)) => Some(a | b),
            (Some(a), None) => Some(a),
            (None, Some(b)) => Some(b),
            (None, None) => None,
        })
    }

    fn contains(&self, e: u32) -> bool {
        self.a.contains(e) || self.b.contains(e)
    }
}

impl<B> BitOr<B> for &BitSet
where
    B: BitSetLike,
{
    type Output = BitSet;

    fn bitor(self, rhs: B) -> Self::Output {
        self.union(rhs).collect()
    }
}

impl<B> BitOrAssign<B> for BitSet
where
    B: BitSetLike,
{
    fn bitor_assign(&mut self, rhs: B) {
        self.merge_with(rhs, |lhs, rhs| match (lhs, rhs) {
            (Some(lhs), Some(rhs)) => Some(lhs | rhs),
            (None, Some(rhs)) => Some(rhs),
            (_, None) => None,
        });
    }
}

#[cfg(test)]
mod test {
    use crate::bitset;

    #[test]
    fn union() {
        let a = bitset![1, 3, 7];
        let b = bitset![122, 4, 999];
        assert_eq!(&a | &b, bitset![1, 3, 7, 122, 4, 999]);
    }

    #[test]
    fn bit_or_assign() {
        let mut a = bitset![1, 2, 4];
        let b = bitset![5];

        a |= b;

        assert_eq!(a, bitset![1, 2, 4, 5]);
    }
}
