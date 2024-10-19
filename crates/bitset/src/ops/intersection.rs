use std::ops::BitAnd;

use crate::{BitSet, BitSetLike, BlockRepr};

pub struct Intersection<A, B> {
    a: A,
    b: B,
}

impl<A, B> BitSetLike for Intersection<A, B>
where
    A: BitSetLike,
    B: BitSetLike,
{
    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        let mut iter_a = self.a.blocks();
        let mut iter_b = self.b.blocks();

        std::iter::from_fn(move || match (iter_a.next(), iter_b.next()) {
            (Some(a), Some(b)) => Some(a & b),
            _ => None,
        })
    }

    fn contains(&self, e: u32) -> bool {
        self.a.contains(e) && self.b.contains(e)
    }
}

impl<A, B> Intersection<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<B> BitAnd<B> for &BitSet
where
    B: BitSetLike,
{
    type Output = BitSet;

    fn bitand(self, rhs: B) -> Self::Output {
        self.intersection(rhs).collect()
    }
}

#[cfg(test)]
mod test {
    use crate::bitset;

    #[test]
    fn intersection() {
        let a = bitset![1, 3, 7, 999, 521];
        let b = bitset![2, 7, 999, 523, 521];

        assert_eq!(&a & &b, bitset![7, 999, 521]);
    }
}
