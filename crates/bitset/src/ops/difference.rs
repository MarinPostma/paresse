use std::ops::Sub;

use crate::{BitSet, BitSetLike, BlockRepr};

pub struct Difference<A, B> {
    a: A,
    b: B,
}

impl<A, B> Difference<A, B> {
    pub fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A, B> BitSetLike for Difference<A, B>
    where A: BitSetLike, B: BitSetLike
{
    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        let mut a_blocks = self.a.blocks();
        let mut b_blocks = self.b.blocks();

        std::iter::from_fn(move || {
            match (a_blocks.next(), b_blocks.next()) {
                (None, _) => None,
                (Some(b), None) => Some(b),
                (Some(a), Some(b)) => Some(a & !b),
            }
        })
    }

    fn contains(&self, e: u32) -> bool {
        self.a.contains(e) && !self.b.contains(e)
    }
}

impl<T> Sub<T> for BitSet
where
    T: BitSetLike
{
    type Output = Difference<BitSet, T>;

    fn sub(self, rhs: T) -> Self::Output {
        Difference::new(self, rhs)
    }
}
