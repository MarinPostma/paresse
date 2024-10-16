use crate::bitset::{bitset_like::BitSetLike, BlockRepr};

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

        std::iter::from_fn(move || match dbg!((iter_a.next(), iter_b.next())) {
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
