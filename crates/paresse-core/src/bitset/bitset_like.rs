use super::{BitSet, BlockRepr, IntoU32};
use super::ops::union::Union;
use super::ops::intersection::Intersection;
use super::ops::difference::Difference;
use super::blocks_iter::BlocksIter;

/// This trait represent types that behave like bitset, and operation that can be performed on them
pub trait BitSetLike {
    fn blocks(&self) -> impl Iterator<Item = BlockRepr>;
    fn contains(&self, e: impl IntoU32) -> bool;

    /// Collect the content of self into a bitset
    fn collect(&self) -> BitSet {
        BitSet::from_blocks(self.blocks().collect())
    }

    /// Returns an iterator over the items of this bitset
    fn items(&self) -> BlocksIter<impl Iterator<Item = BlockRepr>> {
        let var_name = BlocksIter::new(self.blocks());
        var_name
    }

    /// Count the items in the bitset
    fn count(&self) -> usize {
        self.blocks().map(|b| b.count_ones() as usize).sum()
    }

    /// Returns whether the bitset is empty
    fn is_empty(&self) -> bool {
        !self.blocks().any(|b| b != 0)
    }

    /// Returns the union of this bitset with another bitset
    fn union<O>(&self, other: O) -> Union<&Self, O>
    where
        Self: Sized,
    {
        Union::new(self, other)
    }

    /// Returns the insersection of this bitset with another bitset
    fn intersection<O>(&self, other: O) -> Intersection<&Self, O> {
        Intersection::new(self, other)
    }

    fn difference<O>(&self, other: O) -> Difference<&Self, O> {
        Difference::new(self, other)
    }
}

impl<T: BitSetLike> BitSetLike for &T {
    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        T::blocks(self)
    }

    fn contains(&self, e: impl IntoU32) -> bool {
        T::contains(self, e)
    }
}
