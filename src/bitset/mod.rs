use std::iter::Copied;
use std::fmt;

pub use bitset_like::BitSetLike;
pub use blocks_iter::BlocksIter;

mod bitset_like;
mod ops;
mod blocks_iter;

/// Integer type used to represent a block
type BlockRepr = u128;
pub type Iter<'a> = BlocksIter<Copied<std::slice::Iter<'a, BlockRepr>>>;

#[derive(Default, Clone, PartialEq, Eq)]
pub struct BitSet {
    blocks: Vec<BlockRepr>,
}

impl fmt::Debug for BitSet {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list().entries(self.iter()).finish()
    }
}

impl BitSet {
    pub fn new() -> Self {
        Self::default()
    }

    fn from_blocks(blocks: Vec<BlockRepr>) -> Self {
        Self { blocks }
    }

    pub fn max(&self) -> Option<u32> {
        for (idx, block) in self.blocks.iter().enumerate().rev() {
            if *block == 0 { continue }
            let last_bit_offset = BlockRepr::BITS - block.leading_zeros() - 1;
            return Some(idx as u32 * BlockRepr::BITS + last_bit_offset)
        }
        None
    }

    /// Returns whether the bitset is empty
    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty() || self.blocks.iter().all(|b| *b == 0)
    }

    /// Returns the number of items in this bitset
    pub fn len(&self) -> usize {
        self.blocks.iter().map(|b| b.count_ones() as usize).sum()
    }

    /// Returns the bucket and the bit for that element
    fn pos(&self, e: u32) -> (usize, BlockRepr) {
        let block = (e / BlockRepr::BITS) as usize;
        let bit = (e % BlockRepr::BITS) as BlockRepr;
        (block, bit)
    }

    /// Inset an element in the bitset
    pub fn insert(&mut self, e: u32) {
        let (block, bit) = self.pos(e);

        if block >= self.blocks.len() {
            self.blocks
                .extend(std::iter::repeat(0).take(1 + block - self.blocks.len()));
        }

        self.blocks[block] |= 1 << bit;
    }

    /// Remove an element from the bitset
    pub fn remove(&mut self, e: u32) {
        if self.is_empty() {
            return;
        }
        let (block, bit) = self.pos(e);
        self.blocks[block] &= !(1 << bit);
    }

    pub fn iter(&self) -> Iter {
        BlocksIter::new(self.blocks.iter().copied())
    }

    /// remove trailing empty blocks
    pub fn shrink(&mut self) {
        while let Some(&0) = self.blocks.last() {
            self.blocks.pop();
        }
    }

    fn merge_with<O, F>(&mut self, other: O, f: F)
    where
        O: BitSetLike,
        F: Fn(Option<BlockRepr>, Option<BlockRepr>) -> Option<BlockRepr>
    
    {
        let mut other_iter = other.blocks();

        let mut idx = 0;
        while let Some(block) = f(self.blocks.get(idx).copied(), other_iter.next()) {
            if idx < self.blocks.len() {
                self.blocks[idx] = block;
            } else {
                self.blocks.push(block);
            }
            idx += 1;
        }
    }
}

impl BitSetLike for BitSet {
    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        self.blocks.iter().copied()
    }

    fn contains(&self, e: u32) -> bool {
        if self.is_empty() {
            return false;
        }
        let (block, bit) = self.pos(e);
        (self.blocks[block] & 1 << bit) != 0
    }
}


#[macro_export]
macro_rules! bitset {
    ($($it:expr),*) => {
        {
            let mut bitset = $crate::bitset::BitSet::new();
            $(bitset.insert($it);)*
            bitset
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn inserts() {
        let mut bitset = BitSet::default();
        bitset.insert(12);
        assert!(bitset.contains(12));
        assert!(!bitset.contains(0));

        bitset.insert(525);
        assert!(bitset.contains(525));
    }

    #[test]
    fn iterator() {
        let mut bitset = BitSet::default();
        bitset.insert(12);
        bitset.insert(17);
        bitset.insert(525);
        bitset.insert(2344);

        let values = bitset.items().collect::<Vec<u32>>();
        assert_eq!(values, &[12, 17, 525, 2344]);
    }

    #[test]
    fn difference() {
        let a = bitset![1, 4, 1022, 1700];
        let b = bitset![1, 1022];

        let diff = a.difference(&b);

        let values = diff.items().collect::<Vec<u32>>();
        assert_eq!(values, &[4, 1700]);
    }

    #[test]
    fn remove() {
        let mut b = bitset![1, 42, 666];
        let values = b.items().collect::<Vec<u32>>();
        assert_eq!(values, &[1, 42, 666]);

        b.remove(42);

        let values = b.items().collect::<Vec<u32>>();
        assert_eq!(values, &[1, 666]);

        b.remove(666);
        let values = b.items().collect::<Vec<u32>>();
        assert_eq!(values, &[1]);
    }

    #[test]
    fn max() {
        let mut b = bitset![];
        assert!(b.max().is_none());
        b.insert(0);
        assert_eq!(b.max().unwrap(), 0);
        b.insert(7);
        assert_eq!(b.max().unwrap(), 7);
        b.insert(798);
        assert_eq!(b.max().unwrap(), 798);
    }
}
