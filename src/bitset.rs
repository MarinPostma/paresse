use std::{
    fmt,
    ops::{BitOr, BitOrAssign},
};

type BlockRepr = u128;

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

    /// Returns the number of buckets
    pub fn blocks(&self) -> usize {
        self.blocks.len()
    }

    pub fn is_empty(&self) -> bool {
        self.blocks.is_empty() || self.blocks.iter().all(|b| *b == 0)
    }

    pub fn len(&self) -> usize {
        self.blocks.iter().map(|b| b.count_ones() as usize).sum()
    }

    /// Returns the bucket and the bit for that element
    fn pos(&self, e: u32) -> (usize, BlockRepr) {
        let block = (e / BlockRepr::BITS) as usize;
        let bit = (e % BlockRepr::BITS) as BlockRepr;
        (block, bit)
    }

    pub fn insert(&mut self, e: u32) {
        let (block, bit) = self.pos(e);

        if block >= self.blocks.len() {
            self.blocks
                .extend(std::iter::repeat(0).take(1 + block - self.blocks.len()));
        }

        self.blocks[block] |= 1 << bit;
    }

    pub fn remove(&mut self, e: u32) {
        if self.is_empty() {
            return;
        }
        let (block, bit) = self.pos(e);
        self.blocks[block] &= !(1 << bit);
    }

    pub fn iter(&self) -> Iter {
        Iter::new(self)
    }

    pub fn difference(&self, other: &Self) -> Self {
        let mut blocks: Vec<_> = self
            .blocks
            .iter()
            .zip(other.blocks.iter())
            .map(|(&s, &o)| s & !o)
            .collect();

        if self.blocks() > other.blocks() {
            blocks.extend(self.blocks[other.blocks()..].iter().copied())
        }

        Self { blocks }
    }

    /// remove trailing empty blocks
    pub fn shrink(&mut self) {
        while let Some(&0) = self.blocks.last() {
            self.blocks.pop();
        }
    }
}

pub struct Iter<'a> {
    curr_block_index: usize,
    curr_block: Option<BlockRepr>,
    set: &'a BitSet,
}

impl<'a> Iter<'a> {
    fn new(set: &'a BitSet) -> Self {
        Self {
            curr_block_index: 0,
            curr_block: None,
            set,
        }
    }
}

impl<'a> Iterator for Iter<'a> {
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        if self.set.is_empty() {
            return None;
        }

        while self.curr_block_index < self.set.blocks() {
            let curr_block = match self.curr_block {
                Some(0) => {
                    self.curr_block = None;
                    self.curr_block_index += 1;
                    continue;
                }
                Some(ref mut b) => b,
                None => {
                    self.curr_block = Some(self.set.blocks[self.curr_block_index]);
                    continue;
                }
            };

            // set all bits to 0 except for LSB
            let last_bit = (!*curr_block + 1) & *curr_block;
            // subtract one to set LSB to 0 and all bits before to 1s, and count them
            let item = (last_bit - 1).count_ones();
            // unset the LSB from the current block.
            *curr_block &= !last_bit;

            return Some(self.curr_block_index as u32 * BlockRepr::BITS + item);
        }

        None
    }
}

impl BitOrAssign<&Self> for BitSet {
    fn bitor_assign(&mut self, rhs: &Self) {
        // maybe extend to match sized
        while self.blocks() < rhs.blocks() {
            self.blocks.push(0);
        }

        self.blocks
            .iter_mut()
            .zip(rhs.blocks.iter())
            .for_each(|(s, b)| *s |= *b)
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

pub trait BitSetLike {
    fn block_count(&self) -> usize;
    fn blocks(&self) -> impl Iterator<Item = BlockRepr>;
    fn contains(&self, e: u32) -> bool;

    fn collect(&self) -> BitSet {
        BitSet {
            blocks: self.blocks().collect(),
        }
    }

    fn count(&self) -> usize {
        self.blocks().map(|b| b.count_ones() as usize).sum()
    }

    fn is_empty(&self) -> bool {
        !self.blocks().any(|b| b != 0)
    }

    fn union<O>(&self, other: O) -> Union<&Self, O>
    where
        Self: Sized,
    {
        Union::new(self, other)
    }

    fn intersection<O>(&self, other: O) -> Intersection<&Self, O> {
        Intersection::new(self, other)
    }
}

impl<T: BitSetLike> BitSetLike for &T {
    fn block_count(&self) -> usize {
        T::block_count(self)
    }

    fn blocks(&self) -> impl Iterator<Item = BlockRepr> {
        T::blocks(self)
    }

    fn contains(&self, e: u32) -> bool {
        T::contains(self, e)
    }
}

impl BitSetLike for BitSet {
    fn block_count(&self) -> usize {
        self.blocks.len()
    }

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

pub struct Intersection<A, B> {
    a: A,
    b: B,
}

impl<A, B> BitSetLike for Intersection<A, B>
where
    A: BitSetLike,
    B: BitSetLike,
{
    fn block_count(&self) -> usize {
        self.a.block_count().min(self.b.block_count())
    }

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

pub struct Union<A, B> {
    a: A,
    b: B,
}

impl<A, B> Union<A, B> {
    fn new(a: A, b: B) -> Self {
        Self { a, b }
    }
}

impl<A, B> BitSetLike for Union<A, B>
where
    A: BitSetLike,
    B: BitSetLike,
{
    fn block_count(&self) -> usize {
        self.a.block_count().max(self.b.block_count())
    }

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

        let values = bitset.iter().collect::<Vec<u32>>();
        assert_eq!(values, &[12, 17, 525, 2344]);
    }

    #[test]
    fn difference() {
        let a = bitset![1, 4, 1022, 1700];
        let b = bitset![1, 1022];

        let diff = a.difference(&b);

        let values = diff.iter().collect::<Vec<u32>>();
        assert_eq!(values, &[4, 1700]);
    }

    #[test]
    fn remove() {
        let mut b = bitset![1, 42, 666];
        let values = b.iter().collect::<Vec<u32>>();
        assert_eq!(values, &[1, 42, 666]);

        b.remove(42);

        let values = b.iter().collect::<Vec<u32>>();
        assert_eq!(values, &[1, 666]);

        b.remove(666);
        let values = b.iter().collect::<Vec<u32>>();
        assert_eq!(values, &[1]);
    }
}
