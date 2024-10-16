use super::BlockRepr;

pub struct BlocksIter<I> {
    it: I,
    curr_block_index: usize,
    curr_block: Option<BlockRepr>,
}

impl<I> BlocksIter<I>
    where I: Iterator<Item = BlockRepr>
{
    pub fn new(mut it: I) -> Self {
        let curr_block = it.next(); 
        Self { it, curr_block_index: 0, curr_block }
    }
}

impl<I> Iterator for BlocksIter<I>
    where I: Iterator<Item = BlockRepr>
{
    type Item = u32;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.curr_block.is_none() {
                match self.it.next() {
                    Some(b) => {
                        self.curr_block_index += 1;
                        self.curr_block = Some(b);
                    }
                    None => return None,
                }
            }

            let curr_block = self.curr_block.as_mut().unwrap();
            if *curr_block == 0 {
                self.curr_block.take();
                continue
            }

            // set all bits to 0 except for LSB
            let last_bit = (!*curr_block + 1) & *curr_block;
            // subtract one to set LSB to 0 and all bits before to 1s, and count them
            let item = (last_bit - 1).count_ones();
            // unset the LSB from the current block.
            *curr_block &= !last_bit;

            return Some(self.curr_block_index as u32 * BlockRepr::BITS + item);
        }
    }
}

