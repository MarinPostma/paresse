pub enum Either<A, B> {
    A(A),
    B(B),
}

impl<A, B> Either<A, B> {
    /// Returns `true` if the either is [`A`].
    ///
    /// [`A`]: Either::A
    #[must_use]
    pub fn is_a(&self) -> bool {
        matches!(self, Self::A(..))
    }

    /// Returns `true` if the either is [`B`].
    ///
    /// [`B`]: Either::B
    #[must_use]
    pub fn is_b(&self) -> bool {
        matches!(self, Self::B(..))
    }

    pub fn as_a(&self) -> Option<&A> {
        if let Self::A(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_b(&self) -> Option<&B> {
        if let Self::B(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl<A, B, T> Iterator for Either<A, B>
where
    A: Iterator<Item = T>,
    B: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Either::A(a) => a.next(),
            Either::B(b) => b.next(),
        }
    }
}
