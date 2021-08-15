use std::marker::PhantomData;

pub struct ConsecutivesIterator<'a, T, I: Sized + Iterator<Item = T>>(
    I,
    std::marker::PhantomData<&'a ()>,
);

pub trait ConsecutiveIteratorHelper<'a, T, I: Iterator<Item = T>> {
    fn consecutives(self) -> ConsecutivesIterator<'a, T, I>;
}

impl<'a, T, I: Iterator<Item = T>> ConsecutiveIteratorHelper<'a, T, I> for I {
    fn consecutives(self) -> ConsecutivesIterator<'a, T, I> {
        ConsecutivesIterator(self, PhantomData)
    }
}

impl<'a, T: 'a, I: Sized + Iterator<Item = T>> Iterator for ConsecutivesIterator<'a, T, I> {
    type Item = (T, Option<T>);

    fn next(&mut self) -> Option<Self::Item> {
        match (self.0.next(), self.0.next()) {
            (None, _) => None,
            (Some(l), r) => Some((l, r)),
        }
    }
}
