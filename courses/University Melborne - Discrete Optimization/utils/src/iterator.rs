use  std::collections::BTreeSet;

#[extension_trait::extension_trait]
pub impl<'a, T: 'static + Ord, I: std::iter::Iterator<Item = &'a T>> IteratorUtils<'a, T> for I {
    fn distinct(&'a mut self) -> Vec<&'a T> {
        let mut set = BTreeSet::new();

        loop {
            if let Some(v) = self.next() {
                set.insert(v);
            } else {
                break;
            }
        }

        set.iter().map(|x| *x).collect()
    }
}