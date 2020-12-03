use crate::writers::SSTableDataWriter;
use std::boxed::Box;
use std::collections::HashMap;

pub trait SSTableIterator<'a, K: 'a + SSTableDataWriter, V: 'a + SSTableDataWriter> {
    fn iter(&'a self) -> Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a>;
    fn sorted_keys(&'a self) -> Vec<&'a K>;
    fn get(&'a self, k: &'a K) -> Option<&'a V>;
}

impl<'a, K: 'a + Ord, V: 'a> SSTableIterator<'a, K, V> for HashMap<K, V>
where
    K: SSTableDataWriter + std::hash::Hash,
    V: SSTableDataWriter,
{
    fn iter(&'a self) -> Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a> {
        Box::new(self.iter())
    }

    fn sorted_keys(&'a self) -> Vec<&'a K> {
        let mut keys: Vec<&'a K> = self.keys().collect();
        keys.sort_by(|a, b| (*a).cmp(*b));
        keys
    }

    fn get(&'a self, k: &'a K) -> Option<&'a V> {
        self.get(k)
    }
}
