use std::cmp::Ordering;
use std::collections::hash_map::DefaultHasher;
use std::collections::BTreeMap;
use std::hash::{Hash, Hasher};
use std::ops::Bound::Included;
use std::ops::Bound::Unbounded;
use std::vec::Vec;

#[derive(Eq, PartialEq)]
struct ConsistentHashBucket {
    id: usize,
    data: Vec<Option<String>>,
}

impl PartialOrd for ConsistentHashBucket {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for ConsistentHashBucket {
    fn cmp(&self, other: &Self) -> Ordering {
        self.id.cmp(&other.id)
    }
}

struct ConsistentHashing {
    buckets: BTreeMap<usize, ConsistentHashBucket>,
}

fn hash(k: &str) -> usize {
    let mut hasher = DefaultHasher::new();
    k.hash(&mut hasher);
    hasher.finish() as usize
}

impl ConsistentHashing {
    pub fn new() -> ConsistentHashing {
        ConsistentHashing {
            buckets: BTreeMap::new(),
        }
    }

    pub fn insert(&mut self, k: &str, v: &str) {
        let h = hash(k);
        let first = self.buckets.iter_mut().next().as_mut();
        let mut after = self.buckets.range_mut((Included(h), Unbounded));
        let (i, bucket) = after.next().as_mut().or(first).unwrap();
        bucket.data[h % bucket.data.len()] = Some(v.to_owned());
    }

    pub fn get(&self, k: &str) -> Option<&String> {
        let h = hash(k);
        let mut after = self.buckets.range((Included(h), Unbounded));
        let (i, bucket) = after.next().or(self.buckets.iter().next()).unwrap();
        bucket.data[h % bucket.data.len()].as_ref()
    }

    pub fn push(&mut self, id: usize) {
        self.buckets.insert(
            id,
            ConsistentHashBucket {
                id,
                data: Vec::new(),
            },
        );
    }

    pub fn remove(&mut self, id: usize) {
        self.buckets.remove(&id);
    }
}

fn main() {
    let mut h = ConsistentHashing::new();
    h.push(10);
    h.insert("name", "Daniel");
    println!("Value: {}", h.get("name").unwrap());
    h.push(20);
    println!("Value: {}", h.get("name").unwrap());
    h.remove(10);
}
