# Consistent Hashing


# Simple Hash table

```
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};
use std::vec::Vec;

struct ConsistentHashing {
    data: Vec<Option<String>>,
}

fn hash(k: &str) -> usize {
    let mut hasher = DefaultHasher::new();
    k.hash(&mut hasher);
    hasher.finish() as usize
}

impl ConsistentHashing {
    pub fn new() -> ConsistentHashing {
        ConsistentHashing {
            data: vec![None; 10],
        }
    }

    pub fn insert(&mut self, k: &str, v: &str) {
        let bucketi = hash(k) % self.data.len();
        self.data[bucketi] = Some(v.to_owned());
    }

    pub fn get(&self, k: &str) -> Option<&String> {
        let bucketi = hash(k) % self.data.len();
        self.data[bucketi].as_ref()
    }
}

fn main() {
    let mut h = ConsistentHashing::new();
    h.insert("name", "Daniel");
    println!("Value: {}", h.get("name").unwrap_or(&"<NONE>".to_owned()));
}
```

```
> cargo run
   Compiling consistent-hashing v0.1.0 (/root/github/sandbox/sources/rust/consistent-hashing)
    Finished dev [unoptimized + debuginfo] target(s) in 0.31s
     Running `target/debug/consistent-hashing`
Value: Daniel
```

# Resize

```
impl ConsistentHashing {
    ...

    pub fn resize(&mut self, new_len: usize) {
        self.data.resize(new_len, None);
    }
}
```

```
fn main() {
    let mut h = ConsistentHashing::new();
    h.insert("name", "Daniel");
    println!("Value: {}", h.get("name").unwrap());
    h.resize(20);
    println!("Value: {}", h.get("name").unwrap());
}
```

```
> cargo run
   Compiling consistent-hashing v0.1.0 (/root/github/sandbox/sources/rust/consistent-hashing)
    Finished dev [unoptimized + debuginfo] target(s) in 0.30s
     Running `target/debug/consistent-hashing`
Value: Daniel
thread 'main' panicked at 'called `Option::unwrap()` on a `None` value', src/main.rs:42:27
note: run with `RUST_BACKTRACE=1` environment variable to display a backtrace
```