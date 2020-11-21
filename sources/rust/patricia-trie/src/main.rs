#![feature(box_syntax)]
#![feature(const_in_array_repeat_expressions)]

#[derive(Debug, Clone)]
enum Value {
    None,
    I32(i32),
    I64(i64),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    String(String),
}

#[derive(Debug)]
struct ObservableValue {
    inode: usize,
    value: Value,
}

struct Node {
    children: [Option<Box<Node>>; 26],
    labels: [Option<String>; 26],
    value: ObservableValue,
}
impl Node {
    pub fn new() -> Self {
        Self {
            children: [None; 26],
            labels: [None; 26],
            value: ObservableValue {
                inode: 0,
                value: Value::None,
            },
        }
    }

    pub fn with_value(value: Value) -> Self {
        Self {
            children: [None; 26],
            labels: [None; 26],
            value: ObservableValue { inode: 0, value },
        }
    }

    pub fn split_at(&mut self, idx: usize, at: usize) -> &mut Node {
        println!("Before: {:?}", self);

        let (a, b) = match &self.labels[idx] {
            Some(l) => l.split_at(at),
            None => panic!(),
        };
        let (a, b) = (a.to_string(), b.to_string());

        self.labels[idx] = Some(a);
        let mut oldleaf = self.children[idx].as_mut().unwrap();

        let first_letter = b.chars().nth(0).unwrap();
        let mut first_letter = first_letter.to_lowercase();
        let first_letter = first_letter.nth(0).unwrap();
        let bidx = first_letter as usize - 'a' as usize;

        let mut bnode = Self {
            children: [None; 26],
            labels: [None; 26],
            value: ObservableValue {
                inode: 0,
                value: oldleaf.value.value.clone(),
            },
        };
        oldleaf.value.value = Value::None;

        oldleaf.labels[bidx] = Some(b);
        oldleaf.children[bidx] = Some(box bnode);

        println!("After: {:?}", &self);
        self.children[idx].as_mut().unwrap()
    }
}

fn first_diferrent(a: &str, b: &str) -> Option<usize> {
    let achars = a.chars();
    let bchars = b.chars();

    for (i, (a, b)) in achars.zip(bchars).enumerate() {
        if a != b {
            return Some(i);
        }
    }

    let alen = a.len();
    let blen = b.len();
    if alen != blen {
        return Some(alen.min(blen));
    }

    return None;
}
impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = rand::random::<f32>();
        let color = (color * 10.0) as u8 + 31;
        f.write_str(&format!("\x1b[{}m[", color));

        f.write_str("Value:");
        f.write_str(&format!("{:?}, ", self.value));
        for (l, c) in self.labels.iter().zip(self.children.iter()) {
            match (l, c) {
                (Some(l), Some(c)) => {
                    let node = c.as_ref();
                    f.write_str(&format!("{:?} -> {:?}; ", l, node));
                }
                _ => {}
            }
        }

        f.write_str(&format!("\x1b[{}m]\x1b[0m", color));
        Ok(())
    }
}

#[derive(Debug)]
struct PatriciaTrie {
    root: Node,
}
impl PatriciaTrie {
    pub fn new() -> Self {
        Self { root: Node::new() }
    }

    pub fn insert(&mut self, key: &str, value: Value) {
        let start = 0;
        let compare_to = unsafe { key.get_unchecked(start..) };
        let first_letter = compare_to.chars().nth(0).unwrap();
        let mut first_letter = first_letter.to_lowercase();
        let first_letter = first_letter.nth(0).unwrap();
        let idx = first_letter as usize - 'a' as usize;

        match &self.root.labels[idx] {
            Some(l) => match first_diferrent(compare_to, l.as_str()) {
                Some(at) => {
                    let oldleaf = self.root.split_at(idx, at);

                    let (a, b) = key.split_at(at);
                    let first_letter = b.chars().nth(0).unwrap();
                    let mut first_letter = first_letter.to_lowercase();
                    let first_letter = first_letter.nth(0).unwrap();
                    let bidx = first_letter as usize - 'a' as usize;
                    oldleaf.labels[bidx] = Some(b.to_string());
                    oldleaf.children[bidx] = Some(box Node::with_value(value));
                }
                None => {}
            },
            _ => {
                let node = Node::with_value(value);
                self.root.children[idx] = Some(box node);
                self.root.labels[idx] = Some(key.to_string());
            }
        };
    }

    fn get_with_node<'a>(&'a self, key: &str, start: usize, node: Option<&'a Node>) -> Value {
        match node {
            Some(node) => {
                let compare_to = unsafe { key.get_unchecked(start..) };

                let first_letter = compare_to.chars().nth(0).unwrap();
                let mut first_letter = first_letter.to_lowercase();
                let first_letter = first_letter.nth(0).unwrap();
                let idx = first_letter as usize - 'a' as usize;

                match &node.labels[idx] {
                    Some(l) => match first_diferrent(compare_to, l.as_str()) {
                        Some(diff) => {
                            let a = node
                                .children
                                .get(idx)
                                .and_then(|x| x.as_ref())
                                .map(|x| x.as_ref());
                            self.get_with_node(key, start + diff, a)
                        }
                        None => node
                            .children
                            .get(idx)
                            .and_then(|x| x.as_ref())
                            .map(|x| &x.value.value)
                            .unwrap()
                            .clone(),
                    },
                    _ => Value::None,
                }
            }
            _ => Value::None,
        }
    }

    pub fn get(&self, key: &str) -> Value {
        let first_letter = key.chars().nth(0).unwrap();
        let mut first_letter = first_letter.to_lowercase();
        let first_letter = first_letter.nth(0).unwrap();
        let idx = first_letter as usize - 'a' as usize;

        match &self.root.labels[idx] {
            Some(l) => self.get_with_node(key, 0, Some(&self.root)),
            _ => Value::None,
        }
    }
}

fn main() {

    // winapi::um::libloaderapi::LoadLibraryA
}

#[test]
fn name() {
    let mut tree = PatriciaTrie::new();
    tree.insert("Daniel", Value::I32(1));
    tree.insert("Dante", Value::I32(2));

    println!("Tree: {:?}", tree);

    println!("{:?}", tree.get("Daniel"));
    println!("{:?}", tree.get("Dante"));
    println!("{:?}", tree.get("Danty"));

    //tree.update("Daniel", Value::I32(3));
}
