#![feature(box_syntax)]
#![feature(const_in_array_repeat_expressions)]

#[derive(Debug)]
struct Node<T> {
    children: [Option<Box<Node<T>>>; 26],
    labels: [Option<String>; 26],
    value: Option<T>,
}
impl<T> Node<T> {
    pub fn new() -> Self {
        Self {
            children: [None; 26],
            labels: [None; 26],
            value: None,
        }
    }

    pub fn with_value(value: T) -> Self {
        Self {
            children: [None; 26],
            labels: [None; 26],
            value: Some(value),
        }
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

    return None;
}

#[derive(Debug)]
struct PatriciaTrie<T> {
    root: Node<T>,
}
impl<T> PatriciaTrie<T> {
    pub fn new() -> Self {
        Self { root: Node::new() }
    }

    pub fn insert(&mut self, key: &str, value: T) {
        let first_letter = key.chars().nth(0).unwrap();
        let mut first_letter = first_letter.to_lowercase();
        let first_letter = first_letter.nth(0).unwrap();
        let idx = first_letter as usize - 'a' as usize;

        let node = Node::with_value(value);
        self.root.children[idx] = Some(box node);
        self.root.labels[idx] = Some(key.to_string());
    }

    fn get_with_node<'a>(
        &'a self,
        key: &str,
        start: usize,
        node: Option<&'a Node<T>>,
    ) -> Option<&T> {
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
                        None => node.value.as_ref(),
                    },
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn get(&self, key: &str) -> Option<&T> {
        let first_letter = key.chars().nth(0).unwrap();
        let mut first_letter = first_letter.to_lowercase();
        let first_letter = first_letter.nth(0).unwrap();
        let idx = first_letter as usize - 'a' as usize;

        match &self.root.labels[idx] {
            Some(l) => self.get_with_node(key, 0, Some(&self.root)),
            _ => None,
        }
    }
}

fn main() {

    // winapi::um::libloaderapi::LoadLibraryA
}

#[test]
fn name() {
    let mut tree = PatriciaTrie::new();
    tree.insert("Daniel", 1);
    let r = tree.get("Daniel");

    println!("{:?}", r);
}
