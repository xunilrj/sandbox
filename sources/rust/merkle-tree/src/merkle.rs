use sha2::{digest::generic_array::typenum::private::PrivateDivIntOut, Digest};

pub struct MerkleTreeBuilder {
    nodes: Vec<MerkleNode>,
}

impl MerkleTreeBuilder {
    pub fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn push(&mut self, data: &[u8]) {
        let mut d = sha2::Sha256::new();
        d.update(data);
        let hash = d.finalize();
        let hash: Vec<u8> = hash.as_slice().into();
        let node = MerkleNode { hash };
        self.nodes.push(node);
    }

    pub fn build(self) -> MerkleTree {
        let MerkleTreeBuilder { mut nodes } = self;

        let mut idx = 0;
        let qty_leafs = nodes.len();
        let complete =
            2 * (if qty_leafs % 2 == 0 {
                qty_leafs
            } else {
                qty_leafs + 1
            }) - 2;
        loop {
            if idx >= complete {
                break;
            }

            match (nodes.get(idx), nodes.get(idx + 1)) {
                (Some(l), Some(r)) => {
                    let mut d = sha2::Sha256::new();
                    d.update(l.hash.as_slice());
                    d.update(r.hash.as_slice());
                    let hash = d.finalize();
                    let hash: Vec<u8> = hash.as_slice().into();
                    let node = MerkleNode { hash };
                    nodes.push(node);
                    idx += 2;
                }
                (Some(l), None) => {
                    let mut d = sha2::Sha256::new();
                    d.update(l.hash.as_slice());
                    d.update(l.hash.as_slice());
                    let hash = d.finalize();
                    let hash: Vec<u8> = hash.as_slice().into();
                    let node = MerkleNode { hash };
                    nodes.push(node);

                    idx += 1;
                }
                _ => break,
            }
        }

        nodes.reverse();
        MerkleTree { qty_leafs, nodes }
    }
}

#[derive(Clone)]
pub struct MerkleTree {
    qty_leafs: usize,
    nodes: Vec<MerkleNode>,
}

#[derive(Debug)]
pub enum MerkleTreeComparison {
    DifferenceNumberOfNodes,
}

impl MerkleTree {
    pub fn compare(&self, other: &Self) -> std::result::Result<Vec<usize>, MerkleTreeComparison> {
        if self.qty_leafs != other.qty_leafs {
            Err(MerkleTreeComparison::DifferenceNumberOfNodes)
        } else {
            let mut differences = Vec::new();
            let mut q = vec![0];
            let size = self.nodes.len();
            loop {
                if let Some(idx) = q.pop() {
                    let self_node = &self.nodes[idx];
                    let other_node = &other.nodes[idx];

                    if self_node.hash == other_node.hash {
                        continue;
                    } else {
                        if Self::is_leaf(size, idx) {
                            differences.push(size - idx - 1);
                        } else {
                            Self::get_left_children_idx(size, idx).map(|x| q.push(x));
                            Self::get_right_children_idx(size, idx).map(|x| q.push(x));
                        }
                    }
                } else {
                    break;
                }
            }
            Ok(differences)
        }
    }

    #[inline(always)]
    pub fn is_leaf(size: usize, idx: usize) -> bool {
        (2 * idx + 1) >= size
    }

    #[inline(always)]
    pub fn get_left_children_idx(size: usize, idx: usize) -> Option<usize> {
        if Self::is_leaf(size, idx) {
            None
        } else {
            Some(2 * idx + 1)
        }
    }

    #[inline(always)]
    pub fn get_right_children_idx(size: usize, idx: usize) -> Option<usize> {
        if Self::is_leaf(size, idx) {
            None
        } else {
            Some(2 * idx + 2)
        }
    }
}

#[derive(Debug, Clone)]
struct MerkleNode {
    hash: Vec<u8>,
}

#[test]
pub fn merkle_tree_test_children_indices() {
    assert!(matches!(MerkleTree::get_left_children_idx(7, 0), Some(1)));
    assert!(matches!(MerkleTree::get_right_children_idx(7, 0), Some(2)));

    assert!(matches!(MerkleTree::get_left_children_idx(7, 1), Some(3)));
    assert!(matches!(MerkleTree::get_right_children_idx(7, 1), Some(4)));

    assert!(matches!(MerkleTree::get_left_children_idx(7, 2), Some(5)));
    assert!(matches!(MerkleTree::get_right_children_idx(7, 2), Some(6)));

    assert!(matches!(MerkleTree::get_left_children_idx(7, 6), None));
    assert!(matches!(MerkleTree::get_right_children_idx(7, 6), None));
}

#[test]
pub fn merkle_tree_comparison_equal() {
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());

    let tree1 = b.build();
    let tree2 = tree1.clone();

    assert!(matches!(tree1.compare(&tree2), Ok(differences) if differences.len() == 0));
}

#[test]
pub fn merkle_tree_comparison_different_number_of_nodes() {
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());
    let tree1 = b.build();

    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    let tree3 = b.build();

    assert!(matches!(
        tree1.compare(&tree3),
        Err(DifferenceNumberOfNodes)
    ));
}

#[test]
pub fn merkle_tree_comparison_find_different_node() {
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());
    let tree1 = b.build();

    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![4].as_slice());
    b.push(vec![3].as_slice());
    let tree4 = b.build();

    assert!(
        matches!(tree1.compare(&tree4), Ok(differences) if differences.len() == 1 && differences == vec![2])
    );
}

#[test]
pub fn merkle_tree_comparison_all_nodes_are_different() {
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());
    let tree1 = b.build();

    let mut b = MerkleTreeBuilder::new();
    b.push(vec![5].as_slice());
    b.push(vec![4].as_slice());
    b.push(vec![3].as_slice());
    b.push(vec![2].as_slice());
    let tree4 = b.build();

    assert!(
        matches!(tree1.compare(&tree4), Ok(differences) if differences.len() == 4 && differences == vec![0, 1, 2,3])
    );
}
