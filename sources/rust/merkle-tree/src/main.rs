use merkle::MerkleTreeBuilder;

mod merkle;
mod utils;

#[tokio::main]
async fn main() {
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());

    let tree1 = b.build();
    let tree2 = tree1.clone();

    println!("{:?}", tree1.compare(&tree2));

    // Different number of nodes
    let mut b = MerkleTreeBuilder::new();
    b.push(vec![0].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());

    let tree3 = b.build();
    println!("{:?}", tree1.compare(&tree3));

    let mut b = MerkleTreeBuilder::new();
    b.push(vec![4].as_slice());
    b.push(vec![1].as_slice());
    b.push(vec![2].as_slice());
    b.push(vec![3].as_slice());

    let tree4 = b.build();
    println!("{:?}", tree1.compare(&tree4));
}
