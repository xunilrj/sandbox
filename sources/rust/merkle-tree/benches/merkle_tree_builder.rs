use criterion::{criterion_group, criterion_main, Criterion};
use merkle_tree::merkle::*;

async fn build_tree() {
    let mut b = MerkleTreeBuilder::new();
    b.push("Sed ut perspiciatis, unde omnis iste natus error sit voluptatem accusantium doloremque laudantium".as_bytes());
    b.push("totam rem aperiam eaque ipsa, quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt, explicabo.".as_bytes());
    b.push("Nemo enim ipsam voluptatem, quia voluptas sit, aspernatur aut odit aut fugit, sed quia consequuntur magni dolores eos, qui ratione voluptatem sequi nesciunt, neque porro quisquam est, qui dolorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum[d] exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur?".as_bytes());
    b.push("Temporibus autem quibusdam et aut officiis debitis aut rerum necessitatibus saepe eveniet, ut et voluptates repudiandae sint et molestiae non recusandae. Itaque earum rerum hic tenetur a sapiente delectus, ut aut reiciendis voluptatibus maiores alias consequatur aut perferendis doloribus asperiores repellat.".as_bytes());
    let _ = b.build();
}

fn merkle_parallel_benches(c: &mut Criterion) {
    let mut group = c.benchmark_group("merkle");
    group.bench_function("build_tree", |b| b.iter(|| build_tree()));
}

criterion_group!(benches, merkle_parallel_benches);
criterion_main!(benches);
