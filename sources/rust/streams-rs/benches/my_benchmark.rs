#![feature(with_options)]

use criterion::{black_box, criterion_group, criterion_main, Criterion};

use streams_rs::*;

fn criterion_benchmark(c: &mut Criterion)
{
    println!("{:?}", std::env::current_dir());

    let stream = "room-1";

    c.bench_function("xadd", move |b| {
        b.iter(move || 
        {
            let mut x = Streams::new(".");
            let id = x.add(&stream, StreamId::Asterisk).unwrap();
        })
    });
}

fn create_file_write_byte_sync(c: &mut Criterion)
{
    c.bench_function("xadd", move |b| {
        b.iter(move || 
        {
            use std::io::*;
            let mut f = std::fs::File::with_options()
                .append(true)
                .create(true)
                .open(".create_file_write_byte_sync")
                .unwrap();
            f.write_all(&[0u8]).unwrap();
            f.sync_data().unwrap();
            drop(f);
        })
    });
}

// criterion_group!(benches, criterion_benchmark);
criterion_group!(benches, create_file_write_byte_sync);
criterion_main!(benches);