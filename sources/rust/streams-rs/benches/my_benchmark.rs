#![feature(with_options)]

use criterion::{criterion_group, criterion_main, Criterion};

use streams_rs::*;

fn xadd_always_file_sync(c: &mut Criterion)
{
    println!("{:?}", std::env::current_dir());

    let stream = "room-1";

    c.bench_function("xadd_always_file_sync", move |b| {
        b.iter(move || 
        {
            let mut x = Streams::new(".");
            x.xadd_sync = WhenSync::AlwaysFileSync;
            let _ = x.add(&stream, StreamId::Asterisk).unwrap();
        })
    });
}

fn xadd_always_system_sync(c: &mut Criterion)
{
    println!("{:?}", std::env::current_dir());

    let stream = "room-1";

    c.bench_function("xadd_always_system_sync", move |b| {
        b.iter(move || 
        {
            let mut x = Streams::new(".");
            x.xadd_sync = WhenSync::AlwaysFileSync;
            let _ = x.add(&stream, StreamId::Asterisk).unwrap();
        })
    });
}

fn xadd_never(c: &mut Criterion)
{
    println!("{:?}", std::env::current_dir());

    let stream = "room-1";

    c.bench_function("xadd_never", move |b| {
        b.iter(move || 
        {
            let mut x = Streams::new(".");
            x.xadd_sync = WhenSync::Never;
            let _ = x.add(&stream, StreamId::Asterisk).unwrap();
        })
    });
}

fn xadd_file_manager(c: &mut Criterion)
{
    let (mgr, s) = filemgr::FileMgr::new();
    let mut mgr = mgr.start_new_thread();

    c.bench_function("xadd_file_manager", 
    {
        let mut x = Streams::new("./data/");
        x.xadd_sync = WhenSync::FileManager(s.clone(), None);
        move |b| {b.iter(|| 
        {
            let stream = "room-1";
            let _ = x.add(&stream, StreamId::Asterisk).unwrap();
        })
    }});

    mgr.kill().wait(std::time::Duration::from_secs(1));
}

// fn create_file_write_byte_sync(c: &mut Criterion)
// {
//     c.bench_function("xadd", move |b| {
//         b.iter(move || 
//         {
//             use std::io::*;
//             let mut f = std::fs::File::with_options()
//                 .append(true)
//                 .create(true)
//                 .open(".create_file_write_byte_sync")
//                 .unwrap();
//             f.write_all(&[0u8]).unwrap();
//             f.sync_data().unwrap();
//             drop(f);
//         })
//     });
// }

// criterion_group!(benches, criterion_benchmark);
criterion_group!(benches, xadd_file_manager);
criterion_main!(benches);