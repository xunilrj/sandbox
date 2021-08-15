#![feature(async_stream)]
#![allow(dead_code)]

mod catalog;
mod channels;
mod db;
mod folds;
mod io;

use crate::{
    catalog::Catalog,
    db::computegrafdef::{
        ComputeGraphDefinition, ComputeTaskDefinitionFolds, ComputeTaskDefinitionSources,
    },
    io::tempfile::TempFile,
};
use std::time::Duration;

#[tokio::main]
async fn main() {
    let now = std::time::Instant::now();
    println!("Generating file...");
    let _100_mb = 100 * 1024 * 1024;
    let f = TempFile::random(_100_mb).expect("Cannot create tempfile");
    println!(
        "Done @ {:?} took {:?}",
        f.path,
        std::time::Instant::now() - now
    );

    let mut catalog = Catalog::new();
    catalog.push_col("Table1.Col1", f.path.clone());

    let mut def = ComputeGraphDefinition::new();
    let a = def.source(ComputeTaskDefinitionSources::ReadColumn {
        column: "Table1.Col1".into(),
    });
    let b = def.fold(ComputeTaskDefinitionFolds::Maximum);
    def.link(a, b);
    let mut c = def.build(&catalog);
    c.start();
    c.wait(Duration::from_millis(1000));

    let now = std::time::Instant::now();
    let buffers = catalog.col_read_all("Table1.Col1", 16 * 1024);
    let m = folds::max(buffers).await;
    println!("{:?} took {:?}", m, std::time::Instant::now() - now);
}
