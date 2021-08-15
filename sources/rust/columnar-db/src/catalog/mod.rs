use std::{collections::HashMap, path::PathBuf};

use std::path::Path;

use arrow::array::UInt8Array;
use async_stream::stream;

use futures_core::stream::Stream;

pub struct Catalog {
    columns: HashMap<String, PathBuf>,
}

impl Catalog {
    pub fn new() -> Self {
        Self {
            columns: HashMap::new(),
        }
    }

    pub fn push_col<S: Into<String>, P: Into<PathBuf>>(&mut self, name: S, path: P) {
        self.columns.insert(name.into(), path.into());
    }

    pub fn col_read_all<P: Into<String>>(
        &self,
        name: P,
        buffer_size: usize,
    ) -> impl Stream<Item = std::io::Result<UInt8Array>> + 'static {
        let k = name.into();
        let p = self.columns.get(&k).unwrap().clone();
        crate::io::file_read_all(p, buffer_size)
    }
}
