use std::{
    collections::*,
    path::{Path, PathBuf},
};

#[derive(Debug)]
pub struct DataCatalog {
    catalog: HashMap<String, PathBuf>,
}

impl DataCatalog {
    pub fn new() -> Self {
        Self {
            catalog: HashMap::new(),
        }
    }

    pub fn add<S: Into<String>, P: AsRef<Path>>(&mut self, name: S, file: P) {
        self.catalog.insert(name.into(), file.as_ref().to_owned());
    }

    pub fn get_file<'a>(&'a self, name: &str) -> &'a Path {
        self.catalog.get(name).unwrap().as_path()
    }
}
