use std::collections::*;

pub struct DataCatalog {
    catalog: HashMap<String, String>,
}

impl DataCatalog {
    pub fn new() -> Self {
        Self {
            catalog: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: &str, file: &str) {
        self.catalog.insert(name.to_string(), file.to_string());
    }

    pub fn get_file(&self, name: &str) -> &str {
        self.catalog.get(name).unwrap()
    }
}
