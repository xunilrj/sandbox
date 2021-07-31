mod scans;
mod utils;

use std::{io::Read, path::PathBuf, str::FromStr};

use scan::{ScanContext};

pub struct RustScanner {}

pub struct RustVisitor<'a> {
    context: &'a mut ScanContext,
}

impl<'a> RustVisitor<'a> {
    fn visit_file(&mut self, file: &syn::File) {
        for item in file.items.iter() {
            self.visit_item(item);
        }
    }

    fn visit_item(&mut self, item: &syn::Item) {
        match item {
            syn::Item::Fn(function) => self.visit_fn(&function),
            _ => todo!(),
        }
    }

    fn visit_fn(&mut self, function: &syn::ItemFn) {
        self.visit_block(function.block.as_ref());
    }

    fn visit_block(&mut self, block: &syn::Block) {
        for stmt in block.stmts.iter() {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &syn::Stmt) {
        match stmt {
            syn::Stmt::Local(local) => self.visit_local(local),
            _ => {}
        }
    }

    fn visit_local(&mut self, local: &syn::Local) {
        scans::check_password_init_with_local::check(&mut self.context, local);
    }
}

impl RustScanner {
    pub fn new() -> Self {
        Self {}
    }
}

impl scan::Scanner for RustScanner {
    fn can_scan(&self, root: &str) -> bool {
        let mut cargo_toml = match PathBuf::from_str(root) {
            Ok(p) => p,
            _ => return false,
        };
        cargo_toml.push("Cargo.toml");
        cargo_toml.is_file()
    }

    fn scan(
        &self,
        root: &str,
        context: &mut ScanContext,
    ) -> std::result::Result<(), scan::ScanError> {
        use walkdir::WalkDir;
        for entry in WalkDir::new(root) {
            let file = entry.unwrap();
            let path = file.path();
            if let Some(ext) = path.extension() {
                if ext == "rs" {
                    let mut content = String::new();
                    let mut file = std::fs::File::open(path)?;
                    file.read_to_string(&mut content)?;
                    match syn::parse_file(content.as_str()) {
                        Ok(ast) => {
                            context.path = Some(path.to_path_buf());
                            let mut visitor = RustVisitor { context };
                            visitor.visit_file(&ast)
                        }
                        Err(err) => println!("{:?}", err),
                    }
                }
            }
        }

        Ok(())
    }
}
