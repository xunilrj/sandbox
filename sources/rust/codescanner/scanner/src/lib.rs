use scan::{ScanContext, ScanError, Scanner};

fn find_scanner<'a>(scanners: &'a Vec<Box<dyn Scanner>>, path: &str) -> Option<&'a dyn Scanner> {
    for scanner in scanners.iter() {
        if scanner.can_scan(path) {
            return Some(scanner.as_ref());
        }
    }
    None
}

pub fn scan(path: &str) -> std::result::Result<ScanContext, ScanError> {
    let scanners: Vec<Box<dyn Scanner>> = vec![Box::new(scan_rust::RustScanner::new())];
    let scanner = find_scanner(&scanners, path);

    let mut ctx = ScanContext::new();

    if let Some(scanner) = scanner {
        match scanner.scan(path, &mut ctx) {
            Ok(_) => Ok(ctx),
            Err(e) => Err(e),
        }
    } else {
        Err(ScanError::NoScannerFound)
    }
}
