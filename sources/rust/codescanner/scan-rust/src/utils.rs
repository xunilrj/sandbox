pub fn ident_to_string(ident: &syn::PatIdent) -> String {
    ident.ident.to_string()
}

pub fn is_literal(expr: &syn::Expr) -> bool {
    match expr {
        syn::Expr::Lit(_) => true,
        _ => false,
    }
}

#[cfg(test)]
pub fn parse_and_run<T: syn::parse::Parse>(
    code: &str,
    f: Box<dyn Fn(&mut scan::ScanContext, &T)>,
) -> scan::ScanResult {
    use std::str::FromStr;
    let ast = syn::parse_str::<T>(code).unwrap();
    let mut ctx = scan::ScanContext::new();
    ctx.path = Some(std::path::PathBuf::from_str("somefile.rs").unwrap());
    f(&mut ctx, &ast);
    ctx.result
}
