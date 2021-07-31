use scan::ScanContext;
use syn::spanned::Spanned;

pub fn check(ctx: &mut ScanContext, local: &syn::Local) {
    match &local.pat {
        syn::Pat::Ident(ident) => {
            let ident = crate::utils::ident_to_string(ident);
            if ident.contains("password") {
                if let Some((_, init)) = &local.init {
                    if crate::utils::is_literal(init.as_ref()) {
                        ctx.result.push_security(
                            ctx.path.as_ref().unwrap().as_path(),
                            local.span().clone(),
                            "Password initialized with constant directly in code.",
                        );
                    }
                }
            }
        }
        _ => {}
    }
}

#[cfg(test)]
fn check_stmt(ctx: &mut scan::ScanContext, stmt: &syn::Stmt) {
    match &stmt {
        syn::Stmt::Local(local) => check(ctx, local),
        _ => todo!(),
    }
}

#[test]
pub fn must_find_passwork_init_with_constant() {
    let result = crate::utils::parse_and_run::<syn::Stmt>(
        r#"let password = "SUPERSECRET";"#,
        Box::new(check_stmt),
    );
    assert_eq!(result.findings.len(), 1);
}

#[test]
pub fn must_accept_passwork_init_with_fn_call() {
    let result = crate::utils::parse_and_run::<syn::Stmt>(
        r#"let password = some_fn();"#,
        Box::new(check_stmt),
    );
    assert_eq!(result.findings.len(), 0);
}

#[test]
pub fn must_accept_variable_not_named_password() {
    let result =
        crate::utils::parse_and_run::<syn::Stmt>(r#"let hey = "a";"#, Box::new(check_stmt));
    assert_eq!(result.findings.len(), 0);
}
