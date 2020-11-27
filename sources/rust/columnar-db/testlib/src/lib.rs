extern crate proc_macro;
extern crate quote;
use proc_macro::TokenStream;
use quote::*;
use syn::{parse_macro_input, BinOp, Expr, ExprBinary};

#[proc_macro]
pub fn assert(input: TokenStream) -> TokenStream {
    let was_typed = format!("input clone: {}", &input);
    let input = parse_macro_input!(input as Expr);
    match input {
        Expr::Binary(ExprBinary {
            left, op, right, ..
        }) => match op {
            BinOp::Eq(_) => {
                let q = quote! {
                    let l = #left;
                    let r = #right;
                    if !(l == r) {
                        let fail = format!("{}, Values: left: [{}], Right: [{}]", #was_typed, l, r);
                        panic!(fail)
                    }
                };
                q.into()
            }
            _ => "!".parse().unwrap(),
        },
        _ => "!".parse().unwrap(),
    }
}
