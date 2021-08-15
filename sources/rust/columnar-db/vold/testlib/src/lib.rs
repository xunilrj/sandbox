extern crate proc_macro;
extern crate quote;
use proc_macro::TokenStream;
use quote::*;
use std::string::ToString;
use syn::{parse::*, token::*, *};

fn handle_binary(node: ExprBinary, was_typed: &str) -> TokenStream {
    let left = node.left;
    let right = node.right;
    match node.op {
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
    }
}

fn handle_method_call(node: ExprMethodCall, was_typed: &str) -> TokenStream {
    let q = quote! {
        let value = #node;
        let v: bool = value.into();
        if !v {
            let fail = format!("FAILED: {}. Value: {:?}", #was_typed, value);
            panic!(fail)
        }
    };
    q.into()
}

fn handle_pat_guard(id: Ident, pat: Pat, guard: Option<Expr>, was_typed: &str) -> TokenStream {
    let guard = match guard {
        Some(g) => quote! {if #g},
        _ => quote! {},
    };
    let q = quote! {
        match #id {
            #pat #guard => {}
            _ => {
                let fail = format!("Pattern FAILED: {}. Value: {:?}", #was_typed, #id);
                panic!(fail);
            }
        }
    };
    q.into()
}

enum Acceptable {
    BinaryExpr(ExprBinary),
    MethodCallExpr(ExprMethodCall),
    PatternGuard(Ident, Pat, Option<Expr>),
}

fn parse_seq1<T1: Parse>(stream: &mut ParseStream) -> syn::Result<T1> {
    let fork = stream.fork();
    match T1::parse(&fork) {
        Ok(a) => {
            let _ = T1::parse(&stream);
            Ok(a)
        }
        Err(e) => Err(e),
    }
}

fn parse_seq2<T1: Parse, T2: Parse>(stream: &mut ParseStream) -> syn::Result<(T1, T2)> {
    let fork = stream.fork();
    match (T1::parse(&fork), T2::parse(&fork)) {
        (Ok(a), Ok(b)) => {
            let _ = T1::parse(&stream);
            let _ = T2::parse(&stream);
            Ok((a, b))
        }
        (Err(e), _) => Err(e),
        (_, Err(e)) => Err(e),
    }
}

fn parse_seq3<T1: Parse, T2: Parse, T3: Parse>(
    stream: &mut ParseStream,
) -> syn::Result<(T1, T2, T3)> {
    let fork = stream.fork();
    match (T1::parse(&fork), T2::parse(&fork), T3::parse(&fork)) {
        (Ok(a), Ok(b), Ok(c)) => {
            let _ = T1::parse(&stream);
            let _ = T2::parse(&stream);
            let _ = T3::parse(&stream);
            Ok((a, b, c))
        }
        (Err(e), _, _) => Err(e),
        (_, Err(e), _) => Err(e),
        (_, _, Err(e)) => Err(e),
    }
}

impl Parse for Acceptable {
    fn parse(mut stream: ParseStream) -> Result<Self> {
        // Try parse
        // assert!(<ID>, <PATTERN> [<if> <guard>]?)

        let r = parse_seq3::<Ident, Comma, Pat>(&mut stream)
            .map(|(a, b, c)| (a, b, c, parse_seq2::<If, Expr>(&mut stream)));
        match r {
            Ok((id, _, pat, Ok((_, guard)))) => {
                return Ok(Acceptable::PatternGuard(id, pat, Some(guard)))
            }
            Ok((id, _, pat, _)) => return Ok(Acceptable::PatternGuard(id, pat, None)),
            _ => {}
        }

        match parse_seq1::<ExprBinary>(&mut stream) {
            Ok(node) => return Ok(Acceptable::BinaryExpr(node)),
            _ => {}
        }

        match parse_seq1::<ExprMethodCall>(&mut stream) {
            Ok(node) => return Ok(Acceptable::MethodCallExpr(node)),
            _ => {}
        }

        Err(Error::new(stream.span(), "Invalid Expression"))
    }
}

#[proc_macro]
pub fn assert(input: TokenStream) -> TokenStream {
    let was_typed = input.to_string();
    let parsed = parse_macro_input!(input as Acceptable);
    match parsed {
        Acceptable::BinaryExpr(node) => handle_binary(node, &was_typed),
        Acceptable::PatternGuard(id, pat, guard) => handle_pat_guard(id, pat, guard, &was_typed),
        Acceptable::MethodCallExpr(node) => handle_method_call(node, &was_typed),
    }
}
