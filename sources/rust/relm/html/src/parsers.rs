use syn::parse::{Parse, ParseBuffer, ParseStream};

pub fn parse_seq1<T1: Parse>(stream: &mut ParseStream) -> syn::Result<T1> {
    let fork = stream.fork();
    match T1::parse(&fork) {
        Ok(a) => {
            let _ = T1::parse(&stream);
            Ok(a)
        }
        Err(e) => Err(e),
    }
}

pub fn parse_seq2<T1: Parse, T2: Parse>(stream: &mut ParseStream) -> syn::Result<(T1, T2)> {
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

pub fn parse_seq3<T1: Parse, T2: Parse, T3: Parse>(
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

pub fn parse_seq4<T1: Parse, T2: Parse, T3: Parse, T4: Parse>(
    stream: &mut ParseStream,
) -> syn::Result<(T1, T2, T3, T4)> {
    let fork = stream.fork();
    match (
        T1::parse(&fork),
        T2::parse(&fork),
        T3::parse(&fork),
        T4::parse(&fork),
    ) {
        (Ok(a), Ok(b), Ok(c), Ok(d)) => {
            let _ = T1::parse(&stream);
            let _ = T2::parse(&stream);
            let _ = T3::parse(&stream);
            let _ = T4::parse(&stream);
            Ok((a, b, c, d))
        }
        (Err(e), _, _, _) => Err(e),
        (_, Err(e), _, _) => Err(e),
        (_, _, Err(e), _) => Err(e),
        (_, _, _, Err(e)) => Err(e),
    }
}

pub fn parse_seq5<T1: Parse, T2: Parse, T3: Parse, T4: Parse, T5: Parse>(
    stream: &mut ParseStream,
) -> syn::Result<(T1, T2, T3, T4, T5)> {
    let fork = stream.fork();
    match (
        T1::parse(&fork),
        T2::parse(&fork),
        T3::parse(&fork),
        T4::parse(&fork),
        T5::parse(&fork),
    ) {
        (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e)) => {
            let _ = T1::parse(&stream);
            let _ = T2::parse(&stream);
            let _ = T3::parse(&stream);
            let _ = T4::parse(&stream);
            let _ = T5::parse(&stream);
            Ok((a, b, c, d, e))
        }
        (Err(e), _, _, _, _) => Err(e),
        (_, Err(e), _, _, _) => Err(e),
        (_, _, Err(e), _, _) => Err(e),
        (_, _, _, Err(e), _) => Err(e),
        (_, _, _, _, Err(e)) => Err(e),
    }
}

pub fn parse_seq6<T1: Parse, T2: Parse, T3: Parse, T4: Parse, T5: Parse, T6: Parse>(
    stream: &mut ParseStream,
) -> syn::Result<(T1, T2, T3, T4, T5, T6)> {
    let fork = stream.fork();
    match (
        T1::parse(&fork),
        T2::parse(&fork),
        T3::parse(&fork),
        T4::parse(&fork),
        T5::parse(&fork),
        T6::parse(&fork),
    ) {
        (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f)) => {
            let _ = T1::parse(&stream);
            let _ = T2::parse(&stream);
            let _ = T3::parse(&stream);
            let _ = T4::parse(&stream);
            let _ = T5::parse(&stream);
            let _ = T6::parse(&stream);
            Ok((a, b, c, d, e, f))
        }
        (Err(e), _, _, _, _, _) => Err(e),
        (_, Err(e), _, _, _, _) => Err(e),
        (_, _, Err(e), _, _, _) => Err(e),
        (_, _, _, Err(e), _, _) => Err(e),
        (_, _, _, _, Err(e), _) => Err(e),
        (_, _, _, _, _, Err(e)) => Err(e),
    }
}

pub struct OpenBrace {}

impl syn::parse::Parse for OpenBrace {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Brace) {
            input.parse()
        } else {
            Err(syn::Error::new(input.span(), "Expected [{]"))
        }
    }
}

pub struct CloseBrace {}

impl syn::parse::Parse for CloseBrace {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        if input.peek(syn::token::Brace) {
            input.parse()
        } else {
            Err(syn::Error::new(input.span(), "Expected [{]"))
        }
    }
}

pub fn braced_map<'a, T, F>(stream: &mut ParseStream, f: F) -> syn::Result<T>
where
    F: Fn(&mut ParseStream) -> T + 'a,
{
    let content;
    syn::braced!(content in stream);
    let mut stream: ParseStream = &content;
    Ok(f(&mut stream))
}

pub fn braced<T: Parse>(stream: &mut ParseStream) -> syn::Result<T> {
    let content;
    syn::braced!(content in stream);
    content.parse::<T>()
}
