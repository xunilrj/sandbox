use syn::parse::{Parse, ParseStream};

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
