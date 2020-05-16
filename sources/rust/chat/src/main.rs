use std::{error::Error, boxed::Box, net::SocketAddr, collections::HashMap};
use mio::{Poll, Events, Token, Interest};
use mio::net::{TcpListener, TcpStream};

mod http_context;
use http_context::{HttpContext};

fn accept(i: usize, poll: &Poll, server: &TcpListener) -> Result<(TcpStream, SocketAddr, Token), Box<dyn Error>>
{
    let (mut conn,addr) = server.accept()?;
    let token = Token(i);

    poll.registry()
        .register(&mut conn, token, Interest::READABLE | Interest::WRITABLE)?;

    Ok((conn, addr, token))
}

trait OptionEqual<T> where T : PartialEq
{
    fn is_equal(&self, v: &T) -> bool;
}

impl<T: std::cmp::PartialEq> OptionEqual<T> for Option<T>
{
    fn is_equal(&self, v: &T) -> bool
    {
        self.as_ref().map_or(false, |x| x.eq(v))
    }
}

fn main() -> Result<(), Box<dyn Error>>
{
    let mut poll = Poll::new()?;
    
    let mut events = Events::with_capacity(128);

    let addr = "127.0.0.1:8080".parse()?;
    let mut server = TcpListener::bind(addr)?;
    const SERVERTOKEN: Token = Token(0);
    poll.registry()
        .register(&mut server, SERVERTOKEN, Interest::READABLE)?;

    let mut ctxs = HashMap::new();
    let mut tokeni = 0;

    loop
    {
        poll.poll(&mut events, None)?;

        for event in events.iter() {
            let et = event.token();
            if et == SERVERTOKEN {
                tokeni += 1;
                match accept(tokeni, &poll, &server) {
                    Ok((conn, _, token)) => { ctxs.insert(token.0, HttpContext::new(conn)); },
                    Err(_) => {}
                }
                continue;
            }
            
            match ctxs.get_mut(&et.0) {
                Some(ctx) => {
                    if ctx.read().unwrap_or(false) {
                        let r = ctx.write("hi");
                    }
                    // ctxs.remove(&et.0);
                },
                None => println!("Unkown token")
            }
        }
    }
}