#![feature(unsized_tuple_coercion)]
#![feature(llvm_asm)]
#![feature(never_type)]
#![feature(backtrace)]
#[macro_use]
extern crate derive_builder;

mod platform;
mod runtime;

use log::{info, trace};
use platform::{
    acceptor, actors::EpollReactor, dummy_receiver, EpollSocket, Protocol, SinFamily, SockType,
    Socket,
};
use runtime::Runtime;

async fn handle<'a>(mut fd: EpollSocket<'a>) {
    trace!(target: "Handler", "Reading {:?}", fd);
    match fd.read(1024).await {
        Ok((buff, size)) => {
            let _ = std::str::from_utf8(&buff[0..size]).unwrap();
            trace!(target: "Handler", "Writing {:?}", fd);
            let _ = fd
                .write(
                    "HTTP/1.1 200 OK

<html>
    <head>
    <title>An Example Page</title>
    </head>
    <body>
    <p>Hello World, this is a very simple HTML document.</p>
    </body>
</html>"
                        .as_bytes(),
                )
                .await
                .unwrap();
            trace!(target: "Handler", "Done");
        }
        _ => {}
    }
}

pub static mut REC: Option<EpollReactor> = None;

fn main() -> Result<(), String> {
    pretty_env_logger::init();
    info!(target: "app", "PID: {}", std::process::id());

    println!("1");
    unsafe { REC = Some(EpollReactor::new()?) };
    let reactor = unsafe { REC.as_mut().unwrap() };
    let r = reactor.spawn();

    let j = dummy_receiver(r);

    let socket = Socket::new(
        SinFamily::Inet,
        SockType::StreamNonBlock,
        Protocol::TCP,
        8888,
    )?;
    println!("2");
    let socket = socket.attach(&reactor.epoll);

    let mut rt = Runtime::new();
    rt.increase_threads(10).unwrap();
    let handle_socket = rt.spawner(&handle);

    rt.spawn(acceptor(socket, handle_socket));

    j.join().unwrap();
}
