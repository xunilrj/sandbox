#![feature(unsized_tuple_coercion)]
#![feature(llvm_asm)]
#[macro_use]
extern crate derive_builder;

mod platform;
mod runtime;

use platform::{
    actors::{accept, open, EpollReactor},
    EpollSocket,
};
use runtime::Runtime;

async fn handle(fd: EpollSocket) {
    let mut fd = fd;
    loop {
        match fd.read(1024).await {
            Ok((buff, size)) => {
                let _ = std::str::from_utf8(&buff[0..size]).unwrap();
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
                break;
            }
            _ => break,
        }
    }
}

fn main() {
    println!("PID: {}", std::process::id());

    let mut epoll = EpollReactor::new();
    let r = epoll.spawn();

    let j = std::thread::spawn(move || loop {
        let _ = r.recv().unwrap();
    });

    let fd = open(8888).unwrap();
    let fd = epoll.0.add_to(fd);

    let rt = Runtime::new();
    let handle_socket = rt.spawner(&handle);
    rt.spawn(accept(fd, handle_socket));

    j.join().unwrap();
}
