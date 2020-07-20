use super::{
    epoll_create1, EpollCreateFlag, EpollSocket, Errno, Protocol, SinFamily, SockType, Socket,
};
use crate::platform::api::Epoll;
use crate::platform::linux_x86_64::epoll_wait;
use crate::platform::linux_x86_64::EpollEvent;
use crate::platform::linux_x86_64::EpollEventBuilder;
use crate::platform::linux_x86_64::SocketAddressInetBuilder;
use std::sync::mpsc::Receiver;
use std::task::Waker;

pub fn open(port: u16) -> Result<Socket, Errno> {
    let fd = Socket::new(SinFamily::Inet, SockType::StreamNonBlock, Protocol::TCP)?;
    let addr = SocketAddressInetBuilder::default()
        .port(port)
        .build()
        .unwrap();
    fd.bind(&addr)?;
    fd.listen(1000)?;
    Ok(fd)
}

pub async fn accept<F: Fn(EpollSocket)>(fd: EpollSocket, f: F) {
    let mut fd = fd;
    loop {
        let (newfd, _) = fd.accept_async().await.unwrap();
        f(newfd);
    }
}

pub struct EpollReactor(pub Epoll);
impl EpollReactor {
    pub fn new() -> Self {
        let epoll = epoll_create1(EpollCreateFlag::None).unwrap();
        Self(Epoll(epoll as i32))
    }
    pub fn spawn(&self) -> Receiver<u64> {
        let (w, r) = std::sync::mpsc::channel();
        let epfd = (self.0).0;
        std::thread::spawn(move || loop {
            let mut waitevents = vec![EpollEventBuilder::uninitialized(); 100];
            let qtd = epoll_wait(epfd as u16, &mut waitevents, 10000).unwrap() as usize;
            for i in 0..qtd {
                let e = waitevents[i].data_as::<EpollEvent>();
                let waker = e.data_as::<Waker>();
                waker.wake_by_ref();
                w.send(e.data as u64).unwrap();
            }
        });
        r
    }
}
