use crate::platform::linux_x86_64::{
    accept4, bind, close, epoll_ctl, listen, read, socket, write, Accept4Flags::NonBlock,
    EpollEvent, EpollEventBuilder, EpollOperation, Errno, Protocol, SinFamily, SockType,
    SocketAddress, SocketAddressInet, SocketAddressInetBuilder,
};
use std::{
    fmt::Debug,
    pin::Pin,
    task::{Poll, Waker},
};

pub struct Socket(pub i32);

impl Debug for Socket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Socket({})", self.0))
    }
}

impl Drop for Socket {
    fn drop(&mut self) {
        if self.0 > 0 {
            let _ = close(self.0 as u16);
        }
    }
}

impl Socket {
    pub fn new(domain: SinFamily, sock_type: SockType, protocol: Protocol) -> Result<Self, Errno> {
        let fd = socket(domain, sock_type, protocol)?;
        Ok(Self(fd as i32))
    }
    pub fn bind<T: SocketAddress>(&self, addr: &T) -> Result<(), Errno> {
        bind(self.0 as u16, addr)
    }
    pub fn listen(&self, backlog: u16) -> Result<(), Errno> {
        listen(self.0 as u16, backlog as u16)
    }
}

pub struct Epoll(pub i32);

impl Drop for Epoll {
    fn drop(&mut self) {
        let _ = close(self.0 as u16);
    }
}

impl Epoll {
    pub fn add_to(&mut self, mut socket: Socket) -> EpollSocket {
        let fd = socket.0;
        socket.0 = 0;

        let mut event = EpollEventBuilder::in_edge_triggered().pin_box();
        event.data_to_self();
        let _ = epoll_ctl(self.0 as u16, EpollOperation::Add, fd as u16, &mut event);

        EpollSocket {
            fd,
            epollfd: self.0,
            event,
        }
    }
}

pub struct EpollSocket {
    fd: i32,
    epollfd: i32,
    event: Pin<Box<EpollEvent>>,
}

impl Debug for EpollSocket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("EpollSocket({},{})", self.fd, self.epollfd))
    }
}

impl Drop for EpollSocket {
    fn drop(&mut self) {
        if self.fd > 0 {
            let _ = epoll_ctl(
                self.epollfd as u16,
                EpollOperation::Delete,
                self.fd as u16,
                &mut self.event,
            );
            let _ = close(self.fd as u16);
        }
    }
}

impl EpollSocket {
    pub async fn accept_async(&mut self) -> Result<(EpollSocket, SocketAddressInet), Errno> {
        let (fd, addr) = Accept4Awaitable(self).await;

        let mut event = EpollEventBuilder::in_edge_triggered().pin_box();
        event.data_to_self();
        epoll_ctl(self.epollfd as u16, EpollOperation::Add, fd, &mut event)?;

        Ok((
            EpollSocket {
                fd: fd as i32,
                epollfd: self.epollfd,
                event,
            },
            addr,
        ))
    }

    pub async fn read(&mut self, size: usize) -> Result<(Vec<u8>, usize), Errno> {
        ReadAwaitable(size, self).await
    }

    pub async fn write(&mut self, buffer: &[u8]) -> Result<usize, Errno> {
        WriteAwaitable(buffer, self).await
    }
}

struct Accept4Awaitable<'a>(&'a mut EpollSocket);
impl<'a> std::future::Future for Accept4Awaitable<'a> {
    type Output = (u16, SocketAddressInet);
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let Accept4Awaitable(EpollSocket { fd, event, .. }) = self.get_mut();
        let e = event.as_mut().get_mut();
        e.data = ctx.waker() as *const Waker as usize;

        let mut addr: SocketAddressInet = SocketAddressInetBuilder::build_default();
        match accept4(*fd as u16, &mut addr, NonBlock) {
            Ok(fd) => Poll::Ready((fd, addr)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(_) => Poll::Pending,
        }
    }
}

struct ReadAwaitable<'a>(usize, &'a mut EpollSocket);
impl<'a> std::future::Future for ReadAwaitable<'a> {
    type Output = Result<(Vec<u8>, usize), Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let ReadAwaitable(size, socket) = self.get_mut();
        let e = socket.event.as_mut().get_mut();
        e.data = ctx.waker() as *const Waker as usize;

        let mut buf = vec![0; *size];
        match read(socket.fd as u16, &mut buf) {
            Ok(size) => {
                if size == 0 {
                    Poll::Ready(Err(Errno::BadFileDescriptor))
                } else {
                    Poll::Ready(Ok((buf, size as usize)))
                }
            }
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}

struct WriteAwaitable<'a>(&'a [u8], &'a mut EpollSocket);
impl<'a> std::future::Future for WriteAwaitable<'a> {
    type Output = Result<usize, Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let WriteAwaitable(buffer, socket) = self.get_mut();
        let e = socket.event.as_mut().get_mut();
        e.data = ctx.waker() as *const Waker as usize;

        match write(socket.fd as u16, buffer) {
            Ok(size) => Poll::Ready(Ok(size as usize)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}
