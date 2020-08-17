use super::EpollEvent;
use crate::platform::linux_x86_64::{
    accept4, bind, close, epoll_ctl, listen, read, socket, write, Accept4Flags::NonBlock,
    EpollEventBuilder, EpollOperation, Errno, Protocol, SinFamily, SockType, SocketAddress,
    SocketAddressInet, SocketAddressInetBuilder,
};
use log::trace;
use std::{
    fmt::Debug,
    pin::Pin,
    sync::{
        atomic::{AtomicPtr, AtomicUsize, Ordering},
        Arc, Mutex, RwLock,
    },
    task::{Poll, RawWaker, Waker},
};

pub struct Socket(pub u16);

impl Debug for Socket {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Socket({})", self.0))
    }
}

impl Drop for Socket {
    fn drop(&mut self) {
        if self.0 > 0 {
            let _ = close(self.0);
        }
    }
}

impl Socket {
    pub fn new(
        domain: SinFamily,
        sock_type: SockType,
        protocol: Protocol,
        port: u16,
    ) -> Result<Self, Errno> {
        let socket = socket(domain, sock_type, protocol)?;
        let socket = Socket(socket);
        let addr = SocketAddressInetBuilder::default()
            .port(port)
            .build()
            .unwrap();
        socket.bind(&addr)?;
        socket.listen(1000)?;
        Ok(socket)
    }
    pub fn bind<T: SocketAddress>(&self, addr: &T) -> Result<(), Errno> {
        bind(self.0, addr)
    }
    pub fn listen(&self, backlog: u16) -> Result<(), Errno> {
        listen(self.0, backlog)
    }

    pub fn attach<'a>(mut self, epoll: &'a Epoll) -> EpollSocket<'a> {
        let mut fd = 0;
        std::mem::swap(&mut fd, &mut self.0);

        let idx = epoll.reserve();
        let mut event = EpollEvent::new(idx);
        epoll_ctl(epoll.fd, EpollOperation::Add, fd, &mut event).unwrap();

        EpollSocket { fd, epoll, idx }
    }
}

#[derive(Copy, Clone)]
struct EpollData {
    free: bool,
    data: usize,
}

pub struct Epoll {
    pub fd: u16,
    ptrs: Mutex<Vec<EpollData>>,
    nextptr: usize,
}

impl Drop for Epoll {
    fn drop(&mut self) {
        let _ = close(self.fd);
    }
}

impl Epoll {
    pub fn new(fd: u16) -> Self {
        Self {
            fd,
            ptrs: Mutex::new(vec![
                EpollData {
                    free: true,
                    data: 0
                };
                100
            ]),
            nextptr: 0,
        }
    }

    pub fn reserve(&mut self) -> usize {
        let mut g = self.ptrs.lock().unwrap();
        let mut idx = self.nextptr;
        while !g[idx].free {
            idx += 1;
            if idx >= g.len() {
                idx = 0;
            }
        }

        g[idx] = EpollData {
            free: false,
            data: 0,
        };

        self.nextptr += 1;
        if self.nextptr >= g.len() {
            self.nextptr = 0;
        }

        idx
    }

    pub fn take(&mut self, idx: usize) -> Option<&Waker> {
        let mut g = self.ptrs.lock().unwrap();
        if g[idx].free {
            None
        } else {
            g[idx].free = true;
            Some(unsafe { &*(g[idx].data as *const Waker) })
        }
    }

    pub fn store(&mut self, idx: usize, waker: &Waker) {
        let mut g = self.ptrs.lock().unwrap();
        g[idx].data = waker as *const Waker as usize;
    }
}

pub struct EpollSocket<'a> {
    fd: u16,
    epoll: &'a Epoll,
    idx: usize,
}

impl<'a> Debug for EpollSocket<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "EpollSocket(fd: {},epollfd: {})",
            self.fd, self.epoll.fd
        ))
    }
}

impl<'a> Drop for EpollSocket<'a> {
    fn drop(&mut self) {
        if self.fd > 0 {
            trace!(target:"SocketApi", "Socket closing {}", self.fd);

            epoll_ctl(
                self.epoll.fd,
                EpollOperation::Delete,
                self.fd,
                &mut EpollEventBuilder::uninitialized(),
            )
            .unwrap();
            close(self.fd).unwrap();
        }
    }
}

impl<'a> EpollSocket<'a> {
    pub async fn accept_async<'b>(
        &'b mut self,
    ) -> Result<(EpollSocket<'a>, SocketAddressInet), Errno> {
        let (fd, addr) = Accept4Awaitable(&self.fd, &self.epoll, &self.idx).await;

        let mut event = EpollEvent::new(self.idx);
        epoll_ctl(self.epoll.fd, EpollOperation::Modify, self.fd, &mut event)?;

        let idx = self.epoll.reserve();
        let mut event = EpollEvent::new(idx);
        epoll_ctl(self.epoll.fd, EpollOperation::Add, fd, &mut event)?;

        Ok((
            EpollSocket {
                fd,
                epoll: self.epoll,
                idx,
            },
            addr,
        ))
    }

    pub async fn read<'b>(&'b mut self, size: usize) -> Result<(Vec<u8>, usize), Errno> {
        ReadAwaitable(size, &self.fd, &self.epoll, &self.idx).await
    }

    pub async fn write<'b>(&'b mut self, buffer: &'a [u8]) -> Result<usize, Errno> {
        WriteAwaitable(buffer, &self.fd, &self.epoll, &self.idx).await
    }
}

struct Accept4Awaitable<'a>(&'a u16, &'a Epoll, &'a usize);
impl<'a> std::future::Future for Accept4Awaitable<'a> {
    type Output = (u16, SocketAddressInet);
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let Accept4Awaitable(fd, epoll, idx) = self.get_mut();

        trace!(target:"SocketApi", "Accept4Awaitable {}", **fd);
        epoll.store(**idx, ctx.waker());

        let mut addr: SocketAddressInet = SocketAddressInetBuilder::build_default();
        match accept4(**fd, &mut addr, NonBlock) {
            Ok(fd) => Poll::Ready((fd, addr)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(_) => Poll::Pending,
        }
    }
}

struct ReadAwaitable<'a>(usize, &'a u16, &'a Epoll, &'a usize);
impl<'a> std::future::Future for ReadAwaitable<'a> {
    type Output = Result<(Vec<u8>, usize), Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let ReadAwaitable(size, fd, epoll, idx) = self.get_mut();

        trace!(target:"SocketApi", "ReadAwaitable {}", **fd);
        epoll.store(**idx, ctx.waker());

        let mut buf = vec![0; *size];
        match read(**fd, &mut buf) {
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

struct WriteAwaitable<'a>(&'a [u8], &'a u16, &'a Epoll, &'a usize);
impl<'a> std::future::Future for WriteAwaitable<'a> {
    type Output = Result<usize, Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let WriteAwaitable(buffer, fd, epoll, idx) = self.get_mut();

        trace!(target:"SocketApi", "WriteAwaitable {}", **fd);
        epoll.store(**idx, ctx.waker());

        match write(**fd, buffer) {
            Ok(size) => Poll::Ready(Ok(size as usize)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}
