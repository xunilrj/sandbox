use crate::platform::linux_x86_64::{
    accept4, bind, close, epoll_ctl, listen, read, socket, write, Accept4Flags::NonBlock,
    EpollEventBuilder, EpollOperation, Errno, Protocol, SinFamily, SockType, SocketAddress,
    SocketAddressInet, SocketAddressInetBuilder,
};
use log::trace;
use std::{
    fmt::Debug,
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
    task::Poll,
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

pub struct EpollEventData(AtomicUsize);
impl EpollEventData {
    pub fn pinned() -> Pin<Box<EpollEventData>> {
        Box::pin(EpollEventData(AtomicUsize::new(0)))
    }

    // pub fn store_usize(&mut self, value: usize) {
    //     self.0.store(value, Ordering::SeqCst);
    // }

    // pub fn store_mut<T>(&mut self, value: &mut T) {
    //     self.0.store(value as *const T as usize, Ordering::SeqCst);
    // }

    pub fn store_ref<T>(&mut self, value: &T) -> Option<&T> {
        match self.0.swap(value as *const T as usize, Ordering::SeqCst) {
            0 => None,
            r => unsafe { Some(&*(r as *const T)) },
        }
    }

    pub fn take_ref<T>(&mut self) -> Option<&T> {
        match self.0.swap(0, Ordering::SeqCst) {
            0 => None,
            r => unsafe { Some(&*(r as *const T)) },
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
        let fd = self.0;
        self.0 = 0;

        let data = EpollEventData::pinned();
        let mut event = EpollEventBuilder::in_edge_triggered_one_shot()
            .data(&data)
            .pin_box();
        epoll_ctl(epoll.0, EpollOperation::Add, fd, &mut event).unwrap();

        EpollSocket { fd, epoll, data }
    }
}

pub struct Epoll(pub u16);

impl Drop for Epoll {
    fn drop(&mut self) {
        let _ = close(self.0);
    }
}

impl Epoll {}

pub struct EpollSocket<'a> {
    fd: u16,
    epoll: &'a Epoll,
    data: Pin<Box<EpollEventData>>,
}

impl<'a> Debug for EpollSocket<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!(
            "EpollSocket(fd: {},epollfd: {})",
            self.fd, self.epoll.0
        ))
    }
}

impl<'a> Drop for EpollSocket<'a> {
    fn drop(&mut self) {
        if self.fd > 0 {
            trace!(target:"SocketApi", "Socket closing {} {:X}", self.fd, self.data.as_ref().get_ref() as *const EpollEventData as usize);

            epoll_ctl(
                self.epoll.0,
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
        let (fd, addr) = Accept4Awaitable(&self.fd, &mut self.data).await;

        let mut event = EpollEventBuilder::in_edge_triggered_one_shot()
            .data(&self.data)
            .pin_box();
        epoll_ctl(self.epoll.0, EpollOperation::Modify, self.fd, &mut event)?;

        let data = EpollEventData::pinned();
        let mut event = EpollEventBuilder::in_edge_triggered_one_shot()
            .data(&data)
            .pin_box();
        epoll_ctl(self.epoll.0, EpollOperation::Add, fd, &mut event)?;

        Ok((
            EpollSocket {
                fd,
                epoll: self.epoll,
                data,
            },
            addr,
        ))
    }

    pub async fn read<'b>(&'b mut self, size: usize) -> Result<(Vec<u8>, usize), Errno> {
        ReadAwaitable(size, &self.fd, &mut self.data).await
    }

    pub async fn write<'b>(&'b mut self, buffer: &'a [u8]) -> Result<usize, Errno> {
        WriteAwaitable(buffer, &self.fd, &mut self.data).await
    }
}

struct Accept4Awaitable<'a>(&'a u16, &'a mut EpollEventData);
impl<'a> std::future::Future for Accept4Awaitable<'a> {
    type Output = (u16, SocketAddressInet);
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let Accept4Awaitable(fd, data) = self.get_mut();
        trace!(target:"SocketApi", "Accept4Awaitable {}", **fd);
        data.store_ref(ctx.waker());

        let mut addr: SocketAddressInet = SocketAddressInetBuilder::build_default();
        match accept4(**fd, &mut addr, NonBlock) {
            Ok(fd) => Poll::Ready((fd, addr)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(_) => Poll::Pending,
        }
    }
}

struct ReadAwaitable<'a>(usize, &'a u16, &'a mut EpollEventData);
impl<'a> std::future::Future for ReadAwaitable<'a> {
    type Output = Result<(Vec<u8>, usize), Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let ReadAwaitable(size, fd, data) = self.get_mut();
        trace!(target:"SocketApi", "ReadAwaitable {}", **fd);
        data.store_ref(ctx.waker());

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

struct WriteAwaitable<'a>(&'a [u8], &'a u16, &'a mut EpollEventData);
impl<'a> std::future::Future for WriteAwaitable<'a> {
    type Output = Result<usize, Errno>;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        let WriteAwaitable(buffer, fd, data) = self.get_mut();
        trace!(target:"SocketApi", "WriteAwaitable {}", **fd);
        data.store_ref(ctx.waker());

        match write(**fd, buffer) {
            Ok(size) => Poll::Ready(Ok(size as usize)),
            Err(Errno::WouldBlock) => Poll::Pending,
            Err(err) => Poll::Ready(Err(err)),
        }
    }
}
