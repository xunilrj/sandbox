use nix::{
    libc::printf,
    sys::{
        epoll::{
            epoll_create1, epoll_ctl, epoll_wait, EpollCreateFlags, EpollEvent, EpollFlags, EpollOp,
        },
        socket::{
            self, accept4, bind, listen, socket, AddressFamily, InetAddr, SockAddr, SockFlag,
            SockProtocol, SockType,
        },
    },
};
use std::sync::mpsc::Sender;
use std::{
    net::{IpAddr, Ipv4Addr, SocketAddr, SocketAddrV4},
    os::unix::prelude::RawFd,
};

use std::boxed::*;
use std::future::*;
use std::pin::*;
use std::task::*;

type ExecutorHandleType = Option<Pin<Box<dyn std::future::Future<Output = ()>>>>;
pub struct Executor {
    next: usize,
    handles: [ExecutorHandleType; 10 * 1024],
}

pub static mut VTABLE: RawWakerVTable = std::task::RawWakerVTable::new(
    Executor::clone,
    Executor::wake,
    Executor::wake_by_ref,
    Executor::drop,
);

impl Executor {
    unsafe fn clone(v: *const ()) -> RawWaker {
        std::task::RawWaker::new(v, &VTABLE)
    }
    unsafe fn wake(v: *const ()) {
        // println!("w {}", v as usize);

        let waker = Self::clone(v);
        let waker = std::task::Waker::from_raw(waker);
        let mut cx = Context::from_waker(&waker);

        let handle = v as *mut ExecutorHandleType;
        let handle = &mut *handle;

        let remove = match handle.as_mut() {
            Some(handle) => match handle.as_mut().poll(&mut cx) {
                Poll::Ready(_) => true,
                Poll::Pending => false,
            },
            None => false,
        };

        if remove {
            // println!("killing {}", v as usize);
            handle.take();
        }
    }
    unsafe fn wake_by_ref(v: *const ()) {
        Self::wake(v)
    }
    unsafe fn drop(_: *const ()) {}

    pub fn new() -> Self {
        use std::mem::MaybeUninit;
        Self {
            next: 0,
            handles: unsafe {
                let mut arr: [ExecutorHandleType; 10 * 1024] = MaybeUninit::zeroed().assume_init();
                for dst in &mut arr[..] {
                    std::ptr::write(dst, None);
                }
                arr
            },
        }
    }

    async fn closure<T, THandle: std::future::Future<Output = T>>(handle: THandle) {
        let _ = handle.await;
    }

    pub fn push<T: 'static, THandle: 'static + std::future::Future<Output = T>>(
        &mut self,
        handle: THandle,
    ) -> RawWaker {
        let handle = Self::closure(handle);
        let handle = Box::pin(handle);

        let mut i = rand::random::<usize>() % self.handles.len();
        loop {
            if self.handles[i].is_none() {
                self.handles[i] = Some(handle);
                break;
            }
            i += 1 % self.handles.len();
        }

        let v = self.handles.get_mut(i).unwrap() as *mut ExecutorHandleType as *const ();

        // println!("s {}", v as usize);
        unsafe { Self::clone(v) }
    }

    pub fn spawn<T: 'static, THandle: 'static + std::future::Future<Output = T>>(
        &mut self,
        handle: THandle,
    ) {
        let waker = self.push(handle);
        let waker = unsafe { std::task::Waker::from_raw(waker) };
        waker.wake();
    }
}

pub fn wake_by_id(addr: u64) {
    let waker = unsafe { Box::from_raw(addr as *mut Waker) };
    waker.wake();
}

fn set_epoll(waker: &Waker, op: nix::sys::epoll::EpollOp, epfd: i32, fd: i32) {
    let addr = waker.clone();
    let addr = Box::leak(Box::new(addr)) as *mut Waker as u64;

    let mut flags = EpollFlags::empty();
    flags.set(EpollFlags::EPOLLIN, true);
    flags.set(EpollFlags::EPOLLOUT, true);
    flags.set(EpollFlags::EPOLLONESHOT, true);
    flags.set(EpollFlags::EPOLLET, true);
    let mut e = EpollEvent::new(flags, addr);
    match epoll_ctl(epfd, op, fd, Some(&mut e)) {
        OK => {}
        Err(_) => {
            // println!("adding");
            epoll_ctl(epfd, EpollOp::EpollCtlAdd, fd, Some(&mut e)).unwrap();
        }
    }
}

struct EpollCtlAdd(i32, i32);
impl std::future::Future for EpollCtlAdd {
    type Output = ();

    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        let epfd = self.0;
        let fd = self.1;
        set_epoll(ctx.waker(), EpollOp::EpollCtlAdd, epfd, fd);

        std::task::Poll::Ready(())
    }
}

struct Accept4(i32, i32);
impl std::future::Future for Accept4 {
    type Output = Result<RawFd, nix::Error>;

    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        let epfd = self.0;
        let sockfd = self.1;

        let flags = SockFlag::empty();
        match accept4(sockfd, flags) {
            Err(nix::Error::Sys(nix::errno::Errno::EAGAIN)) => {
                set_epoll(ctx.waker(), EpollOp::EpollCtlMod, epfd, sockfd);
                std::task::Poll::Pending
            }
            r @ _ => std::task::Poll::Ready(r),
        }
    }
}

struct Read<'a>(i32, i32, &'a mut Vec<u8>);
impl<'a> std::future::Future for Read<'a> {
    type Output = Result<usize, nix::Error>;

    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        let epfd = self.0;
        let sockfd = self.1;
        let buf = self.2.as_mut_slice();

        match nix::unistd::read(sockfd, buf) {
            Err(nix::Error::Sys(nix::errno::Errno::EAGAIN)) => {
                set_epoll(ctx.waker(), EpollOp::EpollCtlMod, epfd, sockfd);
                std::task::Poll::Pending
            }
            r @ _ => std::task::Poll::Ready(r),
        }
    }
}
async fn read(epfd: i32, fd: i32, buf: &mut Vec<u8>) -> Result<usize, nix::Error> {
    Read(epfd, fd, buf).await
}

struct Write<'a>(i32, i32, &'a [u8]);
impl<'a> std::future::Future for Write<'a> {
    type Output = Result<usize, nix::Error>;

    fn poll(mut self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        let epfd = self.0;
        let sockfd = self.1;
        let buf = self.2;

        match nix::unistd::write(sockfd.into(), buf) {
            Err(nix::Error::Sys(nix::errno::Errno::EAGAIN)) => {
                set_epoll(ctx.waker(), EpollOp::EpollCtlMod, epfd, sockfd);
                std::task::Poll::Pending
            }
            r @ _ => std::task::Poll::Ready(r),
        }
    }
}
async fn write(epfd: i32, fd: i32, buf: &[u8]) -> Result<usize, nix::Error> {
    Write(epfd, fd, buf).await
}

async fn start(epfd: i32, output: crossbeam::channel::Sender<i32>) -> Result<(), nix::Error> {
    let family = AddressFamily::Inet;
    let socket_type = SockType::Stream;
    let flags = SockFlag::SOCK_NONBLOCK;
    let protocol = SockProtocol::Tcp;
    let fd = socket(family, socket_type, flags, protocol).unwrap();

    let addr = SocketAddrV4::new(Ipv4Addr::new(127, 0, 0, 1), 8080);
    let addr = InetAddr::from_std(&SocketAddr::V4(addr));
    let addr = SockAddr::Inet(addr);
    loop {
        match bind(fd, &addr) {
            Ok(_) => break,
            _ => {
                print!(".");
                use std::io::Write;
                std::io::stdout().flush();
                std::thread::sleep(std::time::Duration::from_secs(1));
            }
        }
    }
    listen(fd, 100).unwrap();
    println!("");
    println!("at 127.0.0.1:80");

    EpollCtlAdd(epfd, fd).await;

    loop {
        let fd = Accept4(epfd, fd).await?;
        output.send(fd).unwrap();
    }
}

async fn read_all(epfd: i32, fd: i32) {
    let mut buffer = vec![0; 1024];
    loop {
        let qtd = match read(epfd, fd, &mut buffer).await {
            Ok(qtd) => qtd,
            _ => break,
        };

        let request = {
            let slice = buffer.as_slice();
            let mut cursor = std::io::Cursor::new(&slice[0..qtd]);
            match http::HttpRequest::try_parse_from_cursor(&mut cursor) {
                Ok(request) => request,
                _ => break,
            }
        };

        let connection = request.header("Connection").unwrap_or("close");

        let response = {
            use std::io::Write;
            let mut response = http::HttpResponseBuilder::default()
                .status(http::ResponseStatus::Ok)
                .build()
                .unwrap();
            response.push_header("Content-Type", "plain/text");
            response.body_writer().write_all("BODY".as_bytes()).unwrap();
            response.push_content_length();
            response
        };

        {
            let inner = buffer.as_mut_slice();
            let mut cursor = std::io::Cursor::new(inner);
            let qtd = response.dump_to_write(&mut cursor).unwrap();
            drop(cursor);
            let buffer = &buffer.as_slice()[0..qtd];
            match write(epfd, fd, buffer).await {
                Ok(_) => {}
                _ => break,
            };
        };

        if connection == "close" {
            break;
        }
    }

    nix::unistd::close(fd).unwrap();
    // println!("closed");

    //         b
    //     }
    //     _ => b,
    // };

    // buffer = b;
    // // }
}

static mut CONTINUE: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(true);

extern "C" fn control_c(signal: i32) {
    println!("control+c");
    unsafe { CONTINUE.store(false, std::sync::atomic::Ordering::SeqCst) };
}

struct SecondTimeReady(u8);
impl std::future::Future for SecondTimeReady {
    type Output = ();
    fn poll(mut self: std::pin::Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.0 == 0 {
            self.0 += 1;
            let addr = ctx.waker().clone();
            let addr = Box::leak(Box::new(addr)) as *mut Waker as usize;
            std::task::Poll::Pending
        } else {
            std::task::Poll::Ready(())
        }
    }
}

async fn f1() {
    print!("1");
    SecondTimeReady(1).await;
    print!("2");
    SecondTimeReady(1).await;
    print!("3");
    SecondTimeReady(1).await;
    print!("4");
}

fn spawn_handler(epfd: i32, r: crossbeam::channel::Receiver<i32>) {
    std::thread::spawn({
        move || {
            let mut executor2 = Executor::new();
            loop {
                let epfd = epfd;
                match r.recv() {
                    Ok(fd) => {
                        // println!("{:?}", std::thread::current().id());
                        let handle = read_all(epfd, fd);
                        executor2.spawn(handle);
                    }
                    _ => break,
                }
            }
        }
    });
}

fn spawn_waker(epfd: i32, r: crossbeam::channel::Receiver<u64>) {
    std::thread::spawn({
        move || loop {
            match r.recv() {
                Ok(addr) => wake_by_id(addr),
                _ => break,
            }
        }
    });
}

fn main() {
    let handler = nix::sys::signal::SigHandler::Handler(control_c);
    let flags = nix::sys::signal::SaFlags::empty();
    let mask = nix::sys::signal::SigSet::empty();
    let action = nix::sys::signal::SigAction::new(handler, flags, mask);
    let _ = unsafe { nix::sys::signal::sigaction(nix::sys::signal::Signal::SIGINT, &action) };

    let flags = EpollCreateFlags::empty();
    let epfd = epoll_create1(flags).unwrap();

    let mut executor = Executor::new();

    let (s, r) = crossbeam::channel::unbounded();
    let start_handle = start(epfd, s);
    executor.spawn(start_handle);
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());
    spawn_handler(epfd, r.clone());

    let (s, r) = crossbeam::channel::unbounded();
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());
    spawn_waker(epfd, r.clone());

    let mut events = vec![EpollEvent::empty(); 10000];
    loop {
        let cont = unsafe { CONTINUE.load(std::sync::atomic::Ordering::SeqCst) };
        if !cont {
            break;
        }
        match epoll_wait(epfd, events.as_mut_slice(), -1) {
            Ok(qtd) => {
                for e in events.iter().take(qtd) {
                    s.send(e.data()).unwrap();
                }
            }
            _ => {}
        }
    }

    nix::unistd::close(epfd).unwrap();

    // let mut executor = Executor::new();
    // executor.spawn(f1());
}
