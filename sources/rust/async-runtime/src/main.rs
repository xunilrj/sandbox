use std::{
    any::Any,
    collections::HashMap,
    future::Future,
    io::Read,
    os::raw::c_int,
    pin::Pin,
    sync::atomic::{AtomicUsize, Ordering},
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

use log::{trace, warn};

pub struct Epoll {
    fd: c_int,
    sleeping: HashMap<u64, Box<dyn Fn()>>,
}

#[derive(Debug)]
pub enum EpollCreationErrors {
    EINVAL,
    EMFILE,
    ENFILE,
    ENOMEM,
    UnknownError,
}

#[derive(Debug)]
pub enum EpollAddErrors {
    EBADF,
    EEXIST,
    EINVAL,
    ENOENT,
    ENOMEM,
    ENOSPC,
    EPERM,
    UnknownError,
}

impl Epoll {
    pub fn new() -> Result<Self, EpollCreationErrors> {
        let fd = unsafe { libc::epoll_create1(0) };
        if fd == -1 {
            let errno = unsafe { *libc::__errno_location() };
            let err = match errno {
                libc::EINVAL => EpollCreationErrors::EINVAL,
                libc::EMFILE => EpollCreationErrors::EMFILE,
                libc::ENFILE => EpollCreationErrors::ENFILE,
                libc::ENOMEM => EpollCreationErrors::ENOMEM,
                _ => EpollCreationErrors::UnknownError,
            };
            Err(err)
        } else {
            Ok(Self {
                fd,
                sleeping: HashMap::new(),
            })
        }
    }

    pub fn add(&mut self, fd: i32, tag: u64, f: impl Fn() + 'static) -> Result<(), EpollAddErrors> {
        let mut e = libc::epoll_event {
            events: libc::EPOLLIN as u32,
            u64: 0,
        };
        let r = unsafe { libc::epoll_ctl(self.fd, libc::EPOLL_CTL_ADD, 0, &mut e) };
        if r == -1 {
            let errno = unsafe { *libc::__errno_location() };
            let err = match errno {
                libc::EBADF => EpollAddErrors::EBADF,
                libc::EEXIST => EpollAddErrors::EEXIST,
                libc::EINVAL => EpollAddErrors::EINVAL,
                libc::ENOENT => EpollAddErrors::ENOENT,
                libc::ENOMEM => EpollAddErrors::ENOMEM,
                libc::ENOSPC => EpollAddErrors::ENOSPC,
                libc::EPERM => EpollAddErrors::EPERM,
                _ => EpollAddErrors::UnknownError,
            };
            Err(err)
        } else {
            self.sleeping.insert(fd as u64, Box::new(f));
            Ok(())
        }
    }

    pub fn wait(&self) {
        let mut events: [libc::epoll_event; 1024] =
            unsafe { std::mem::MaybeUninit::zeroed().assume_init() };
        let event_count =
            unsafe { libc::epoll_wait(self.fd, events.as_mut_ptr(), events.len() as i32, -1) };
        for e in &events[0..event_count as usize] {
            let waker = unsafe { &mut *(e.u64 as *mut Waker) };
            waker.wake_by_ref();
        }
    }
}

impl Drop for Epoll {
    fn drop(&mut self) {
        let r = unsafe { libc::close(self.fd) };
        if r != 0 {
            let errno = unsafe { *libc::__errno_location() };
            warn!("Failed when closing epoll {}. errno: {errno}", self.fd);
        }
    }
}

fn read_stdin() {
    println!("fd 0 is ready");
    let mut buffer = [0u8; 1024];
    loop {
        let len = unsafe {
            libc::read(
                0,
                buffer.as_mut_ptr() as *mut std::ffi::c_void,
                buffer.len(),
            )
        };
        if len <= 0 {
            break;
        }
    }
}

struct ReadStdInAsync {
    epoll_fd: i32,
}

impl std::future::Future for ReadStdInAsync {
    type Output = (usize, [u8; 1024]);

    fn poll(self: Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        trace!("ReadStdInAsync::poll");

        let mut buffer = [0u8; 1024];
        let len = unsafe {
            libc::read(
                0,
                buffer.as_mut_ptr() as *mut std::ffi::c_void,
                buffer.len(),
            )
        };
        if len == -1 {
            trace!("ReadStdInAsync::poll: would block");
            let waker = Box::leak(Box::new(ctx.waker().clone())) as *mut Waker as usize;

            let mut e = libc::epoll_event {
                events: libc::EPOLLIN as u32,
                u64: waker as u64,
            };
            let r = unsafe { libc::epoll_ctl(self.epoll_fd, libc::EPOLL_CTL_ADD, 0, &mut e) };

            Poll::Pending
        } else {
            trace!("ReadStdInAsync::poll: ok");
            Poll::Ready((len as usize, buffer))
        }
    }
}

pub struct SimpleWaker {}

impl SimpleWaker {
    pub fn clone(data: *const ()) -> RawWaker {
        let ctx = unsafe { &mut *(data as *mut () as *mut FutureContext) };
        ctx.refs.fetch_add(1, Ordering::SeqCst);
        trace!("SimpleWaker::clone");
        RawWaker::new(data, &vtable)
    }

    pub fn wake(ptr: *const ()) {
        trace!("SimpleWaker::wake");
        dbg!(ptr);
    }

    pub fn wake_by_ref(ptr: *const ()) {
        trace!("SimpleWaker::wake_by_ref");
        let ctx = unsafe { &mut *(ptr as *mut FutureContext) };
        (ctx.wake)(ctx);
    }

    pub fn drop(ptr: *const ()) {
        trace!("SimpleWaker::drop");
        let ctx = unsafe { &mut *(ptr as *mut () as *mut FutureContext) };
        let refs = ctx.refs.fetch_sub(1, Ordering::SeqCst);
        if refs == 0 {
            trace!("SimpleWaker::drop: kill ctx");
        }
    }
}

const vtable: RawWakerVTable = RawWakerVTable::new(
    SimpleWaker::clone,
    SimpleWaker::wake,
    SimpleWaker::wake_by_ref,
    SimpleWaker::drop,
);

struct FutureContext {
    refs: AtomicUsize,
    executor: *mut Executor,
    future: Pin<Box<dyn Any>>,
    wake: for<'a> fn(&mut FutureContext),
}

struct Executor {}

impl Executor {
    pub fn poll<O, F: 'static + Future<Output = O>>(&mut self, ctx: &mut FutureContext) {
        trace!("Executor::poll");

        let waker = RawWaker::new(ctx as *mut FutureContext as *mut (), &vtable);
        let waker = unsafe { Waker::from_raw(waker) };
        let mut cx = std::task::Context::from_waker(&waker);

        let future = unsafe {
            ctx.future
                .as_mut()
                .map_unchecked_mut(|x| x.downcast_mut::<F>().unwrap())
        };
        future.poll(&mut cx);
    }
}

fn executor_spawn<O, F: 'static + Future<Output = O>>(ctx: &mut FutureContext) {
    trace!("executor_spawn");

    let executor = unsafe { &mut *(ctx.executor) };
    executor.poll::<O, F>(ctx);
}

struct Runtime {
    executor: Pin<Box<Executor>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            executor: Box::pin(Executor {}),
        }
    }

    pub fn spawn<O, F: 'static + Future<Output = O>>(&mut self, future: F) {
        trace!("Runtime::spawn");

        let ctx = FutureContext {
            refs: AtomicUsize::new(0),
            executor: self.executor.as_mut().get_mut() as *mut Executor,
            future: Box::pin(future),
            wake: executor_spawn::<O, F>,
        };

        let ptr = Box::leak(Box::new(ctx));
        self.executor.poll::<O, F>(ptr);
    }
}

async fn read_stdin_async(epoll_fd: i32) {
    for _ in 0..2 {
        trace!("read_stdin_async::loop");
        let (len, buffer) = ReadStdInAsync { epoll_fd }.await;
        let str = std::str::from_utf8(&buffer[0..len]).unwrap();
        println!("Read: {str}");
    }
}

fn main() {
    pretty_env_logger::init();

    let flags = unsafe { libc::fcntl(0, libc::F_GETFL, 0) };
    unsafe { libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) };

    let mut epoll = Epoll::new().unwrap();

    let mut exec = Runtime::new();
    exec.spawn(read_stdin_async(epoll.fd));

    // let mut f = Box::pin();
    // let waker = RawWaker::new(Pin::, &vtable);
    // let waker = unsafe { Waker::from_raw(waker) };

    // let mut ctx = Context::from_waker(&waker);
    // f.as_mut().poll(&mut ctx);

    loop {
        epoll.wait();
    }
}
