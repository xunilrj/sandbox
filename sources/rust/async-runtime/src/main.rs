use std::{collections::HashMap, io::Read, os::raw::c_int};

use log::warn;

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
            if let Some(wake) = self.sleeping.get(&e.u64) {
                wake();
            }
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

fn main() {
    pretty_env_logger::init();

    let flags = unsafe { libc::fcntl(0, libc::F_GETFL, 0) };
    unsafe { libc::fcntl(0, libc::F_SETFL, flags | libc::O_NONBLOCK) };

    let mut epoll = Epoll::new().unwrap();
    epoll.add(0, 0, read_stdin);

    loop {
        epoll.wait();
    }
}
