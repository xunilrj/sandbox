use std::{convert::TryFrom, fmt::Debug, pin::Pin};

// https://filippo.io/linux-syscall-table/
#[repr(C)]
enum Syscall {
    Read = 0,
    Write = 1,
    Close = 3,
    Socket = 41,
    Bind = 49,
    Listen = 50,
    EpollWait = 232,
    EpollControl = 233,
    Accept4 = 288,
    EpollCreate1 = 291,
}

// https://www.man7.org/linux/man-pages/man3/errno.3.html
#[repr(C)]
#[derive(Clone, Copy)]
#[allow(dead_code)]
pub enum Errno {
    NoFile = 2,
    BadFileDescriptor = 9,
    WouldBlock = 11,
    Fault = 14,
    InvalidArgument = 22,
    MaxErrno = 4095,
}

impl From<&Errno> for String {
    fn from(errno: &Errno) -> Self {
        match errno {
            Errno::NoFile => "No such file or directory (POSIX.1-2001)".to_string(),
            Errno::WouldBlock => {
                "Operation would block (may be same value as EAGAIN)(POSIX.1-2001)".to_owned()
            }
            Errno::Fault => "Bad address (POSIX.1-2001)".to_owned(),
            Errno::BadFileDescriptor => "Bad file descriptor (POSIX.1-2001)".to_owned(),
            Errno::InvalidArgument => "Invalid argument (POSIX.1-2001)".to_owned(),
            x @ _ => format!("Unknown errno: [{}]", *x as i32),
        }
    }
}

impl From<Errno> for String {
    fn from(errno: Errno) -> Self {
        (&errno).into()
    }
}

impl Debug for Errno {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data: String = self.into();
        f.write_str(&data)
    }
}

extern "C" {
    pub fn __errno_location() -> *mut i32;
}

#[inline(always)]
unsafe fn as_errno<T: TryFrom<i64>>(ret: i64) -> Result<T, Errno> {
    if ret < 0 {
        Err(std::mem::transmute::<i32, Errno>(ret as i32 * -1))
    } else {
        Ok(match T::try_from(ret) {
            Ok(a) => a,
            _ => unreachable!(),
        })
    }
}

#[inline(always)]
fn syscall1<T: TryFrom<i64>>(n: Syscall, a1: usize) -> Result<T, Errno> {
    unsafe {
        let ret: i64;
        llvm_asm!("syscall"
         : "={rax}"(ret)
         : "{rax}"(n),
           "{rdi}"(a1)
         : "rcx",
           "r11",
           "memory"
         : "volatile");
        as_errno::<T>(ret)
    }
}

#[inline(always)]
fn syscall2<T: TryFrom<i64>>(n: Syscall, a1: usize, a2: usize) -> Result<T, Errno> {
    unsafe {
        let ret: i64;
        llvm_asm!("syscall"
         : "={rax}"(ret)
         : "{rax}"(n),
           "{rdi}"(a1),
           "{rsi}"(a2)
         : "rcx",
           "r11",
           "memory"
         : "volatile");
        as_errno::<T>(ret)
    }
}

#[inline(always)]
fn syscall3<T: TryFrom<i64>>(n: Syscall, a1: usize, a2: usize, a3: usize) -> Result<T, Errno> {
    unsafe {
        let ret: i64;
        llvm_asm!("syscall"
         : "={rax}"(ret)
         : "{rax}"(n),
           "{rdi}"(a1),
           "{rsi}"(a2),
           "{rdx}"(a3)
         : "rcx",
           "r11",
           "memory"
         : "volatile");
        as_errno::<T>(ret)
    }
}

#[inline(always)]
fn syscall4<T: TryFrom<i64>>(
    n: Syscall,
    a1: usize,
    a2: usize,
    a3: usize,
    a4: usize,
) -> Result<T, Errno> {
    unsafe {
        let ret: i64;
        llvm_asm!("syscall"
         : "={rax}"(ret)
         : "{rax}"(n),
           "{rdi}"(a1),
           "{rsi}"(a2),
           "{rdx}"(a3),
           "{r10}"(a4)
         : "rcx",
           "r11",
           "memory"
         : "volatile");
        as_errno::<T>(ret)
    }
}

extern "C" {
    #[link_name = "htons"]
    fn _htons(hostshort: u16) -> u16;
    #[link_name = "htonl"]
    fn _htonl(hostshort: u32) -> u32;
}

pub fn htons(hostshort: u16) -> u16 {
    unsafe { _htons(hostshort) }
}
pub fn htonl(hostshort: u32) -> u32 {
    unsafe { _htonl(hostshort) }
}

// https://linux.die.net/man/2/read
// https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/read_write.c#n596
// EAGAIN
// EAGAIN or EWOULDBLOCK
// EBADF
// EFAULT
// EINTR
// EINVAL
// EINVAL
// EIO
// EISDIR
#[inline(always)]
pub fn read(fd: u16, buffer: &mut [u8]) -> Result<usize, Errno> {
    syscall3(
        Syscall::Read,
        fd as usize,
        buffer.as_ptr() as usize,
        buffer.len(),
    )
}

// https://linux.die.net/man/2/write
// https://git.kernel.org/pub/scm/linux/kernel/git/torvalds/linux.git/tree/fs/read_write.c#n620
// EAGAIN
// EAGAIN or EWOULDBLOCK
// EBADF
// EDESTADDRREQ
// EDQUOT
// EFAULT
// EFBIG
// EINTR
// EINVAL
// EIO
// ENOSPC
// EPIPE
#[inline(always)]
pub fn write(fd: u16, buffer: &[u8]) -> Result<usize, Errno> {
    syscall3::<usize>(
        Syscall::Write,
        fd as usize,
        buffer.as_ptr() as usize,
        buffer.len(),
    )
}

#[repr(C)]
#[derive(Default, Debug, Clone, Copy)]
pub struct BigEndianU16(u16);

impl From<u16> for BigEndianU16 {
    fn from(v: u16) -> Self {
        BigEndianU16(htons(v))
    }
}

#[repr(C)]
#[derive(Default, Debug, Clone, Copy)]
pub struct BigEndianU32(u32);

impl From<u32> for BigEndianU32 {
    fn from(v: u32) -> Self {
        BigEndianU32(htonl(v))
    }
}

#[repr(u16)]
#[derive(Debug, Clone, Copy)]
pub enum SinFamily {
    Inet = 2,
}

// https://www.man7.org/linux/man-pages/man3/inet_addr.3.html

pub trait SocketAddress {}

#[repr(C, packed)]
#[derive(Clone, Copy, Debug, Builder)]
#[builder(setter(into))]
pub struct SocketAddressInet {
    #[builder(field(private), default = "SinFamily::Inet")]
    family: SinFamily,
    #[builder(default = "0.into()")]
    pub port: BigEndianU16,
    #[builder(default = "0.into()")]
    pub addr: BigEndianU32,
    #[builder(field(private), default = "[0;8]")]
    pad: [u8; 8],
}
impl SocketAddress for SocketAddressInet {}

impl SocketAddressInetBuilder {
    pub fn build_default() -> SocketAddressInet {
        SocketAddressInetBuilder::default().build().unwrap()
    }
}

// https://linux.die.net/man/2/accept4
#[repr(C)]
#[allow(dead_code)]
pub enum Accept4Flags {
    NonBlock = 0o000_4000,
    Cloexec = 0o200_0000,
}
#[inline(always)]
pub fn accept4<T: SocketAddress>(
    sockfd: u16,
    addr: &mut T,
    flags: Accept4Flags,
) -> Result<u16, Errno> {
    let mut len = std::mem::size_of::<T>();
    syscall4::<u16>(
        Syscall::Accept4,
        sockfd as usize,
        addr as *mut T as usize,
        &mut len as *mut usize as usize,
        flags as usize,
    )
}

// https://www.man7.org/linux/man-pages/man2/bind.2.html
pub fn bind<T: SocketAddress>(sockfd: u16, addr: &T) -> Result<(), Errno> {
    syscall3::<usize>(
        Syscall::Bind,
        sockfd as usize,
        addr as *const T as usize,
        std::mem::size_of::<T>(),
    )
    .map(|_| {})
}

// https://www.man7.org/linux/man-pages/man2/listen.2.html
#[inline(always)]
pub fn listen(sockfd: u16, backlog: u16) -> Result<(), Errno> {
    syscall2::<usize>(Syscall::Listen, sockfd as usize, backlog as usize).map(|_| {})
}

#[derive(Debug, Clone, Copy)]
#[allow(dead_code)]
pub enum SockType {
    Stream = 1,
    NonBlock = 0o000_4000,
    StreamNonBlock = 0o000_4000 | 1,
}

#[derive(Debug, Clone, Copy)]
pub enum Protocol {
    TCP = 6,
}

// https://www.man7.org/linux/man-pages/man2/socket.2.html
pub fn socket(domain: SinFamily, sock_type: SockType, protocol: Protocol) -> Result<u16, Errno> {
    syscall3::<u16>(
        Syscall::Socket,
        domain as usize,
        sock_type as usize,
        protocol as usize,
    )
}

#[repr(C)]
pub enum EpollCreateFlag {
    None = 0,
}

// https://linux.die.net/man/2/epoll_create1
pub fn epoll_create1(flags: EpollCreateFlag) -> Result<u16, Errno> {
    syscall1(Syscall::EpollCreate1, flags as usize)
}

#[repr(u32)]
#[derive(Copy, Clone)]
#[allow(dead_code)]
pub enum EpollEvents {
    In = 0x0000_0001,
    EdgeTriggered = 1 << 31,
    OneShot = 1 << 30,
    InOneShot = 0x0000_0001 | 1 << 30,
}

#[repr(C, packed)]
#[derive(Copy, Clone)]
pub struct EpollEvent {
    pub events: EpollEvents,
    pub data: usize,
}

impl EpollEvent {
    // pub fn data_as_ref<T>(&self) -> Option<&T> {
    //     if self.data == 0 {
    //         None
    //     } else {
    //         Some(unsafe { &*(self.data as *const () as *const T) })
    //     }
    // }

    pub fn data_as_mut<T>(&self) -> Option<&mut T> {
        if self.data == 0 {
            None
        } else {
            Some(unsafe { &mut *(self.data as *mut () as *mut T) })
        }
    }
}

pub struct EpollEventBuilder(EpollEvents, Option<*const ()>);
impl EpollEventBuilder {
    pub fn uninitialized() -> EpollEvent {
        EpollEvent {
            events: EpollEvents::InOneShot,
            data: 0,
        }
    }

    pub fn in_edge_triggered_one_shot() -> Self {
        Self(EpollEvents::InOneShot, None)
    }

    pub fn data<T>(&mut self, pin: &Pin<Box<T>>) -> &mut Self {
        self.1 = Some(pin.as_ref().get_ref() as *const T as *const ());
        self
    }

    pub fn pin_box(&self) -> Pin<Box<EpollEvent>> {
        Box::pin(EpollEvent {
            events: self.0,
            data: match self.1 {
                None => 0,
                Some(v) => v as usize,
            },
        })
    }
}

pub enum EpollOperation {
    Add = 1,
    Delete = 2,
    Modify = 3,
}

pub fn epoll_ctl(
    epfd: u16,
    op: EpollOperation,
    fd: u16,
    event: &mut EpollEvent,
) -> Result<(), Errno> {
    syscall4::<usize>(
        Syscall::EpollControl,
        epfd as usize,
        op as usize,
        fd as usize,
        event as *mut EpollEvent as usize,
    )
    .map(|_| {})
}

pub fn close(fd: u16) -> Result<(), Errno> {
    syscall1::<usize>(Syscall::Close, fd as usize).map(|_| {})
}

pub fn epoll_wait(epfd: u16, events: &mut [EpollEvent], timeout: i32) -> Result<u16, Errno> {
    syscall4::<u16>(
        Syscall::EpollWait,
        epfd as usize,
        events.as_mut_ptr() as usize,
        events.len() as usize,
        timeout as usize,
    )
}
