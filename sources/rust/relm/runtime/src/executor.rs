use std::boxed::*;
use std::future::*;
use std::pin::*;
use std::task::*;

pub struct Executor {
    next: usize,
    handles: [Option<Pin<Box<dyn std::future::Future<Output = ()>>>>; 100],
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
        let waker = Self::clone(v);
        let waker = std::task::Waker::from_raw(waker);
        let mut cx = Context::from_waker(&waker);

        let handle = v as *mut Option<Pin<Box<dyn Future<Output = ()>>>>;
        let handle = &mut *handle;

        let remove = match handle.as_mut() {
            Some(handle) => match handle.as_mut().poll(&mut cx) {
                Poll::Ready(_) => true,
                Poll::Pending => false,
            },
            None => false,
        };

        if remove {
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
                let mut arr: [Option<Pin<Box<dyn std::future::Future<Output = ()>>>>; 100] =
                    MaybeUninit::zeroed().assume_init();
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
        let index = self.next;
        self.next += 1;

        let handle = Self::closure(handle);
        let handle = Box::pin(handle);
        self.handles[index] = Some(handle);
        let v = self.handles.get_mut(index).unwrap()
            as *mut Option<Pin<Box<dyn Future<Output = ()>>>> as *const ();

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

#[no_mangle]
pub fn wake_by_id(addr: usize) {
    let waker = unsafe { Box::from_raw(addr as *mut Waker) };
    waker.wake();
}
