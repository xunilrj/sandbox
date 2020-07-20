use std::{
    cell::RefCell,
    future::Future,
    pin::Pin,
    sync::Arc,
    task::{Context, Poll, RawWaker, RawWakerVTable, Waker},
};

unsafe fn clone(data: *const ()) -> RawWaker {
    RawWaker::new(data, &VTABLE)
}
unsafe fn wake(_: *const ()) {}
unsafe fn wake_by_ref(data: *const ()) {
    let task = &mut *(data as *mut Task);
    let spawner = &mut *task.rt;
    spawner.poll(data);
}
unsafe fn drop(_: *const ()) {}
pub static VTABLE: RawWakerVTable = RawWakerVTable::new(clone, wake, wake_by_ref, drop);

fn new_waker(data: *const ()) -> Waker {
    let waker = RawWaker::new(data, &VTABLE);
    let waker = unsafe { Waker::from_raw(waker) };
    waker
}

struct Spawner {}

impl Spawner {
    pub fn spawn<'a, T: 'a + Future<Output = ()>>(&'a mut self, f: T)
    where
        T::Output: std::fmt::Debug,
    {
        let future = Box::leak(Box::new(f));
        let task = Box::leak(Box::new(Task {
            rt: self as *mut Spawner,
            waker: new_waker(0 as *const ()),
            f: future,
        }));
        let data = task as *const Task as *const ();
        task.waker = new_waker(data);
        let mut context = Context::from_waker(&task.waker);
        let p = unsafe { Pin::new_unchecked(&mut *task.f) };
        match p.poll(&mut context) {
            Poll::Pending => {}
            Poll::Ready(_) => {
                let b = unsafe { Box::from_raw(data as *mut Task) };
                let _ = unsafe { Box::from_raw(b.f) };
            }
        };
    }

    fn poll(&mut self, data: *const ()) {
        let task = unsafe { &mut *(data as *mut Task) };
        task.waker = new_waker(data);
        let mut context = Context::from_waker(&task.waker);
        let p = unsafe { Pin::new_unchecked(&mut *task.f) };
        match p.poll(&mut context) {
            Poll::Pending => {}
            Poll::Ready(_) => {
                let b = unsafe { Box::from_raw(data as *mut Task) };
                let _ = unsafe { Box::from_raw(b.f) };
            }
        };
    }
}

pub struct Runtime {
    spawner: Arc<RefCell<Spawner>>,
}

struct Task<'a> {
    rt: *mut Spawner,
    waker: Waker,
    f: &'a mut dyn Future<Output = ()>,
}

impl Runtime {
    pub fn new() -> Self {
        Runtime {
            spawner: Arc::new(RefCell::new(Spawner {})),
        }
    }

    pub fn spawn<'a, T: 'a + Future<Output = ()>>(&'a self, f: T)
    where
        T::Output: std::fmt::Debug,
    {
        self.spawner.borrow_mut().spawn(f)
    }

    pub fn spawner<'a, T1, TR: 'a + Future<Output = ()>, F: 'a + Fn(T1) -> TR>(
        &'a self,
        f: &'a F,
    ) -> Box<dyn Fn(T1) + 'a> {
        let spawner = self.spawner.clone();
        Box::new(move |x| {
            spawner.borrow_mut().spawn(f(x));
        })
    }
}
