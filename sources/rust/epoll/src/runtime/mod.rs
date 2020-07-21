use log::trace;
use std::{
    future::Future,
    pin::Pin,
    sync::{Arc, Mutex},
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

unsafe impl Send for Spawner {}
unsafe impl Sync for Spawner {}

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
                // let b = unsafe { Box::from_raw(data as *mut Task) };
                // let _ = unsafe { Box::from_raw(b.f) };
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
                // let b = unsafe { Box::from_raw(data as *mut Task) };
                // let _ = unsafe { Box::from_raw(b.f) };
            }
        };
    }
}

trait Runnable {
    fn run(self, spawner: &mut Spawner);
}

struct SpawnerData(Box<dyn FnOnce(&mut Spawner)>);

impl<'a> Runnable for SpawnerData {
    fn run(self, spawner: &mut Spawner) {
        self.0(spawner)
    }
}

unsafe impl<'a> Send for SpawnerData {}

pub struct Runtime {
    spawner: Arc<Mutex<Spawner>>,
    threads: Vec<std::thread::JoinHandle<()>>,
    sender: crossbeam::channel::Sender<SpawnerData>,
    receiver: crossbeam::channel::Receiver<SpawnerData>,
}

struct Task<'a> {
    rt: *mut Spawner,
    waker: Waker,
    f: &'a mut dyn Future<Output = ()>,
}

impl Runtime {
    pub fn new() -> Self {
        let (s, r) = crossbeam::channel::unbounded();

        Runtime {
            spawner: Arc::new(Mutex::new(Spawner {})),
            threads: Vec::new(),
            sender: s,
            receiver: r,
        }
    }

    pub fn increase_threads(&mut self, delta: u8) -> std::io::Result<()> {
        for tid in 0..delta {
            let r = self.receiver.clone();
            let spawner = self.spawner.clone();
            let f = move || {
                trace!(target:"Runtime", "Thread {} Started", tid);
                loop {
                    match r.recv() {
                        Ok(msg) => {
                            trace!(target:"Runtime", "Thread {}", tid);
                            let mut guard = spawner.lock().unwrap();
                            msg.run(&mut guard);
                        }
                        Err(_) => break,
                    }
                }
                trace!(target:"Runtime", "Thread {} stopped", tid);
            };
            let j = std::thread::Builder::new()
                .name(format!("Runtime #{}", 1))
                .spawn(f)?;
            self.threads.push(j);
        }

        Ok(())
    }

    pub fn spawn<'a, T: 'a + Future<Output = ()>>(&'a self, f: T)
    where
        T::Output: std::fmt::Debug,
    {
        self.spawner.lock().unwrap().spawn(f)
    }

    pub fn spawner<
        T1: 'static,
        TR: 'static + Future<Output = ()> + Send + Sync,
        F: Fn(T1) -> TR,
    >(
        &self,
        f: &'static F,
    ) -> Box<dyn Fn(T1) + 'static> {
        let s = self.sender.clone();
        Box::new(move |x| {
            let msg = SpawnerData(Box::new(move |s| {
                let future = f(x);
                s.spawn(future);
            }));
            s.send(msg).unwrap();
        })
    }
}
