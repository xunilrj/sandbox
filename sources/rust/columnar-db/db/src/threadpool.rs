use crossbeam_channel::*;
use std::collections::*;
use std::fs::*;
use std::io::*;

fn read_lines(path: &str) -> Option<Lines<BufReader<File>>> {
    use std::fs::*;
    use std::io::*;
    let f = File::open(path).ok()?;
    Some(BufReader::new(f).lines())
}

fn get_num_cpu() -> Option<usize> {
    let mut cpus: Option<usize> = None;

    let lines = read_lines("/proc/cpuinfo")?;
    for line in lines {
        match line {
            Ok(line) => {
                if let Some(colon_pos) = line.find(':') {
                    let (key, value) = line.split_at(colon_pos);
                    let key = key.trim();
                    let value = value.trim_matches(':').trim();

                    if key == "processor" {
                        cpus = match (value.parse::<usize>(), cpus) {
                            (Ok(a), None) => Some(a + 1),
                            (Ok(a), Some(b)) => Some((a + 1).max(b)),
                            (_, b) => b,
                        };
                    }
                }
            }
            _ => break,
        }
    }

    cpus
}

extern "C" fn thread_f(arg: *mut libc::c_void) -> *mut libc::c_void {
    let receiver = unsafe { Box::from_raw(arg as *mut Receiver<ThreadMessage>) };

    loop {
        match receiver.recv() {
            Ok(ThreadMessage::Kill(s)) => {
                let _ = s.send(());
                break;
            }
            Ok(ThreadMessage::Run(f, s)) => {
                f();
                let _ = s.send(());
            }
            Err(_) => break,
        }
    }

    std::ptr::null_mut()
}

pub struct Thread<'a> {
    sender: Sender<ThreadMessage<'a>>,
}

#[derive(Debug)]
pub enum ThreadSpawnError {
    Error,
}

#[derive(Debug)]
pub enum ThreadKillError {
    Timeout,
    ChannelAlreadyDead,
}

impl<'a> Thread<'a> {
    pub fn kill(
        &mut self,
        timeout: std::time::Duration,
    ) -> std::result::Result<(), ThreadKillError> {
        let (s, r) = bounded(1);
        self.sender
            .send(ThreadMessage::Kill(s))
            .map_err(|_| ThreadKillError::ChannelAlreadyDead)?;
        r.recv_timeout(timeout)
            .map_err(|_| ThreadKillError::Timeout)
    }
}

impl<'a> Thread<'a> {
    pub fn spawn<'b>() -> std::result::Result<Thread<'b>, ThreadSpawnError> {
        let (sender, receiver) = unbounded::<ThreadMessage>();
        Self::spawn_with_channel(sender, receiver)
    }

    pub fn spawn_with_channel<'b>(
        sender: Sender<ThreadMessage<'b>>,
        receiver: Receiver<ThreadMessage<'b>>,
    ) -> std::result::Result<Thread<'b>, ThreadSpawnError> {
        let receiver = Box::leak(Box::new(receiver));
        unsafe {
            let mut thread: libc::pthread_t = std::mem::MaybeUninit::uninit().assume_init();
            let mut attr: libc::pthread_attr_t = std::mem::MaybeUninit::uninit().assume_init();
            let r = libc::pthread_attr_init(&mut attr);
            if r != 0 {
                return Err(ThreadSpawnError::Error);
            }
            let r = libc::pthread_create(
                &mut thread,
                &mut attr,
                thread_f,
                receiver as *mut Receiver<ThreadMessage<'b>> as *mut libc::c_void,
            );
            if r != 0 {
                return Err(ThreadSpawnError::Error);
            }
        };

        Ok(Thread { sender })
    }
}
pub enum ThreadMessage<'a> {
    Kill(Sender<()>),
    Run(Box<dyn Fn() + Send + 'a>, Sender<()>),
}

pub struct Threadpool<'a> {
    threads: VecDeque<Thread<'a>>,
    sender: Sender<ThreadMessage<'a>>,
    receiver: Receiver<ThreadMessage<'a>>,
}

#[derive(Debug)]
pub enum CreateOnePerCpuError {
    CPUCountFailed,
    ThreadSpawnError(ThreadSpawnError),
}

#[derive(Debug)]
pub enum ThreadpoolRunError {
    CannotRun,
    Timeout,
}

#[derive(Debug)]
pub enum ReceiverFutureWaitError {
    Timeout,
    InvalidLock,
    NoValue,
}

pub struct ReceiverFuture<T>(Receiver<()>, std::sync::Arc<std::sync::Mutex<Option<T>>>);

pub struct ReceiverFutureMap<T, U>(ReceiverFuture<T>, Box<dyn Fn(T) -> U>);

impl<T> ReceiverFuture<T> {
    pub fn wait(
        &self,
        timeout: std::time::Duration,
    ) -> std::result::Result<T, ReceiverFutureWaitError> {
        self.0
            .recv_timeout(timeout)
            .map_err(|_| ReceiverFutureWaitError::Timeout)?;
        let mut a = self
            .1
            .lock()
            .map_err(|_| ReceiverFutureWaitError::InvalidLock)?;
        match a.take() {
            Some(v) => Ok(v),
            _ => Err(ReceiverFutureWaitError::NoValue),
        }
    }

    pub fn map<U, F: 'static + Fn(T) -> U>(self, f: F) -> ReceiverFutureMap<T, U> {
        ReceiverFutureMap(self, Box::new(f))
    }
}

impl<T, U> ReceiverFutureMap<T, U> {
    pub fn wait(
        &self,
        timeout: std::time::Duration,
    ) -> std::result::Result<T, ReceiverFutureWaitError> {
        self.0.wait(timeout)
    }
}

impl<T> std::future::Future for ReceiverFuture<T> {
    type Output = T;

    fn poll(
        self: std::pin::Pin<&mut Self>,
        _: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Self::Output> {
        match self.0.try_recv() {
            Ok(_) => {
                let l = self.1.lock().unwrap().take().unwrap(); //TODO
                std::task::Poll::Ready(l)
            }
            Err(_) => std::task::Poll::Pending,
        }
    }
}
pub type RunResult<T> = std::result::Result<ReceiverFuture<T>, ThreadpoolRunError>;

impl<'a> Threadpool<'a> {
    pub fn new() -> Self {
        let (s, r) = unbounded::<ThreadMessage<'a>>();
        Self {
            threads: VecDeque::new(),
            sender: s,
            receiver: r,
        }
    }

    pub fn with_qty(qty: usize) -> std::result::Result<Self, ThreadSpawnError> {
        let (s, r) = unbounded::<ThreadMessage<'a>>();
        let mut pool = Self {
            threads: VecDeque::new(),
            sender: s,
            receiver: r,
        };
        pool.create(qty)?;
        Ok(pool)
    }

    pub fn qty(&self) -> usize {
        self.threads.len()
    }

    pub fn create(&mut self, qty: usize) -> std::result::Result<(), ThreadSpawnError> {
        for _ in 0..qty {
            let s = self.sender.clone();
            let r = self.receiver.clone();
            let t = Thread::spawn_with_channel(s, r)?;
            self.threads.push_back(t);
        }

        Ok(())
    }

    pub fn create_one_per_cpu(&mut self) -> std::result::Result<(), CreateOnePerCpuError> {
        let cpus = get_num_cpu().ok_or(CreateOnePerCpuError::CPUCountFailed)?;
        self.create(cpus)
            .map_err(|e| CreateOnePerCpuError::ThreadSpawnError(e))?;
        Ok(())
    }

    pub fn run<F, TR>(&mut self, f: F) -> RunResult<TR>
    where
        F: 'a + Send + Fn() -> TR,
        TR: 'a + Send + Sync,
    {
        use std::sync::*;
        let storage = Arc::new(Mutex::new(None));
        let storage_clone = storage.clone();

        let (sender, receiver) = bounded(1);
        self.sender
            .send(ThreadMessage::Run(
                Box::new(move || {
                    let r = f();
                    if let Ok(mut l) = storage_clone.lock() {
                        *l = Some(r);
                    }
                }),
                sender,
            ))
            .map_err(|_| ThreadpoolRunError::CannotRun)?;
        Ok(ReceiverFuture(receiver, storage))
    }

    pub fn sender(&mut self) -> Sender<ThreadMessage<'a>> {
        self.sender.clone()
    }

    pub fn new_dispatcher<F, TReq, TRes>(&mut self, f: F) -> TypedThreadDispatcher<'a, TReq, TRes>
    where
        F: 'a + Fn(&TReq) -> TRes + Sync + Send,
        TReq: 'a + Send + Sync,
        TRes: 'a + Send + Sync,
    {
        TypedThreadDispatcher {
            sender: self.sender.clone(),
            f: std::sync::Arc::new(Box::new(f)),
        }
    }
}

impl<'a> Drop for Threadpool<'a> {
    fn drop(&mut self) {
        for t in &mut self.threads {
            let _ = t.kill(std::time::Duration::from_secs(1));
        }
    }
}

pub struct TypedThreadDispatcher<'a, TReq: 'a + Send + Sync, TRes: 'a + Send + Sync> {
    sender: Sender<ThreadMessage<'a>>,
    f: std::sync::Arc<Box<dyn Fn(&TReq) -> TRes + Sync + Send + 'a>>,
}

impl<'a, TReq: 'a + Send + Sync, TRes: 'a + Send + Sync> TypedThreadDispatcher<'a, TReq, TRes> {
    pub fn send(&mut self, message: TReq) -> RunResult<TRes> {
        let f = self.f.clone();

        use std::sync::*;
        let storage = Arc::new(Mutex::new(None));
        let storage_clone = storage.clone();

        let (sender, receiver) = bounded(1);
        self.sender
            .send(ThreadMessage::Run(
                Box::new(move || {
                    let r = f(&message);
                    if let Ok(mut l) = storage_clone.lock() {
                        *l = Some(r);
                    }
                }),
                sender,
            ))
            .map_err(|_| ThreadpoolRunError::CannotRun)?;
        Ok(ReceiverFuture(receiver, storage))
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn get_num_cpu() {
        let cpus = super::get_num_cpu().expect("Cannot get CPU count");
        assert!(cpus > 0);
    }

    #[test]
    fn thread_pool_one_per_cpu() {
        let mut pool = super::Threadpool::new();
        let _ = pool.create_one_per_cpu();

        let cpus = super::get_num_cpu().expect("Cannot get CPU count");
        testlib::assert!(pool.qty() == cpus);
    }

    #[test]
    fn thread_pool_one_must_run_code() {
        let mut pool = super::Threadpool::with_qty(1).expect("Cannot create Threadpool");

        pool.run(move || {})
            .expect("error on Run")
            .wait(std::time::Duration::from_secs(1))
            .expect("run timeout");
    }

    #[test]
    fn thread_pool_must_accept_all_lambdas() {
        use std::sync::*;
        let data1 = Arc::new(Mutex::new(0u8));
        let data2 = Arc::new(Mutex::new(0u8));

        {
            let mut pool = super::Threadpool::with_qty(1).expect("Cannot create Threadpool");

            fn normal_function() {}
            let _ = pool
                .run(normal_function)
                .expect("error on Run")
                .wait(std::time::Duration::from_secs(1))
                .expect("run timeout");

            let no_capture = || {};
            let _ = pool
                .run(no_capture)
                .expect("error on Run")
                .wait(std::time::Duration::from_secs(1))
                .expect("run timeout");

            let _ = pool
                .run(|| {
                    let mut l = data1.lock().expect("Cannot get lock");
                    *l += 1;
                })
                .expect("error on Run")
                .wait(std::time::Duration::from_secs(1))
                .expect("run timeout");

            let _ = pool
                .run(move || {
                    let mut l = data2.lock().expect("Cannot get lock");
                    *l += 1;
                })
                .expect("error on Run")
                .wait(std::time::Duration::from_secs(1))
                .expect("run timeout");
        }
    }

    #[test]
    fn thread_pool_work_with_typed_messages() {
        enum Request<'a> {
            DoA,
            DoB(String),
            DoC(&'a str),
        };
        #[derive(Debug)]
        enum Response<'a> {
            DoA,
            DoB(String),
            DoC(&'a str),
        }

        let mut pool = super::Threadpool::with_qty(1).expect("Cannot create Threadpool");
        let mut pool = pool.new_dispatcher(move |msg: &Request| match msg {
            Request::DoA => Response::DoA,
            Request::DoB(x) => Response::DoB(x.clone()),
            Request::DoC(x) => Response::DoC(x),
        });

        // Now we send the typed messages
        // and wait for their acks

        let r = pool
            .send(Request::DoA)
            .expect("Cannot send")
            .wait(std::time::Duration::from_secs(1))
            .expect("Receive timeout");
        testlib::assert!(r, Response::DoA);

        let r = pool
            .send(Request::DoB("Test".to_string()))
            .expect("Cannot send")
            .wait(std::time::Duration::from_secs(1))
            .expect("Receive timeout");
        testlib::assert!(r, Response::DoB(x) if x == "Test");

        let r = pool
            .send(Request::DoC("Test"))
            .expect("Cannot send")
            .wait(std::time::Duration::from_secs(1))
            .expect("Receive timeout");
        testlib::assert!(r, Response::DoC(x) if x == "Test");
    }
}
