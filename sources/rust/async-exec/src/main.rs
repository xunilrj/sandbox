use {
    futures::{
        future::{BoxFuture, FutureExt},
        task::{waker_ref, ArcWake},
    },
    std::{
        future::Future,
        sync::mpsc::{sync_channel, Receiver, SyncSender},
        sync::{Arc, Mutex},
        task::{Context, Poll},
        time::Duration,
    },
};

struct Task {
    future: Mutex<Option<BoxFuture<'static, ()>>>,
    task_sender: SyncSender<Arc<Task>>,
}

impl ArcWake for Task {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        println!("wake_by_ref.1");
        let cloned = arc_self.clone();
        println!("wake_by_ref.2");
        arc_self
            .task_sender
            .send(cloned)
            .expect("too many tasks queued");
        println!("wake_by_ref.3");
    }
}

struct Sender(SyncSender<BoxFuture<'static, ()>>);

struct Executor {
    task_sender: Arc<Sender>,
    ready_queue: Receiver<BoxFuture<'static, ()>>,
}

impl ArcWake for Sender {
    fn wake_by_ref(arc_self: &Arc<Self>) {
        arc_self.
    }
}

impl Executor {
    fn run(&self) {
        while let Ok(mut f) = self.ready_queue.recv() {
            let c = self.task_sender.clone();
            let waker = waker_ref(&c);
            let ctx = &mut Context::from_waker(&*waker);
            let t = f.as_mut();
            t.poll(ctx);
            // let mut future_slot = task.future.lock().unwrap();
            // if let Some(mut future) = future_slot.take() {
            //     let waker = waker_ref(&task);
            //     let context = &mut Context::from_waker(&*waker);
            //     if let Poll::Pending = future.as_mut().poll(context) {
            //         *future_slot = Some(future);
            //     }
            // }
        }
    }

    fn spawn(&self, future: impl Future<Output = ()> + 'static + Send) {
        let future = future.boxed();
        self.task_sender
            .0
            .send(future)
            .expect("too many tasks queued");
    }
}

fn new_executor_and_spawner() -> Executor {
    const MAX_QUEUED_TASKS: usize = 10_000;
    let (task_sender, ready_queue) = sync_channel(MAX_QUEUED_TASKS);
    Executor {
        task_sender: Arc::new(Sender(task_sender)),
        ready_queue,
    }
}

struct FutureReceiver<T>(Arc<Mutex<std::sync::mpsc::Receiver<T>>>, Option<T>);

impl<T: 'static + Send + Sync> Future for FutureReceiver<T> {
    type Output = T;
    fn poll(
        self: std::pin::Pin<&mut Self>,
        ctx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<T> {
        println!("FutureReceiver.poll.1");
        let ch = self.0.lock().unwrap();
        let mut iter = ch.try_iter();
        match iter.next() {
            Some(v) => std::task::Poll::Ready(v),
            None => {
                let waker = ctx.waker().clone();
                let channel = self.0.clone();
                std::thread::spawn(move || {
                    let item = channel.lock().unwrap().recv();
                    println!("received!");
                    waker.wake();
                });
                std::task::Poll::Pending
            }
        }
    }
}

fn main() {
    let (sender, receiver) = std::sync::mpsc::channel::<i32>();

    let s1 = sender.clone();
    std::thread::spawn(move || {
        std::thread::sleep(std::time::Duration::from_millis(1000));
        println!("sent!");
        s1.send(1).unwrap();
    });

    let receiver = FutureReceiver(Arc::from(Mutex::from(receiver)), None);
    //sender.send(1).unwrap();
    let f = async move {
        println!("howdy!");
        receiver.await;
        println!("done!");
    };
    let exec = new_executor_and_spawner();
    exec.spawn(f);
    exec.run();
}
