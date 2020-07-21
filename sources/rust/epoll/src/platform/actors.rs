use super::{epoll_create1, EpollCreateFlag, EpollEventData, EpollSocket, Errno};
use crate::platform::api::Epoll;
use crate::platform::linux_x86_64::epoll_wait;
use crate::platform::linux_x86_64::EpollEventBuilder;
use log::trace;
use std::sync::mpsc::Receiver;
use std::{task::Waker, thread::JoinHandle};

pub async fn acceptor<'a, F: Fn(EpollSocket<'a>)>(mut fd: EpollSocket<'a>, f: F) {
    loop {
        trace!(target: "Acceptor", "Waiting...");
        let (newfd, _) = fd.accept_async().await.unwrap();
        trace!(target: "Acceptor", "Accepted {:?}", newfd);
        f(newfd);
    }
}

pub fn dummy_receiver<T: 'static + Send>(r: Receiver<T>) -> JoinHandle<!> {
    std::thread::spawn(move || loop {
        let _ = r.recv().unwrap();
    })
}

pub struct EpollReactor {
    pub epoll: Epoll,
}
impl EpollReactor {
    pub fn new() -> Result<Self, Errno> {
        let epoll = Epoll(epoll_create1(EpollCreateFlag::None)?);
        Ok(Self { epoll })
    }
    pub fn spawn(&self) -> Receiver<u64> {
        let (w, r) = std::sync::mpsc::channel();
        let epfd = self.epoll.0;
        std::thread::Builder::new()
            .name("EpollReactor".to_owned())
            .spawn(move || -> Result<(), Errno> {
                trace!(target: "EpollReactor", "Start");
                loop {
                    let mut events = vec![EpollEventBuilder::uninitialized(); 100];
                    let qtd = epoll_wait(epfd, &mut events, -1).unwrap() as usize;
                    trace!(target: "EpollReactor", "epoll_wait returned [{}] events", qtd);
                    for i in 0..qtd {
                        match events[i]
                            .data_as_mut::<EpollEventData>()
                            .map(|x| x.take_ref::<Waker>())
                            .flatten()
                        {
                            Some(waker) => {
                                trace!(target: "EpollReactor", "Waking event {} @ {:X}", i, waker as *const Waker as usize);
                                waker.wake_by_ref();
                                w.send(0).unwrap();
                            }
                            None => {}
                        }
                    }
                }
            })
            .unwrap();
        r
    }
}
