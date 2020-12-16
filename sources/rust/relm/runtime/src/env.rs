use log::trace;
use std::boxed::*;
use std::task::*;

pub struct Env<T> {
    app: usize,
    actor: usize,
    phantom: std::marker::PhantomData<T>,
}

impl<T: crate::MyDebug> Env<T> {
    pub fn new(app: usize, actor: usize) -> Self {
        Self {
            app,
            actor,
            phantom: std::marker::PhantomData,
        }
    }

    pub async fn set_timeout(&mut self, duration: std::time::Duration) {
        trace!(target: "env", "set_timeout waiting for {}ms", duration.as_millis());
        SetTimeoutFuture(0, duration).await;
        trace!(target: "env", "set_timeout done");
    }

    pub fn send(&mut self, message: T) {
        #[cfg(feature = "derive_debug")]
        {
            trace!(target: "env", "send {:?}", message);
        }

        let messages = crate::application_get_messages(self.app, self.actor).unwrap();
        let messages = unsafe { &mut *(messages as *mut Vec<crate::MessageFactory<T>>) };

        messages.push(crate::MessageFactory::Message(message));
        let id = messages.len() - 1;

        crate::application_send(self.app, self.actor, id, 0, 0, 0, 0, 0);
    }
}

struct SetTimeoutFuture(u8, std::time::Duration);
impl std::future::Future for SetTimeoutFuture {
    type Output = ();
    fn poll(mut self: std::pin::Pin<&mut Self>, ctx: &mut Context<'_>) -> Poll<Self::Output> {
        if self.0 == 0 {
            self.0 += 1;

            let addr = ctx.waker().clone();
            let addr = Box::leak(Box::new(addr)) as *mut Waker as usize;

            unsafe {
                set_timeout(self.1.as_millis() as usize, addr);
            };

            std::task::Poll::Pending
        } else {
            std::task::Poll::Ready(())
        }
    }
}

#[cfg(target_arch = "wasm32")]
extern "C" {
    pub fn set_timeout(dur_ms: usize, handle: usize);
}

#[cfg(target_arch = "x86_64")]
unsafe fn set_timeout(dur_ms: usize, handle: usize) {}
