use std::fmt::Debug;

#[cfg(loom)]
pub(crate) use loom::sync::atomic::{AtomicPtr, Ordering};
//#[cfg(loom)]
//pub(crate) use loom::sync::Arc;

#[cfg(not(loom))]
pub(crate) use std::sync::atomic::{AtomicPtr, Ordering};
//#[cfg(not(loom))]
pub(crate) use std::sync::Arc;

use std::boxed::Box;

#[derive(Debug)]
pub struct FifoNode<T> {
    value: T,
    next: AtomicPtr<FifoNode<T>>,
}

impl<T> FifoNode<T> {
    fn unwrap(self) -> T {
        self.value
    }
}

#[derive(Debug)]
pub struct Fifo<T: Debug> {
    head: Arc<AtomicPtr<FifoNode<T>>>,
}

unsafe impl<T: Debug> Send for Fifo<T> {}
unsafe impl<T: Debug> Sync for Fifo<T> {}
impl<T: Debug> Clone for Fifo<T> {
    fn clone(&self) -> Self {
        Self {
            head: self.head.clone(),
        }
    }
}
impl<T: Debug> Default for Fifo<T> {
    fn default() -> Self {
        Self {
            head: Arc::new(AtomicPtr::default()),
        }
    }
}

impl<'a, T: Debug> Fifo<T> {
    pub fn new() -> Self {
        Default::default()
    }
    pub fn push(&self, value: T) {
        let mut next_head = Box::new(FifoNode::<T> {
            value,
            next: AtomicPtr::default(),
        });
        loop {
            let current_head = self.head.load(Ordering::Relaxed);
            next_head.next = AtomicPtr::new(current_head);
            match self.head.compare_exchange(
                current_head,
                &mut *next_head,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => {
                    Box::leak(next_head);
                    break;
                }
                Err(_) => {}
            }
        }
    }

    pub fn peek(&'a self) -> Option<&'a T> {
        let h = self.head.load(Ordering::Relaxed);
        if h.is_null() {
            None
        } else {
            let node = unsafe { &*h };
            Some(&node.value)
        }
    }

    pub fn pop(&self) -> Option<T> {
        loop {
            let current_head_ptr = self.head.load(Ordering::SeqCst);
            if current_head_ptr.is_null() {
                break None;
            } else {
                let current_head = unsafe { &*current_head_ptr };
                let current_head_next_ptr = current_head.next.load(Ordering::SeqCst);
                match self.head.compare_exchange(
                    current_head_ptr,
                    current_head_next_ptr,
                    Ordering::SeqCst,
                    Ordering::SeqCst,
                ) {
                    Ok(_) => {
                        let current_head = unsafe { Box::from_raw(current_head_ptr) };
                        break Some(current_head.unwrap());
                    }
                    Err(_) => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    #[cfg(loom)]
    fn t1push_t2pop() {
        pretty_env_logger::init();
        loom::model(|| {
            let q = crate::Fifo::<u32>::new();
            let q1 = q.clone();
            let q2 = q.clone();
            let j1 = loom::thread::spawn(move || {
                q1.push(1);
            });
            let j2 = loom::thread::spawn(move || loop {
                match q2.pop() {
                    None => {}
                    Some(v) => break v,
                }
                loom::thread::yield_now();
            });

            j1.join().unwrap();
            assert_eq!(1, j2.join().unwrap());
        });
    }

    #[test]
    fn a() {
        let a = 1;
        let b = 2;
        assert_eq!(a + b, 3);
    }
}
