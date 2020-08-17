use std::cell::RefCell;
use std::sync::Arc;

pub struct DekkerData {
    waiting: [bool; 2],
    turn: usize,
}

pub struct DekkerLock {
    data: Arc<RefCell<DekkerData>>,
}

impl Clone for DekkerLock {
    fn clone(&self) -> Self {
        DekkerLock {
            data: self.data.clone(),
        }
    }
}

pub struct DekkerLockToken<'a> {
    l: &'a mut DekkerLock,
    id: usize,
}

impl<'a> Drop for DekkerLockToken<'a> {
    fn drop(&mut self) {
        let arc = self.l.data.clone();
        let mut data = arc.borrow_mut();
        let next_id = (self.id + 1) % 2;

        data.turn = next_id;
        data.waiting[self.id] = false;
    }
}

impl DekkerLock {
    pub fn new() -> DekkerLock {
        let c = RefCell::new(DekkerData {
            waiting: [false, false],
            turn: 0,
        });
        DekkerLock { data: Arc::new(c) }
    }
    pub fn lock(&mut self, id: usize) -> DekkerLockToken {
        let arc = self.data.clone();
        let mut data = arc.borrow_mut();

        data.waiting[id] = true;
        let next_id = (id + 1) % 2;
        while data.waiting[next_id] {
            println!("waiting {}", next_id);
            if data.turn != id {
                data.waiting[id] = false;
                while data.turn != id {
                    // // busy wait
                    println!("{}: waiting...", id);
                }
                data.waiting[id] = true;
            }
        }

        DekkerLockToken { l: self, id }
    }
}

unsafe impl Send for DekkerLock {}

#[cfg(test)]
mod tests {
    use std::sync::atomic::Ordering;

    #[tokio::test]
    async fn my_test() {
        let c = std::sync::Arc::new(std::cell::RefCell::new(std::sync::atomic::AtomicU8::new(0)));
        let c1 = c.clone();
        let c2 = c.clone();

        let l = super::DekkerLock::new();
        let mut l1 = l.clone();
        let mut l2 = l.clone();
        let p1 = std::thread::spawn(move || loop {
            let token = l1.lock(0);
            let c1 = c1.clone();
            let c1 = c1.borrow_mut();
            let old_value = c1.fetch_add(1, Ordering::SeqCst);
            if old_value == 1 {
                panic!("TWO INSIDE CRITICAL VALUE");
            }
            c1.fetch_sub(1, Ordering::SeqCst);
            drop(token);
        });
        let p2 = std::thread::spawn(move || loop {
            let token = l2.lock(1);
            let c2 = c2.borrow_mut();
            let old_value = c2.fetch_add(1, Ordering::SeqCst);
            if old_value == 1 {
                panic!("TWO INSIDE CRITICAL VALUE");
            }
            c2.fetch_sub(1, Ordering::SeqCst);
            drop(token);
        });

        p1.join().unwrap();
        p2.join().unwrap();
    }
}
