struct DekkerLock {
    data: std::sync::Arc<([bool; 2], usize)>,
}

impl Clone for DekkerLock {
    fn clone(&self) -> Self {
        DekkerLock {
            data: self.data.clone(),
        }
    }
}

struct DekkerLockToken<'a> {
    l: &'a mut DekkerLock,
    id: usize,
}

impl<'a> Drop for DekkerLockToken<'a> {
    fn drop(&mut self) {
        let next_id = self.id + 1 % 2;
        self.l.turn = next_id;
        self.l.wants_to_enter[self.id] = false;
    }
}

impl DekkerLock {
    fn new() -> DekkerLock {
        DekkerLock {
            wants_to_enter: [false, false],
            turn: 0,
        }
    }
    fn lock(&mut self, id: usize) {
        self.wants_to_enter[id] = true;
        let next_id = id + 1 % 2;
        while self.wants_to_enter[next_id] {
            if self.turn != id {
                self.wants_to_enter[id] = false;
                while self.turn != id {
                    // // busy wait
                }
                self.wants_to_enter[id] = true;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    #[tokio::test]
    async fn my_test() {
        let mut l = super::DekkerLock::new();
        let p1 = async move {
            let token = l.lock(0);
            drop(token);
        };
        let p2 = async move {
            let token = l.lock(1);
            drop(token);
        };
        let h1 = tokio::spawn(p1);
        let h2 = tokio::spawn(p2);

        h1.await.unwrap();
        h2.await.unwrap();
    }
}
