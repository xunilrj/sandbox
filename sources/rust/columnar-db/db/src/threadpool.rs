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

pub enum ThreadMessage {
    Kill,
    Run(Box<dyn Fn() + Send>),
}

pub struct Threadpool {
    handles: VecDeque<Option<std::thread::JoinHandle<()>>>,
    sender: Sender<ThreadMessage>,
    receiver: Receiver<ThreadMessage>,
}

pub enum CreateOnePerCpuError {
    CPUCountFailed,
}

pub enum ThreadpoolRunError {
    CannotRun,
}

impl Threadpool {
    pub fn new() -> Self {
        let (s, r) = unbounded::<ThreadMessage>();
        Self {
            handles: VecDeque::new(),
            sender: s,
            receiver: r,
        }
    }

    pub fn with_qty(qty: usize) -> Self {
        let (s, r) = unbounded::<ThreadMessage>();
        let mut pool = Self {
            handles: VecDeque::new(),
            sender: s,
            receiver: r,
        };
        pool.create(qty);
        pool
    }

    pub fn qty(&self) -> usize {
        self.handles.len()
    }

    pub fn create(&mut self, qty: usize) {
        for _ in 0..qty {
            let r = self.receiver.clone();
            let handle = std::thread::spawn({
                move || loop {
                    match r.recv() {
                        Ok(ThreadMessage::Kill) => break,
                        Ok(ThreadMessage::Run(f)) => f(),
                        Err(_) => panic!("Treat thread recv error"),
                    }
                }
            });

            self.handles.push_back(Some(handle));
        }
    }

    pub fn create_one_per_cpu(&mut self) -> std::result::Result<(), CreateOnePerCpuError> {
        let cpus = get_num_cpu().ok_or(CreateOnePerCpuError::CPUCountFailed)?;
        self.create(cpus);
        Ok(())
    }

    pub fn run<F>(&mut self, f: F) -> std::result::Result<(), ThreadpoolRunError>
    where
        F: 'static + Fn() + Send,
    {
        self.sender
            .send(ThreadMessage::Run(Box::new(f)))
            .map_err(|_| ThreadpoolRunError::CannotRun)?;
        Ok(())
    }
}

impl Drop for Threadpool {
    fn drop(&mut self) {
        for _ in 0..self.qty() {
            let _ = self.sender.send(ThreadMessage::Kill);
        }

        for h in &mut self.handles {
            if let Some(handle) = h.take() {
                let _ = handle.join();
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crossbeam_channel::*;

    #[test]
    fn get_num_cpu() {
        match super::get_num_cpu() {
            Some(cpus) => assert!(cpus > 0),
            None => panic!("Unexpected none"),
        }
    }

    #[test]
    fn thread_pool_one_per_cpu() {
        let mut pool = super::Threadpool::new();
        let _ = pool.create_one_per_cpu();

        match super::get_num_cpu() {
            Some(cpus) => {
                testlib::assert!(pool.qty() == cpus);
            }
            None => panic!("Unexpected none"),
        }
    }

    #[test]
    fn thread_pool_one_must_run_code() {
        let mut pool = super::Threadpool::with_qty(1);

        let (w, r) = bounded::<bool>(1);
        let _ = pool.run(move || {
            let _ = w.send(true);
        });

        let _ = r.recv_timeout(std::time::Duration::from_secs(1));
    }
}
