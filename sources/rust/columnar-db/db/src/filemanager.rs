use crate::threadpool::*;
use crossbeam_channel::*;

pub enum FileManagerRequests {
    ReadAll {
        file: String,
        sender: Sender<Buffer>,
    },
}

pub struct ReadAllResult {}

pub enum FileManagerRequestsResponses {
    Ok,
    ReadAllResult(ReadAllResult),
}

pub struct FileManager<'a> {
    dispatcher: TypedThreadDispatcher<'a, FileManagerRequests, FileManagerRequestsResponses>,
}

pub struct Buffer {
    data: Vec<u8>,
    size: usize,
}

impl Buffer {
    pub fn new() -> Self {
        Self {
            data: Vec::new(),
            size: 0,
        }
    }

    pub fn as_u8(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data.as_ptr(), self.size) }
    }
}

pub struct TempFile {
    pub path: std::path::PathBuf,
}

impl TempFile {
    pub fn all_equal(path: &str, data: u8) -> std::io::Result<Self> {
        use std::io::*;
        let _ = std::fs::remove_file(&path);
        let mut f = std::fs::File::create(&path)?;

        let data = vec![data; 16 * 1024];
        f.write_all(&data)?;
        f.sync_all()?;

        Ok(Self { path: path.into() })
    }

    pub fn with_callback<F>(path: &str, f: F) -> std::io::Result<Self>
    where
        F: Fn(usize) -> u8,
    {
        use std::io::*;
        let _ = std::fs::remove_file(&path);
        let mut file = std::fs::File::create(&path)?;

        let mut data = vec![0u8; 16 * 1024];

        for (i, v) in data.iter_mut().enumerate() {
            *v = f(i);
        }

        file.write_all(&data)?;
        file.sync_all()?;

        Ok(Self { path: path.into() })
    }

    pub fn random(path: &str) -> std::io::Result<Self> {
        use std::io::*;
        let _ = std::fs::remove_file(&path);
        let mut file = std::fs::File::create(&path)?;

        let mut data = vec![0u8; 16 * 1024];
        for v in data.iter_mut() {
            *v = rand::random::<u8>()
        }

        file.write_all(&data)?;
        file.sync_all()?;

        Ok(Self { path: path.into() })
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(self.path.as_path());
    }
}

pub fn handle_read_all(file: &str, sender: &Sender<Buffer>) -> ReadAllResult {
    let mut path = file.to_string();
    path.push('\0');
    let r = unsafe { libc::open(path.as_ptr() as *const i8, libc::O_RDONLY) };
    if r < 0 {
        let err = errno::errno();
        eprintln!("{}", err);
    }
    let f = r;

    loop {
        let mut buffer = Buffer {
            data: vec![0; 4 * 1024],
            size: 4 * 1024,
        };

        buffer.size = unsafe {
            let r = libc::read(
                f,
                buffer.data.as_mut_ptr() as *mut libc::c_void,
                buffer.size,
            );
            if r == 0 {
                break;
            }
            if r < 0 {
                let err = errno::errno();
                eprintln!("{}", err);
                break;
            }
            r as usize
        };
        let _ = sender.send(buffer);
    }

    ReadAllResult {}
}

impl<'a> FileManager<'a> {
    pub fn new(pool: &mut Threadpool<'a>) -> Self {
        let dispatcher = pool.new_dispatcher(move |request| match request {
            FileManagerRequests::ReadAll { file, sender } => {
                FileManagerRequestsResponses::ReadAllResult(handle_read_all(file, sender))
            }
        });
        Self { dispatcher }
    }

    fn send(&mut self, req: FileManagerRequests) -> RunResult<FileManagerRequestsResponses> {
        self.dispatcher.send(req)
    }

    pub fn read_all(
        &mut self,
        file: &str,
        sender: Sender<Buffer>,
    ) -> std::result::Result<
        ReceiverFutureMap<FileManagerRequestsResponses, ReadAllResult>,
        ThreadpoolRunError,
    > {
        let future = self
            .send(FileManagerRequests::ReadAll {
                file: file.to_string(),
                sender,
            })?
            .map(|x| {
                if let FileManagerRequestsResponses::ReadAllResult(r) = x {
                    r
                } else {
                    panic!("unexpected result")
                }
            });
        Ok(future)
    }
}

#[cfg(test)]
mod tests {
    use crate::threadpool::Threadpool;
    use crossbeam_channel::*;

    #[test]
    fn read_all() {
        let file =
            super::TempFile::all_equal(".test.read_all", 1).expect("Cannot create temo file");

        let mut pool = Threadpool::with_qty(1).expect("Cannot create Threadpool");
        let mut mgr = super::FileManager::new(&mut pool);

        let (sender, receiver) = bounded(4);

        let readl_all_result = mgr
            .read_all(file.path.to_str().unwrap(), sender)
            .expect("Cannot read file");

        for _ in 0..4 {
            let buffer = receiver
                .recv_timeout(std::time::Duration::from_secs(1))
                .expect("Read all timeout");
            testlib::assert!(buffer.data.len() == 4096);
            testlib::assert!(buffer.data.iter().all(|x| *x == 1u8));
        }

        readl_all_result
            .wait(std::time::Duration::from_secs(1))
            .expect("Read all timeout");
    }
}
