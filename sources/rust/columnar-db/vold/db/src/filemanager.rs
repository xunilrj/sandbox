use std::path::Path;

use crate::channel::*;
use crate::threadpool::*;
use crossbeam_channel::*;

pub enum FileManagerRequests {
    ReadAll {
        file: String,
        sender: Sender<Message>,
    },
}

pub struct ReadAllResult {}

pub enum FileManagerRequestsResponses {
    Ok,
    ReadAllResult(ReadAllResult),
}

pub struct FileManager<'a> {
    epoll: libc::c_int,
    dispatcher: TypedThreadDispatcher<'a, FileManagerRequests, FileManagerRequestsResponses>,
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

    pub fn random(path: &str, mut size: usize) -> std::io::Result<Self> {
        use std::io::*;
        let _ = std::fs::remove_file(&path);
        let mut file = std::fs::File::create(&path)?;

        let mut data = vec![0u8; 4 * 1024];
        while size > 0 {
            for v in (4 * 1024).min(0) {
                *v = rand::random::<u8>()
            }
            size -= data.len();
            file.write_all(&data)?;
        }

        file.sync_all()?;

        Ok(Self { path: path.into() })
    }
}

impl Drop for TempFile {
    fn drop(&mut self) {
        let _ = std::fs::remove_file(self.path.as_path());
    }
}

pub fn handle_read_all<P: AsRef<Path>>(file: P, sender: &Sender<Message>) -> ReadAllResult {
    let mut path = file.as_ref().to_str().unwrap().to_string();
    path.push('\0');

    let fd = {
        let r = unsafe {
            libc::open(
                path.as_ptr() as *const i8,
                libc::O_RDONLY, /*| libc::O_NONBLOCK*/
            )
        };
        if r < 0 {
            let err = errno::errno();
            eprintln!("{}", err);
        }
        // let flags = unsafe { libc::fcntl(r, libc::F_GETFL, 0) };
        // let _rcontrol = unsafe { libc::fcntl(r, libc::F_SETFL, flags | libc::O_NONBLOCK) };
        r
    };

    // let _r = unsafe {
    //     libc::posix_fadvise(fd, 0, 0, libc::POSIX_FADV_NORMAL | libc::POSIX_FADV_NOREUSE)
    // };

    let mut offset = 0;
    loop {
        let mut buffer = Buffer::all_zero(4 * 1024);
        buffer.size = unsafe {
            let r = libc::pread(
                fd,
                buffer.data.as_mut_ptr() as *mut libc::c_void,
                buffer.size,
                offset,
            );
            if r == 0 {
                break;
            }
            if r < 0 {
                let err = errno::errno();
                eprintln!("{}", err); // TODO
                break;
            }
            r as usize
        };
        offset += buffer.size as i64;
        let _ = sender.send(Message::Buffer(buffer));
    }

    let _ = sender.send(Message::Eof);
    unsafe { libc::close(fd) };

    ReadAllResult {}
}

impl<'a> FileManager<'a> {
    pub fn new(pool: &mut Threadpool<'a>) -> Self {
        let dispatcher = pool.new_dispatcher(move |request| match request {
            FileManagerRequests::ReadAll { file, sender } => {
                FileManagerRequestsResponses::ReadAllResult(handle_read_all(file, sender))
            }
        });

        let epoll = {
            let r = unsafe { libc::epoll_create1(0) };
            if r < 0 {
                let err = errno::errno();
                eprintln!("{}", err); //TODO
            }
            r
        };

        Self { dispatcher, epoll }
    }

    fn send(&mut self, req: FileManagerRequests) -> RunResult<FileManagerRequestsResponses> {
        self.dispatcher.send(req)
    }

    pub fn read_all(
        &mut self,
        file: &str,
        sender: Sender<Message>,
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

impl<'a> Drop for FileManager<'a> {
    fn drop(&mut self) {
        if self.epoll > 0 {
            unsafe { libc::close(self.epoll) };
        }
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
            .read_all(file.path.to_str().unwrap(), 1, sender)
            .expect("Cannot read file");

        for _ in 0..4 {
            if let Ok(crate::channel::Message::Buffer(buffer, next)) =
                receiver.recv_timeout(std::time::Duration::from_secs(1))
            {
                testlib::assert!(next == 1);
                testlib::assert!(buffer.data.len() == 4096);
                testlib::assert!(buffer.data.iter().all(|x| *x == 1u8));
            }
        }

        readl_all_result
            .wait(std::time::Duration::from_secs(1))
            .expect("Read all timeout");
    }
}
