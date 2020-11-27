use std::collections::*;
use std::sync::mpsc::*;

pub enum Flush
{
    FileSync,
    SystemSync,
    None
}

pub struct AppendCommand
{
    full_path: String,
    buffer: Vec<u8>,
    flush: Flush,
    response: Option<Sender<()>>,
}

pub struct FileSyncCommand
{
    full_path: String,
    response: Option<Sender<()>>,
}

pub enum FileOperation
{
    Kill(Option<Sender<()>>),
    // Open(String, Option<Sender<()>>),
    // Close(String, Option<Sender<()>>),
    Append(AppendCommand),
    FileSync(FileSyncCommand)
}

pub struct FileOperationResponse<T>(Receiver<T>);
impl<T> FileOperationResponse<T>
{
    pub fn wait(self, timeout: std::time::Duration) -> T
    {
        self.0.recv_timeout(timeout).unwrap()
    }
}

#[derive(Clone)]
pub struct FileOperationSender(Sender<FileOperation>);
impl FileOperationSender
{
    pub fn kill(&mut self) -> FileOperationResponse<()>
    {
        let (s, r) = channel::<()>();
        self.0.send(FileOperation::Kill(Some(s))).unwrap();
        FileOperationResponse(r)
    }

    pub fn append(&mut self, full_path: &str, buffer: Vec<u8>, flush: Flush) -> FileOperationResponse<()>
    {
        let (s, r) = channel::<()>();
        self.0.send(
            FileOperation::Append(
                AppendCommand
                {
                    full_path:full_path.to_string(),
                    buffer,
                    flush,
                    response: Some(s)
                })
        ).unwrap();
        FileOperationResponse(r)
    }

    pub fn sync(&mut self, full_path: &str) -> FileOperationResponse<()>
    {
        let (s, r) = channel::<()>();
        self.0.send(
            FileOperation::FileSync(
                FileSyncCommand
                {
                    full_path:full_path.to_string(),
                    response: Some(s)
                })
        ).unwrap();
        FileOperationResponse(r)
    }
}

pub struct FileMgr {
    channel: Receiver<FileOperation>,

    internal: FileOperationSender,
    files: std::collections::HashMap<String, std::fs::File>
}

impl FileMgr {
    pub fn new() -> (Self, FileOperationSender) {
        let (s, r) = channel::<FileOperation>();
        let s = FileOperationSender(s);
        (Self { channel: r, files: HashMap::new(), internal: s.clone() }, s)
    }

    pub fn start_new_thread(self) -> FileOperationSender {
        let toreturn = self.internal.clone();

        // Sync thread
        let (dfs, dfr) = channel::<String>();
        
        std::thread::spawn({
        let mut internal = self.internal.clone();
        move || {
            loop {
                let full_path = dfr.recv().unwrap();
                std::thread::sleep(std::time::Duration::from_millis(100));
                internal.sync(&full_path);
            }
        }});

        // Worker Thread
        std::thread::spawn(move || {
            use std::collections::hash_map::*;
            use std::io::*;

            let mut me = self;
            let s = loop {
                let msg = me.channel.recv().unwrap();
                match msg 
                {
                    FileOperation::Kill(s) => break s,
                    FileOperation::Append(AppendCommand {full_path, buffer, response, flush}) => {
                        let fp = full_path.clone();
                        let f = match me.files.entry(full_path)
                        {
                            Entry::Occupied(e) => e.into_mut(),
                            Entry::Vacant(e) => {
                                let f = std::fs::File::with_options()
                                    .read(true)
                                    .append(true)
                                    .create(true)
                                    .open(e.key())
                                    .unwrap();
                                e.insert(f)
                            }
                        };

                        f.seek(SeekFrom::End(0)).unwrap();
                        f.write_all(&buffer).unwrap();

                        match flush {
                            Flush::FileSync => f.sync_all().unwrap(),
                            Flush::SystemSync => nix::unistd::sync(),
                            Flush::None => {}                                    
                        }

                        if let Some(r) = response {
                            let _ = r.send(());
                        }

                        dfs.send(fp).unwrap();
                    },
                    FileOperation::FileSync(FileSyncCommand{full_path, response}) => {
                        match me.files.entry(full_path)
                        {
                            Entry::Occupied(mut e) => e.get_mut().sync_all().unwrap(),
                            Entry::Vacant(_) => {}
                        };

                        if let Some(r) = response {
                            let _ = r.send(());
                        }
                    }
                }
            };

            drop(me);
            if let Some(r) = s {
                let _ = r.send(());
            }
        });

        toreturn
    }
}

impl Drop for FileMgr
{
    fn drop(&mut self) {
        for (_, f) in &self.files
        {   
            let _ = f.sync_all();
            drop(f);
        }
    }
}
