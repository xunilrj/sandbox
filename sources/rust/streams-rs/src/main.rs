#![feature(with_options)]

struct MMBox<T: 'static + Sized>
{
    value: &'static mut T
}

impl<T: Sized> MMBox<T>
{
    pub fn new(f: &mut std::fs::File) -> Self
    {
        use std::ffi::c_void;
        use std::os::unix::io::AsRawFd;
        use nix::sys::mman::*;

        let size = std::mem::size_of::<T>();
        let value = unsafe {
            mmap(std::ptr::null_mut::<c_void>(),
            size,
            ProtFlags::PROT_READ | ProtFlags::PROT_WRITE,
            MapFlags::MAP_SHARED,
            f.as_raw_fd(),
            0).unwrap()
        };
        let value = unsafe {
            std::mem::transmute::<*mut c_void, &'static mut T>(value)
        };

        Self { value }
    }

    pub fn sync(&mut self) -> Result<(), ()>
    {
        use std::ffi::c_void;
        use nix::sys::mman::*;
        let size = std::mem::size_of::<T>();
        unsafe 
        {
            let r = msync(self.value as *mut T as *mut c_void, size, MsFlags::MS_SYNC);
            if r.is_err()
            {
                return Err(());
            }
        }

        Ok(())
    }
}

impl<T: Sized> std::convert::AsRef<T> for MMBox<T>
{
    fn as_ref(&self) -> &T {
        self.value
    }
}

impl<T: Sized> std::convert::AsMut<T> for MMBox<T>
{
    fn as_mut(&mut self) -> &mut T {
        self.value
    }
}

impl<T: Sized>  std::ops::Deref for MMBox<T>
{
    type Target = T;
    fn deref(&self) -> &Self::Target
    {
        self.value
    }
}

impl<T: Sized>  std::ops::DerefMut for MMBox<T>
{
    fn deref_mut(&mut self) -> &mut Self::Target
    {
        self.value
    }
}

impl<T: Sized>  Drop for MMBox<T>
{
    fn drop(&mut self) {
        use std::ffi::c_void;
        use nix::sys::mman::*;
        let size = std::mem::size_of::<T>();
        unsafe 
        {
            let _ = munmap(self.value as *mut T as *mut c_void, size);
        }
    }
}


struct FileLock
{
    path: String
}

struct FileLockGuard
{
    f: std::fs::File,
    path: String
}

impl Drop for FileLockGuard
{
    fn drop(&mut self)
    {
        use fs2::FileExt;
        self.f.unlock().unwrap();
    }
}

impl FileLock
{
    pub fn new(path: &str) -> Self
    {
        Self { path: path.to_string() }
    }

    pub fn lock(&mut self, name: &str) -> Result<FileLockGuard, std::io::Error>
    {
        use std::io::*;
        use fs2::FileExt;

        let path = format!("{}/{}.lock", self.path, name);
        let mut f = std::fs::File::with_options()
            .read(true)
            .write(true)
            .truncate(true)
            .create(true)
            .open(&path)
            .unwrap();
        
        f.lock_exclusive().unwrap();

        Ok(FileLockGuard { f, path })
    }
}

#[derive(Debug)]
#[allow(dead_code)]
enum StreamId {
    None,
    Asterisk,
    Specific(u64, u64),
    Min,
    Max,
    Last,
}

struct StreamMetadata
{
    len: u64
}

#[allow(dead_code)]
struct StreamConsumerMetadata
{
    name: [u8;128],
    pending_some: [bool;128],
    pending_offset: [u64;128],
    pending_id0: [u64;128],
    pending_id1: [u64;128],
    pending_timestamp: [u64;128],
}

#[allow(dead_code)]
struct StreamGroupMetadata
{
    name: [u8;128],    
    stream: [u8;128],
    next_read: u64,
    consumers: [StreamConsumerMetadata;16]
}

impl StreamGroupMetadata
{
    pub fn find_consumer_mut(&mut self, name: &str) -> Result<&mut StreamConsumerMetadata, u8>
    {
        let name = name.as_bytes();

        let mut candidate = None;
        for consumer in self.consumers.iter_mut() {
            if consumer.name.starts_with(name) && consumer.name[name.len()] == 0 {
                candidate = Some(consumer);
                break;
            }
            if candidate.is_none() && consumer.name[0] == 0 {
                candidate = Some(consumer);
            }
        }

        match candidate 
        {
            Some(m) => 
            {
                m.name[0..name.len()].copy_from_slice(name);
                m.name[name.len()] = 0;
                Ok(m)
            }
            _ => Err(0)
        }
    }
}

#[derive(Debug)]
struct StreamMessage
{
    size: u64,
    offset: u64,
    timestamp: u64,
    order: u64,
}

struct Streams {
    folder: String,
    last_timestamp: u64,
    order: u64,
}
impl Streams {
    pub fn new(folder: &str) -> Self {
        Self {
            folder: folder.to_string(),
            last_timestamp: 0,
            order: 0,
        }
    }

    fn path_contents(&self, stream: &str) -> String
    {
        format!("{}/{}.contents", self.folder, stream)
    }

    fn fs_content(&self, stream: &str) -> Result<std::fs::File, std::io::Error>
    {
        let fs = std::fs::File::with_options()
            .read(true)
            .append(true)
            .create(true)
            .open(self.path_contents(stream))?;
        Ok(fs)
    }

    fn zero_metadata(&mut self, f: &mut std::fs::File) -> Result<(), std::io::Error>
    {
        use std::io::*;

        let zeros = [0u8;std::mem::size_of::<StreamMetadata>()];
        f.write_all(&zeros)?;
        f.sync_data()?;
        f.seek(SeekFrom::Start(0))?;

        Ok(())
    }

    fn map_metadata(&mut self, stream: &str) -> Result<MMBox<StreamMetadata>, std::io::Error>
    {
        let path = format!("{}/{}.meta", self.folder, stream);
        let mut f = std::fs::File::with_options()
            .read(true)
            .write(true)
            .create(true)
            .open(&path)?;
            
        let p = std::path::Path::new(&path);
        if !p.exists() 
        {
            self.zero_metadata(&mut f)?;
        }
        else 
        {
            let m = p.metadata().unwrap();
            if m.len() == 0 
            {
                self.zero_metadata(&mut f)?;
            }
        }

        Ok(MMBox::new(&mut f))
    }

    fn write_message(&self, f: &mut std::fs::File, timestamp: u64, order: u64) -> Result<(), std::io::Error> 
    {
        use std::io::Write;
        use bytes::BufMut;

        let mut data: Vec<u8> = Vec::new();
        data.put_u8(1);
        data.put_u64_le(timestamp);
        data.put_u64_le(order);

        f.write_all(data.as_slice())?;

        Ok(())
    }

    fn read_message(&self, f: &mut std::fs::File) -> Result<StreamMessage, std::io::Error> 
    {
        use std::io::*;
        use bytes::*;

        let offset = f.seek(SeekFrom::Current(0)).unwrap();

        let mut data = vec![0u8; 17];
        f.read_exact(&mut data)?;

        let mut c = std::io::Cursor::new(data);
        let _ = c.get_u8();
        let msg = StreamMessage
        {
            size: 16,
            offset,
            timestamp: c.get_u64_le(),
            order: c.get_u64_le()
        };

        Ok(msg)
    }

    fn read_messages(&self, f: &mut std::fs::File, end: Option<u64>, count: Option<u64>) -> Result<Vec<StreamMessage>, std::io::Error> 
    {
        use std::io::*;

        let mut msgs = Vec::new();

        loop {
            if let Some(v) = count {
                if msgs.len() >= (v as usize) { break }
            }

            if let Some(e) = end {
                let pos = f.seek(SeekFrom::Current(0))?;
                if pos > e { break }
            }

            match self.read_message(f)
            {
                Ok(msg) => msgs.push(msg),
                Err(ref e) if e.kind()  == ErrorKind::UnexpectedEof => break,
                Err(e) => return Err(e)
            }
        }

        Ok(msgs)
    }

    // XADD mystream * sensor-id 1234 temperature 19.8
    // https://redis.io/commands/xadd
    pub fn add(&mut self, stream: &str, id: StreamId) -> Result<StreamId, std::io::Error> {
        let mut f = self.fs_content(stream)?;
       
        let id = match id {
            StreamId::Asterisk => {
                use std::time::{SystemTime, UNIX_EPOCH};

                let mut order = 0;

                let start = SystemTime::now();                
                let timestamp = start.duration_since(UNIX_EPOCH).unwrap().as_millis() as u64;
                let timestamp = self.last_timestamp.max(timestamp);

                if timestamp == self.last_timestamp {
                    self.order += 1;
                    order = self.order;
                } else {
                    self.order = 0;
                    self.last_timestamp = timestamp;
                }

                self.write_message(&mut f, timestamp, order)?;

                StreamId::Specific(timestamp, order)
            }
            _ => unimplemented!(),
        };
        f.sync_all()?;

        let mut meta = self.map_metadata(stream)?;
        meta.len += 1;
        meta.sync().unwrap();

        Ok(id)
    }

    // XLEN mystream
    #[allow(dead_code)]
    pub fn len(&mut self, stream: &str) -> Result<u64, std::io::Error>
    {
        let metadata = self.map_metadata(stream)?;
        Ok(metadata.len)
    }

    fn file_pos(&self, f: &mut std::fs::File, id: StreamId) -> Result<u64, std::io::Error>
    {
        match id
        {
            StreamId::Min => Ok(0),
            StreamId::Max => Ok(u64::MAX),
            StreamId::Specific(t, o) => {
                use std::io::*;
                f.seek(SeekFrom::Start(0))?;
                loop {
                    let pos = f.seek(SeekFrom::Current(0))?;
                    match self.read_message(f)
                    {
                        Ok(msg) => 
                        {
                            if msg.timestamp >= t && msg.order >= o { break Ok(pos); }
                        },
                        Err(ref e) if e.kind()  == ErrorKind::UnexpectedEof => break Ok(u64::MAX),
                        Err(e) => break Err(e)
                    }
                }
            }
            _ => Err(std::io::Error::new(std::io::ErrorKind::Other, ""))
        }
    }

    // XRANGE mystream - +
    #[allow(dead_code)]
    pub fn range(&mut self, stream: &str, start: StreamId, end: StreamId, count: Option<u64>) -> Result<Vec<StreamMessage>, std::io::Error>
    {
        use std::io::*;
        let mut f = self.fs_content(stream)?; 

        let s = self.file_pos(&mut f, start)?;
        let e = self.file_pos(&mut f,end)?;
       
        f.seek(SeekFrom::Start(s))?;
        let msgs = self.read_messages(&mut f, Some(e), count).unwrap();

        Ok(msgs)
    }

    fn zero_consumergroup_metadata(&mut self, f: &mut std::fs::File) -> Result<(), std::io::Error>
    {
        use std::io::*;

        let zeros = [0u8;std::mem::size_of::<StreamGroupMetadata>()];
        f.write_all(&zeros)?;
        f.sync_data()?;
        f.seek(SeekFrom::Start(0))?;

        Ok(())
    }

    fn path_meta(&self, stream: &str, name: &str) -> String
    {
        format!("{}/{}.{}.meta", self.folder, stream, name)
    }

    fn map_group_metadata(&mut self, stream: &str, name: &str) -> Result<MMBox<StreamGroupMetadata>, std::io::Error>
    {
        let path = self.path_meta(stream, name);
        let mut f = std::fs::File::with_options()
            .read(true)
            .write(true)
            .create(true)
            .open(&path)?;

        let p = std::path::Path::new(&path);
        if !p.exists() 
        {
            self.zero_consumergroup_metadata(&mut f)?;
        }
        else 
        {
            let m = p.metadata().unwrap();
            if m.len() == 0 
            {
                self.zero_consumergroup_metadata(&mut f)?;
            }
        }

        Ok(MMBox::new(&mut f))
    }

    // XGROUP CREATE mystream mygroup $
    #[allow(dead_code)]
    pub fn group_create(&mut self, stream: &str, name: &str) -> Result<bool, std::io::Error>
    {
        let mut meta = self.map_group_metadata(stream, name)?;

        let stream_bytes = stream.as_bytes();
        let name_bytes = name.as_bytes();

        meta.stream[0..stream_bytes.len()].copy_from_slice(stream_bytes);
        meta.name[0..name_bytes.len()].copy_from_slice(name_bytes);

        meta.sync().unwrap();

        Ok(true)
    }

    // XREADGROUP GROUP mygroup Alice COUNT 1 STREAMS mystream >
    pub fn group_read(&mut self, group: &str, consumer: &str, stream: &str, mkstream: bool) -> Result<Vec<StreamMessage>, std::io::Error>
    {
        use std::time::Duration;
        use std::sync::mpsc::channel;
        use notify::{Watcher, RecursiveMode, watcher};

        if mkstream {
            let _ = self.fs_content(stream).unwrap();
        }

        let mut l = FileLock::new(&self.folder);
        let guard = l.lock(group);

        // do we have messages?
        // don't need to wait
        {
            let mut f = self.fs_content(stream).unwrap();
            let mut meta = self.map_group_metadata(stream, group).unwrap();
            f.seek(SeekFrom::Start(meta.next_read)).unwrap();

            let msgs = self.read_messages(&mut f, None, None).unwrap();
            if let Some(msg) = msgs.last()
            {
                meta.next_read = msg.offset + msg.size + 1; //TODO first byte is type of message 1 = normal message
                meta.sync().unwrap();
                return Ok(msgs);
            }
        }

        // wait for messages

        let (tx, rx) = channel();
        let mut watcher = watcher(tx, Duration::from_secs(10)).unwrap();
        watcher.watch(self.path_contents(stream), RecursiveMode::NonRecursive).unwrap();
        let _ = rx.recv().unwrap();

        use std::io::*;

        let mut meta = self.map_group_metadata(stream, group).unwrap();
        
        let mut f = self.fs_content(stream).unwrap();
        f.seek(SeekFrom::Start(meta.next_read)).unwrap();

        let mut consumer = meta.find_consumer_mut(consumer).unwrap();
        
        let msgs = self.read_messages(&mut f, None, None).unwrap();

        let start = std::time::SystemTime::now();                
        let timestamp = start.duration_since(std::time::UNIX_EPOCH).unwrap().as_millis() as u64;

        let mut i = 0;
        for msg in &msgs
        {
            loop {
                if !consumer.pending_some[i] { break; }
                i += 1;
            }
            consumer.pending_some[i] = true;
            consumer.pending_offset[i] = msg.offset;
            consumer.pending_timestamp[i] = timestamp;

            consumer.pending_id0[i] = msg.timestamp;
            consumer.pending_id1[i] = msg.order;
        }

        if let Some(msg) = msgs.last()
        {
            meta.next_read = msg.offset + msg.size + 1; //TODO first byte is type of message 1 = normal message
            meta.sync().unwrap();
        }

        drop(guard);

        Ok(msgs)
    }


    // XPENDING mystream mygroup
    pub fn pending(&mut self, stream: &str, group: &str)
    -> Result<(u64, StreamId, StreamId, std::collections::BTreeMap<String, u64>), std::io::Error>
    {
        let meta = self.map_group_metadata(stream, group).unwrap();

        let mut count = 0;

        let mut mints = u64::MAX;
        let mut maxts = 0u64;

        let mut consumers: std::collections::BTreeMap<String, u64> =
            std::collections::BTreeMap::new();
        
        for c in &meta.consumers
        {
            if c.name[0] == 0 { continue; }

            let cname = unsafe {std::ffi::CStr::
            from_ptr(c.name.as_ptr() as *const std::os::raw::c_char)
                .to_str()
                .unwrap()
                .to_string()};
            for (i, is_some) in c.pending_some.iter().enumerate()
            {
                if *is_some {
                    count += 1;

                    mints = mints.min(c.pending_id0[i]);
                    maxts = maxts.max(c.pending_id0[i]);
                    consumers.entry(cname.clone())
                        .and_modify(|v| {*v += 1;})
                        .or_insert(1u64);
                }
            }
        }

        Ok((count,
            if count == 0 { StreamId::None } else { StreamId::Specific(mints,0) },
            if count == 0 { StreamId::None } else { StreamId::Specific(maxts,0) },
            consumers))
    }

    // XACK mystream mygroup 1526569495631-0
    pub fn ack(&mut self, stream: &str, group: &str, id: &StreamId)
    -> Result<u64, std::io::Error>
    {
        let mut group = self.map_group_metadata(stream, group).unwrap();

        let (ts, o) = match id {
            StreamId::Specific(ts, o) => (ts,o),
            _ => return Ok(0)
        };
        
        'all: for consumer in &mut group.consumers
        {
            for (i, is_some) in consumer.pending_some.iter_mut().enumerate()
            {
                if *is_some && consumer.pending_id0[i] == *ts && consumer.pending_id1[i] == *o {
                    *is_some = false;
                    break 'all;
                }
            }
        }

        Ok(1)
    }
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = std::env::args().collect();
    println!("{:?}", args);

    let mut x = Streams::new(".");

    if let Some(arg1) = args.get(1) {
        if arg1 == "consumer"
        {
            let stream = args.get(2).unwrap();
            let group = args.get(3).unwrap();
            let consumer = args.get(4).unwrap();
            
            loop
            {
                match x.group_read(&group, &consumer, &stream, true) {
                    Ok(msgs) => {
                        for msg in msgs {
                            let ack = x.ack(stream, group, &StreamId::Specific(msg.timestamp, msg.order));
                            println!("{:?} {:?}", msg, ack);
                        }
                    },
                    Err(e) => return Err(e)
                }
            }
        }
        else if arg1 == "producer"
        {
            let stream = args.get(2).unwrap();

            loop
            {
                std::thread::sleep(std::time::Duration::from_secs(1));
                let id = x.add(&stream, StreamId::Asterisk).unwrap();
                println!("{:?}", id);
            }
        }
        else if arg1 == "pending" 
        {
            let stream = args.get(2).unwrap();
            let group = args.get(3).unwrap();

            let r = x.pending(stream, group).unwrap();
            println!("{:?}", r.0);
            println!("{:?}", r.1);
            println!("{:?}", r.2);
            println!("{:?}", r.3);
        }    
    }


    // let stream = "room-1";

    // let mut x = Streams::new(".");
    // x.add(stream, StreamId::Asterisk)?;

    // println!("{:?}", x.len(stream));
    // let range = x.range(stream, StreamId::Min, StreamId::Max, None)?;
    // println!("{:?}", range);

    // let msg = range.iter().nth(0).unwrap();
    // println!("{:?}", msg);

    // let range = x.range(stream, 
    //     StreamId::Specific(msg.timestamp, msg.order), 
    //     StreamId::Specific(msg.timestamp, msg.order), Some(1))?;
    // println!("{:?}", range);

    // let r = x.group_create(stream, "consumer-1")?;
    // println!("{:?}", r);

    // let (tx, rx) = std::sync::mpsc::channel();
    // std::thread::spawn(move || {
    //     let stream = "room-1";
    //     let mut x = Streams::new(".");
    //     tx.send(1).unwrap();
    //     let r = x.group_read("consumer-1", "a", stream);
    //     println!("group_read: {:?}", r);
    // });
    // rx.recv().unwrap();
    // std::thread::sleep(std::time::Duration::from_secs(1));
    // x.add(stream, StreamId::Asterisk)?;

    // std::thread::sleep(std::time::Duration::from_secs(1));
    // let pending = x.pending(stream, "consumer-1").unwrap();
    // println!("pending: {:?}", pending);

    // let r = x.ack(stream, "consumer-1", &pending.1);
    // println!("ack: {:?} {:?}", &pending.1, r);

    // let pending = x.pending(stream, "consumer-1");
    // println!("pending: {:?}", pending);

    Ok(())
}
