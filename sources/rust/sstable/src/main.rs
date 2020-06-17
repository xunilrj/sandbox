use byteorder::{LittleEndian, WriteBytesExt};
use std::boxed::Box;
use std::fs::File;
use std::io::prelude::Write;
use std::io::Seek;
use std::io::SeekFrom;
use thiserror::Error;

trait Pusher<T> {
    fn push(&mut self, v: T);
}

impl<T> Pusher<T> for Vec<T> {
    fn push(&mut self, v: T) {
        Vec::push(self, v);
    }
}

trait SSTableDataWriter {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize>;
}

trait SSTableIterator<'a, K: 'a + SSTableDataWriter, V: 'a + SSTableDataWriter> {
    fn iter(&'a self) -> Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a>;
    fn sorted_keys(&'a self) -> Vec<&'a K>;
    fn get(&'a self, k: &'a K) -> Option<&'a V>;
}

struct SSTable {}

#[derive(Error, Debug)]
enum FlushError {
    #[error("Error")]
    Error,
}

impl SSTable {
    pub fn write<
        'a,
        K: 'a + SSTableDataWriter,
        V: 'a + SSTableDataWriter,
        T: SSTableIterator<'a, K, V>,
    >(
        filename: &str,
        t: &'a T,
    ) -> Result<usize, FlushError> {
        let mut file = File::create(filename).or(Err(FlushError::Error))?;
        let size = SSTable::write_to_file(&mut file, t).or(Err(FlushError::Error))?;
        file.flush().or(Err(FlushError::Error))?;
        Ok(size)
    }

    fn write_to_file<
        'a,
        K: 'a + SSTableDataWriter,
        V: 'a + SSTableDataWriter,
        T: SSTableIterator<'a, K, V>,
    >(
        f: &mut File,
        t: &'a T,
    ) -> Result<usize, FlushError> {
        let start_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;

        // index table offset
        // will be fixed later
        f.write_u64::<LittleEndian>(0 as u64)
            .or(Err(FlushError::Error))?;

        // Write keys and values
        // store offset for the index
        let mut offsets: HashMap<usize, u64> = HashMap::new();

        for (k, v) in t.iter() {
            let current_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;
            let addr = (k as *const K) as usize;
            offsets.insert(addr, current_offset);

            k.write_to(f).or(Err(FlushError::Error))?;
            v.write_to(f).or(Err(FlushError::Error))?;
        }

        let index_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;

        // Write keys and offsets
        let keys = t.sorted_keys();
        for k in keys.iter() {
            k.write_to(f).or(Err(FlushError::Error))?;
            let addr = (*k as *const K) as usize;
            offsets
                .get(&addr)
                .unwrap()
                .write_to(f)
                .or(Err(FlushError::Error))?;
        }

        f.seek(SeekFrom::Current(start_offset as i64))
            .or(Err(FlushError::Error))?;
        f.write_u64::<LittleEndian>(index_offset)
            .or(Err(FlushError::Error))?;

        let end_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;
        Ok((end_offset - start_offset) as usize)
    }
}

use std::collections::HashMap;

impl<'a, K: 'a + Ord, V: 'a> SSTableIterator<'a, K, V> for HashMap<K, V>
where
    K: SSTableDataWriter + std::hash::Hash,
    V: SSTableDataWriter,
{
    fn iter(&'a self) -> Box<dyn Iterator<Item = (&'a K, &'a V)> + 'a> {
        Box::new(self.iter())
    }

    fn sorted_keys(&'a self) -> Vec<&'a K> {
        let mut keys: Vec<&'a K> = self.keys().collect();
        keys.sort_by(|a, b| (*a).cmp(*b));
        keys
    }

    fn get(&'a self, k: &'a K) -> Option<&'a V> {
        self.get(k)
    }
}

impl SSTableDataWriter for &str {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        w.write(self.as_bytes())
    }
}

impl SSTableDataWriter for i32 {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        w.write_i32::<LittleEndian>(*self)?;
        Ok(4)
    }
}

impl SSTableDataWriter for u64 {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        w.write_u64::<LittleEndian>(*self as u64)?;
        Ok(8)
    }
}

impl SSTableDataWriter for usize {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        w.write_u64::<LittleEndian>(*self as u64)?;
        Ok(8)
    }
}

fn main() {
    let mut hm = HashMap::new();
    hm.insert("Daniel", 1);
    hm.insert("Niara", 2);

    SSTable::write("sstable.db", &hm).unwrap();
}
