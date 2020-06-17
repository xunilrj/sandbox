use byteorder::{LittleEndian, WriteBytesExt};
use std::fs::File;
use std::io::prelude::Write;
use std::io::Seek;
use std::io::SeekFrom;
use thiserror::Error;

trait SSTableDataWriter {
    fn write_to(&self, w: &mut impl std::io::Write) -> std::io::Result<usize>;
}

trait SSTableKeyWriter<'a> {
    type T;
    fn start(&'a self) -> Self::T;
    fn write(&'a self, t: &mut Self::T, w: &mut impl std::io::Write) -> std::io::Result<usize>;
}

trait SSTableValueWriter<'a> {
    type T;
    fn start(&'a self) -> Self::T;
    fn write(&'a self, t: &mut Self::T, w: &mut impl std::io::Write) -> std::io::Result<usize>;
}

struct SSTable {}

#[derive(Error, Debug)]
enum FlushError {
    #[error("Error")]
    Error,
}

impl SSTable {
    pub fn write<'a, T: SSTableKeyWriter<'a> + SSTableValueWriter<'a>>(
        filename: &str,
        t: &'a T,
    ) -> Result<(), FlushError> {
        let mut file = File::create(filename).or(Err(FlushError::Error))?;
        SSTable::write_to_file(&mut file, t).or(Err(FlushError::Error))?;
        file.flush().or(Err(FlushError::Error))?;
        Ok(())
    }

    fn write_to_file<'a, T: SSTableKeyWriter<'a> + SSTableValueWriter<'a>>(
        f: &mut File,
        t: &'a T,
    ) -> std::io::Result<usize> {
        let start_offset = f.seek(SeekFrom::Current(0))?;

        // index table offset
        // will be fixed later
        f.write_u64::<LittleEndian>(0 as u64)?;

        // Write keys and values
        // store offset for the index
        let mut offsets = Vec::new();

        let mut keys = SSTableKeyWriter::start(t);
        let mut values = SSTableValueWriter::start(t);
        loop {
            let current_offset = f.seek(SeekFrom::Current(0))?;
            offsets.push(current_offset);

            let size = SSTableKeyWriter::write(t, &mut keys, f)?;
            if size == 0 {
                break;
            }

            let size = SSTableValueWriter::write(t, &mut values, f)?;
            if size == 0 {
                break;
            }
        }

        let index_offset = f.seek(SeekFrom::Current(0))?;

        // Write keys and offsets
        let mut keys = SSTableKeyWriter::start(t);
        let mut keyoffseti = 0;
        loop {
            let size = SSTableKeyWriter::write(t, &mut keys, f)?;
            if size == 0 {
                break;
            }

            f.write_u64::<LittleEndian>(offsets[keyoffseti] as u64)?;
            keyoffseti += 1;
        }

        f.seek(SeekFrom::Current(start_offset as i64))?;
        f.write_u64::<LittleEndian>(index_offset)?;

        let end_offset = f.seek(SeekFrom::Current(0))?;
        Ok((end_offset - start_offset) as usize)
    }
}

use std::collections::HashMap;

impl<'a, K: 'a, V: 'a> SSTableKeyWriter<'a> for HashMap<K, V>
where
    K: SSTableDataWriter,
{
    type T = std::collections::hash_map::Iter<'a, K, V>;
    fn start(&'a self) -> Self::T {
        self.iter()
    }
    fn write(&'a self, iter: &mut Self::T, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        match iter.next() {
            None => Ok(0),
            Some((k, _)) => k.write_to(w),
        }
    }
}

impl<'a, K: 'a, V: 'a> SSTableValueWriter<'a> for HashMap<K, V>
where
    V: SSTableDataWriter,
{
    type T = std::collections::hash_map::Iter<'a, K, V>;
    fn start(&'a self) -> Self::T {
        self.iter()
    }
    fn write(&'a self, iter: &mut Self::T, w: &mut impl std::io::Write) -> std::io::Result<usize> {
        match iter.next() {
            None => Ok(0),
            Some((_, v)) => v.write_to(w),
        }
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

fn main() {
    let mut hm = HashMap::new();
    hm.insert("Daniel", 1);
    hm.insert("Niara", 2);

    SSTable::write("sstable.db", &hm).unwrap();
}
