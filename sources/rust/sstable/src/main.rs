use byteorder::{LittleEndian, WriteBytesExt};
use std::fs::File;
use std::io::prelude::*;
use std::io::Seek;
use std::io::SeekFrom;
use std::result::Result;
use thiserror::Error;

#[derive(Error, Debug)]
enum KeyWriterError {
    #[error("Error")]
    Error,
}

trait KeyWriter {
    fn write_next(w: impl std::io::Write) -> Result<(), KeyWriterError>;
}

trait SSTable<'a, K, V> {
    type Iter: Iterator<Item = (&'a K, &'a V)>;

    fn items(&self) -> Self::Iter;
    fn values(&self) -> Self::Iter;
}

struct SSTableWriter {}

#[derive(Error, Debug)]
enum FlushError {
    #[error("Error")]
    Error,
}

impl SSTableWriter {
    pub fn write<'a>(filename: &str, t: &'a impl SSTable<'a>) -> Result<(), FlushError> {
        let mut file = File::create(filename).or(Err(FlushError::Error))?;
        SSTableWriter::write_to_file(&mut file, t)?;
        file.flush().or(Err(FlushError::Error))?;
        Ok(())
    }
    fn write_to_file<'a>(f: &mut File, t: &'a impl SSTable<'a>) -> Result<(), FlushError> {
        let mut jumps_offsets = Vec::new();
        let mut keyvec = Vec::new();

        let keys = t.keys();
        for k in keys {
            keyvec.push(k);
            let k = keyvec.last().ok_or(FlushError::Error)?;
            f.write(k.as_slice()).or(Err(FlushError::Error))?;

            let current_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;
            jumps_offsets.push(current_offset);

            f.write_u64::<LittleEndian>(0 as u64)
                .or(Err(FlushError::Error))?;
        }

        let mut values_offsets = Vec::new();
        let values = t.values();

        let keys = keyvec.iter();
        for (k, v) in keys.zip(values) {
            let current_offset = f.seek(SeekFrom::Current(0)).or(Err(FlushError::Error))?;
            values_offsets.push(current_offset);

            f.write(k.as_slice()).or(Err(FlushError::Error))?;
            f.write(v.as_slice()).or(Err(FlushError::Error))?;
        }

        let indices_to_fix = jumps_offsets.iter().zip(values_offsets.iter());
        for (at, value) in indices_to_fix {
            f.seek(SeekFrom::Start(*at)).or(Err(FlushError::Error))?;
            f.write_u64::<LittleEndian>(*value)
                .or(Err(FlushError::Error))?;
        }

        Ok(())
    }
}

use std::collections::HashMap;

impl<'a, K: 'a, V: 'a> SSTable<'a> for HashMap<K, V> {
    type Iter = std::iter::Map<std::collections::hash_map::Iter<'a, K, V>, fn((&K, &V)) -> Vec<u8>>;

    fn items(&'a self) -> Self::Iter {
        self.iter()
    }
}

fn main() {
    let mut hm = HashMap::new();
    hm.insert("Daniel", 1);
    hm.insert("Niara", 2);

    SSTableWriter::write("sstable.db", &hm).unwrap();
}
