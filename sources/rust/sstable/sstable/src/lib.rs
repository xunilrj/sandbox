use byteorder::{LittleEndian, ReadBytesExt, WriteBytesExt};
use log::trace;
use std::cmp::Ordering::{Equal, Greater, Less};
use std::collections::HashMap;
use std::fmt::Debug;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom, Write};

mod readers;
use readers::SSTableDataReader;

mod writers;
use writers::{SSTableDataWriter, WriterInfo};

mod iterators;
use iterators::SSTableIterator;

mod errors;
use errors::FlushError;

pub struct SSTable {}

impl SSTable {
    pub fn write_to_file<'a, K, V, T>(
        filename: &str,
        t: &'a T,
        key_info: &WriterInfo,
        value_info: &WriterInfo,
    ) -> Result<usize, FlushError>
    where
        K: 'a + SSTableDataWriter,
        V: 'a + SSTableDataWriter,
        T: SSTableIterator<'a, K, V>,
    {
        let mut file = File::create(filename)?;
        let size = SSTable::write(&mut file, t, key_info, value_info)?;
        file.flush()?;
        Ok(size)
    }

    fn write<
        'a,
        K: 'a + SSTableDataWriter,
        V: 'a + SSTableDataWriter,
        T: SSTableIterator<'a, K, V>,
        F: Write + Seek,
    >(
        f: &mut F,
        t: &'a T,
        key_info: &WriterInfo,
        value_info: &WriterInfo,
    ) -> Result<usize, FlushError> {
        trace!("writing");
        let start_offset = f.seek(SeekFrom::Current(0))?;
        f.write_u64::<LittleEndian>(0 as u64)?;

        // Write keys and values
        // store offset for the index
        trace!("key and values");
        let mut offsets: HashMap<usize, i64> = HashMap::new();

        for (k, v) in t.iter() {
            let current_offset = f.seek(SeekFrom::Current(0))?;
            let addr = (k as *const K) as usize;
            offsets.insert(addr, current_offset as i64);

            trace!(
                "kv at {} {}",
                current_offset,
                current_offset + key_info.max_size as u64
            );

            k.write_to(f, &key_info)?;
            v.write_to(f, &value_info)?;
        }

        let index_offset = f.seek(SeekFrom::Current(0))?;
        let index_offset = index_offset - start_offset;

        // Write keys and offsets
        trace!("writing sorted key and offsets at {}", index_offset);

        let keys = t.sorted_keys();
        trace!("write len at: {}", f.seek(SeekFrom::Current(0))?);
        f.write_u64::<LittleEndian>(keys.len() as u64)?;
        trace!("#keys: {}", keys.len());

        for k in keys.iter() {
            let addr = (*k as *const K) as usize;
            let current_offset = f.seek(SeekFrom::Current(0))? as i64;
            let jmp = offsets.get(&addr).unwrap() - current_offset - (key_info.max_size as i64) - 8;

            trace!("k at {} offset {}", current_offset, jmp);
            k.write_to(f, &key_info)?;
            f.write_i64::<LittleEndian>(jmp)?;
        }

        trace!("fixing index offset");
        f.seek(SeekFrom::Start(start_offset as u64))?;
        f.write_u64::<LittleEndian>(index_offset)?;

        let end_offset = f.seek(SeekFrom::Current(0))?;
        Ok((end_offset - start_offset) as usize)
    }

    pub fn get_from_file<V, K>(
        filename: &str,
        k: &K,
        key_info: &WriterInfo,
    ) -> Result<Option<V>, FlushError>
    where
        V: Debug,
        K: Ord + Debug,
        std::fs::File: SSTableDataReader<K> + SSTableDataReader<V>,
    {
        let mut file = File::open(filename)?;
        Self::get(&mut file, k, key_info)
    }

    pub fn get<R, K, V>(file: &mut R, k: &K, key_info: &WriterInfo) -> Result<Option<V>, FlushError>
    where
        R: Read + Seek + SSTableDataReader<K> + SSTableDataReader<V>,
        K: Ord + Debug,
        V: Debug,
    {
        trace!("reading");

        let start_offset = file.seek(SeekFrom::Current(0))? as u64;

        let index_offset = file.read_u64::<LittleEndian>()? as u64;
        trace!("index offset: {}", index_offset);
        trace!("going to: {}", index_offset);

        file.seek(SeekFrom::Start(index_offset + start_offset))?;
        let len = file.read_u64::<LittleEndian>()? as u64;

        let key_size = key_info.max_size as u64;
        let tuple_size = key_size + 8;

        trace!("#keys: {}, tuple_size: {}", len, tuple_size);

        let mut idx = len / 2;
        loop {
            trace!("idx: {}", idx);
            if idx >= len {
                break;
            }

            let key_offset = start_offset + index_offset + 8;
            let key_offset = key_offset + (idx * tuple_size);
            let jump_offset = key_offset + key_info.max_size as u64;
            trace!(
                "going to: {} + {} + 8 + {} = {}",
                start_offset,
                index_offset,
                idx * tuple_size,
                key_offset
            );
            file.seek(SeekFrom::Start(key_offset as u64))?;
            let current: K = file.read_type()?;
            trace!("going to: {:?}", current);

            trace!("cmp: {:?} {:?}", current, k);
            match current.cmp(k) {
                Equal => {
                    file.seek(SeekFrom::Start(jump_offset as u64))?;
                    let jmp = file.read_i64::<LittleEndian>().unwrap() + key_info.max_size as i64;
                    trace!(
                        "jumpint: {:?} to {}",
                        jmp,
                        file.seek(SeekFrom::Current(0))? as i64 + jmp
                    );

                    file.seek(SeekFrom::Current(jmp))?;
                    let v: V = file.read_type()?;
                    trace!("value: {:?}", v);

                    return Ok(Some(v));
                }
                Greater => {
                    trace!("Greater");
                    if idx == 0 {
                        break;
                    }
                    idx = idx / 2;
                }
                Less => {
                    trace!("Less");

                    if idx == len - 1 {
                        break;
                    }
                    idx = (idx + len) / 2;
                }
            }
        }
        Ok(None)
    }
}

#[cfg(test)]
#[macro_use(quickcheck)]
extern crate quickcheck_macros;

#[cfg(test)]
mod tests {
    use crate::SSTable;
    use crate::SSTableDataReader;
    use crate::SSTableDataWriter;
    use crate::WriterInfo;
    use quickcheck::TestResult;
    use std::collections::HashMap;
    use std::io::{Cursor, Seek, SeekFrom};

    fn must_write_read<T: SSTableDataWriter + std::fmt::Debug + std::cmp::PartialEq>(
        data: T,
    ) -> bool
    where
        std::io::Cursor<std::vec::Vec<u8>>: SSTableDataReader<T>,
    {
        let v = vec![0; 100];
        let mut c = Cursor::new(v);

        let written_size = data.write_to(&mut c, &WriterInfo { max_size: 0 }).unwrap();
        assert_eq!(
            c.seek(SeekFrom::Current(0)).unwrap() as usize,
            std::mem::size_of::<T>()
        );
        assert_eq!(written_size, std::mem::size_of::<T>());
        assert_eq!(data.size(), std::mem::size_of::<T>());

        c.seek(SeekFrom::Start(0)).unwrap();
        let read_data: T = c.read_type().unwrap();
        assert_eq!(data, read_data);
        true
    }

    #[quickcheck]
    fn write_read_usize(data: usize) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_u64(data: u64) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_i64(data: i64) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_u32(data: u32) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_i32(data: i32) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_u16(data: u16) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_i16(data: i16) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_u8(data: u8) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_i8(data: i8) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_bool(data: bool) -> bool {
        must_write_read(data)
    }
    #[quickcheck]
    fn write_read_string(data: String) -> TestResult {
        println!("[{}]", data);
        let info = WriterInfo { max_size: 100 };

        if data.len() > info.max_size - 8 {
            return TestResult::discard();
        }
        let v = vec![0; 200];
        let mut c = Cursor::new(v);

        let written_size = data.write_to(&mut c, &info).unwrap();
        assert_eq!(
            c.seek(SeekFrom::Current(0)).unwrap() as usize,
            info.max_size
        );
        assert_eq!(written_size, info.max_size);
        assert_eq!(data.size(), data.len());

        c.seek(SeekFrom::Start(0)).unwrap();
        let read_data: String = c.read_type().unwrap();
        assert_eq!(data, read_data);
        TestResult::passed()
    }
    #[quickcheck]
    fn write_table_string_u64(key: String, value: u64) -> TestResult {
        println!("[{}] => [{}]", key, value);

        let max_size = 100;
        if key.len() > max_size - 8 {
            return TestResult::discard();
        }
        let v = vec![0; max_size];
        let mut c = Cursor::new(v);

        let mut hm = HashMap::new();
        hm.insert(key.clone(), value);

        let info = WriterInfo { max_size: max_size };

        SSTable::write(&mut c, &hm, &info, &info).unwrap();
        c.seek(SeekFrom::Start(0)).unwrap();
        let read_value: u64 = SSTable::get(&mut c, &key, &info).unwrap().unwrap();

        assert_eq!(read_value, value);
        TestResult::passed()
    }
}
