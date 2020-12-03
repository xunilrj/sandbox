use byteorder::{LittleEndian, ReadBytesExt};
use log::trace;
use std::io::Read;
use std::io::Result;

pub trait SSTableDataReader<T> {
    type Item;
    fn read_type(&mut self) -> Result<T>;
}

impl<T> SSTableDataReader<usize> for T
where
    T: Read,
{
    type Item = usize;
    fn read_type(&mut self) -> Result<usize> {
        Ok(self.read_u64::<LittleEndian>()? as usize)
    }
}

impl<T> SSTableDataReader<u64> for T
where
    T: Read,
{
    type Item = u64;
    fn read_type(&mut self) -> Result<u64> {
        Ok(self.read_u64::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<i64> for T
where
    T: Read,
{
    type Item = i64;
    fn read_type(&mut self) -> Result<i64> {
        Ok(self.read_i64::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<u32> for T
where
    T: Read,
{
    type Item = u32;
    fn read_type(&mut self) -> Result<u32> {
        Ok(self.read_u32::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<i32> for T
where
    T: Read,
{
    type Item = i32;
    fn read_type(&mut self) -> Result<i32> {
        Ok(self.read_i32::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<u16> for T
where
    T: Read,
{
    type Item = u16;
    fn read_type(&mut self) -> Result<u16> {
        Ok(self.read_u16::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<i16> for T
where
    T: Read,
{
    type Item = i16;
    fn read_type(&mut self) -> Result<i16> {
        Ok(self.read_i16::<LittleEndian>()?)
    }
}

impl<T> SSTableDataReader<u8> for T
where
    T: Read,
{
    type Item = u8;
    fn read_type(&mut self) -> Result<u8> {
        Ok(self.read_u8()?)
    }
}

impl<T> SSTableDataReader<i8> for T
where
    T: Read,
{
    type Item = i8;
    fn read_type(&mut self) -> Result<i8> {
        Ok(self.read_i8()?)
    }
}

impl<T> SSTableDataReader<bool> for T
where
    T: Read,
{
    type Item = bool;
    fn read_type(&mut self) -> Result<bool> {
        Ok(self.read_u8()? == 1)
    }
}

impl<T> SSTableDataReader<String> for T
where
    T: Read,
{
    type Item = String;
    fn read_type(&mut self) -> Result<String> {
        let len = self.read_u64::<LittleEndian>()? as usize;
        trace!("string size: {}", len);
        let mut vec = vec![0; len];
        let mut slice = vec.as_mut_slice();
        self.read_exact(&mut slice)?;
        Ok(String::from_utf8(vec).unwrap())
    }
}
