use byteorder::{LittleEndian, WriteBytesExt};
use std::cmp::Ordering::{Greater, Less};
use std::io::{Error, ErrorKind};

pub struct WriterInfo {
    pub max_size: usize,
}

pub trait SSTableDataWriter {
    type Item;
    fn size(&self) -> usize;
    fn write_to(&self, w: &mut impl std::io::Write, info: &WriterInfo) -> std::io::Result<usize>;
}

impl SSTableDataWriter for &str {
    type Item = String;
    fn size(&self) -> usize {
        self.as_bytes().len()
    }
    fn write_to(&self, w: &mut impl std::io::Write, info: &WriterInfo) -> std::io::Result<usize> {
        let mut max_size = info.max_size;

        let bytes = self.as_bytes();

        w.write_u64::<LittleEndian>(bytes.len() as u64)?;
        max_size -= 8;

        let cmp = bytes.len().cmp(&max_size);
        if cmp == Greater {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("Data would truncated: {} {}", bytes.len(), max_size),
            ));
        }

        w.write(bytes)?;

        if cmp == Less {
            let padding = vec![0; max_size - bytes.len()];
            w.write_all(padding.as_slice())?;
        }

        Ok(info.max_size)
    }
}

impl SSTableDataWriter for String {
    type Item = String;
    fn size(&self) -> usize {
        self.as_bytes().len()
    }
    fn write_to(&self, w: &mut impl std::io::Write, info: &WriterInfo) -> std::io::Result<usize> {
        let mut max_size = info.max_size;

        let bytes = self.as_bytes();

        w.write_u64::<LittleEndian>(bytes.len() as u64)?;
        max_size -= 8;
        let cmp = bytes.len().cmp(&max_size);
        if cmp == Greater {
            return Err(Error::new(
                ErrorKind::InvalidData,
                format!("Data would truncated: {} {}", bytes.len(), max_size),
            ));
        }
        w.write(bytes)?;

        if cmp == Less {
            let padding = vec![0; max_size - bytes.len()];
            w.write_all(padding.as_slice())?;
        }
        Ok(info.max_size)
    }
}

impl SSTableDataWriter for usize {
    type Item = usize;
    fn size(&self) -> usize {
        8
    }

    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u64::<LittleEndian>(*self as u64)?;
        Ok(8)
    }
}

impl SSTableDataWriter for i64 {
    type Item = i64;
    fn size(&self) -> usize {
        8
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u64::<LittleEndian>(*self as u64)?;
        Ok(8)
    }
}

impl SSTableDataWriter for u64 {
    type Item = u64;
    fn size(&self) -> usize {
        8
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u64::<LittleEndian>(*self as u64)?;
        Ok(8)
    }
}

impl SSTableDataWriter for i32 {
    type Item = i32;
    fn size(&self) -> usize {
        4
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_i32::<LittleEndian>(*self)?;
        Ok(4)
    }
}

impl SSTableDataWriter for u32 {
    type Item = u32;
    fn size(&self) -> usize {
        4
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u32::<LittleEndian>(*self)?;
        Ok(4)
    }
}

impl SSTableDataWriter for u16 {
    type Item = u16;
    fn size(&self) -> usize {
        2
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u16::<LittleEndian>(*self)?;
        Ok(2)
    }
}

impl SSTableDataWriter for i16 {
    type Item = i16;
    fn size(&self) -> usize {
        2
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_i16::<LittleEndian>(*self)?;
        Ok(2)
    }
}

impl SSTableDataWriter for u8 {
    type Item = u8;
    fn size(&self) -> usize {
        1
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u8(*self)?;
        Ok(1)
    }
}

impl SSTableDataWriter for i8 {
    type Item = i8;
    fn size(&self) -> usize {
        1
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_i8(*self)?;
        Ok(1)
    }
}

impl SSTableDataWriter for bool {
    type Item = bool;
    fn size(&self) -> usize {
        1
    }
    fn write_to(&self, w: &mut impl std::io::Write, _: &WriterInfo) -> std::io::Result<usize> {
        w.write_u8(if *self { 1 } else { 0 })?;
        Ok(1)
    }
}
