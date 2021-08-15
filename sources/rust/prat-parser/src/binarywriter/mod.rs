pub mod asm;
pub mod elf;
pub mod os;

use std::collections::HashMap;
use std::{fmt, io::Write};

use bytes::BufMut;
use fmt::Debug;

#[extension_trait::extension_trait(pub)]
impl PutUTF8 for Vec<u8> {
    fn put_utf8(&mut self, txt: &str) {
        let bytes = txt.as_bytes();
        self.put_slice(bytes);
    }
}

pub struct BinaryWriter {
    pos: usize,
    data: Vec<ByteValue>,
    vars: HashMap<String, ByteValue>,
}
impl BinaryWriter {
    pub fn new(start: u64) -> Self {
        let mut s = Self {
            pos: 0,
            data: Vec::new(),
            vars: HashMap::new(),
        };

        s.vars.insert("$$".to_string(), ByteValue::U64(start));
        s.vars.insert("$".to_string(), ByteValue::U64(start));

        s
    }

    pub fn as_u64(&self, v: &ByteValue) -> u64 {
        match &v {
            ByteValue::U64(x) => *x,
            ByteValue::Var(size, x) => self.val_u64(x),
            ByteValue::Expr(pos, _size, f) => {
                // println!("EXPR {} {}", pos, size);
                let v = f(self, *pos as u64);
                self.as_u64(&v)
            }
            _ => panic!(format!("{:?}", v)),
        }
    }

    pub fn val_u64(&self, name: &str) -> u64 {
        let v = self.vars.get(name);
        if v.is_none() {
            println!("{:?}", name);
        }
        self.as_u64(v.unwrap())
    }

    pub fn put(&mut self, mut v: ByteValue) {
        let dd = self.val_u64("$$");
        let dollar = self.vars.get_mut("$").unwrap();
        *dollar = ByteValue::U64(dd + self.pos as u64);

        let _oldpos = dd + self.pos as u64;

        self.pos += match &mut v {
            ByteValue::U8(_x) => 1,
            ByteValue::U8Slice(x) => x.len(),
            ByteValue::U16(_x) => 2,
            ByteValue::U32(_x) => 4,
            ByteValue::U64(_x) => 8,
            ByteValue::UTF8(x) => x.as_bytes().len(),
            ByteValue::Var(size, _x) => *size / 8,
            ByteValue::Expr(pos, size, _x) => {
                *pos = dd as usize + self.pos as usize;
                *size / 8
            }
        };
        // println!("{} {:?}", oldpos, v);
        self.data.push(v);
    }

    pub fn set(&mut self, name: &str, mut v: ByteValue) {
        let dd = self.val_u64("$$") as usize;
        let dollar = self.vars.get_mut("$").unwrap();
        *dollar = ByteValue::U64((dd + self.pos) as u64);
        if let ByteValue::Expr(pos, _size, _f) = &mut v {
            *pos = dd + self.pos;
        }
        // println!("{} {:?}", (dd + self.pos) as u64, &v);
        self.vars.insert(name.to_string(), v);
    }

    pub fn debug_here(&self) {
        let d = self.val_u64("$");
        println!("{:#8x}", d);
    }

    pub fn set_as_here(&mut self, name: &str) {
        let d = self.val_u64("$");
        self.vars.insert(name.to_string(), ByteValue::U64(d));
    }

    pub fn set_as_distance(&mut self, name: &str, to: &str) {
        let to = to.to_string();
        self.set(
            name,
            ByteValue::Expr(0, 64, box move |x: &BinaryWriter, pos| {
                ByteValue::U64(pos - x.val_u64(to.as_str()))
            }),
        );
    }

    pub fn set_string_with_size(&mut self, name: &str, string: &str) {
        self.set_as_here(name);
        self.put(ByteValue::UTF8(string.to_string()));
        self.set_as_distance(&format!("{}_SIZE", name), name);
    }

    pub fn set_as_fileoffset(&mut self, name: &str) {
        let d = self.pos as u64;
        self.vars.insert(name.to_string(), ByteValue::U64(d));
    }

    fn write_to(&self, b: &ByteValue, v: &mut Vec<u8>) {
        match b {
            ByteValue::U8(x) => v.put_u8(*x),
            ByteValue::U8Slice(x) => {
                for b in x {
                    v.put_u8(*b);
                }
            }
            ByteValue::U16(x) => v.put_u16_le(*x),
            ByteValue::U32(x) => v.put_u32_le(*x),
            ByteValue::U64(x) => v.put_u64_le(*x),
            ByteValue::UTF8(x) => v.put_utf8(x),
            ByteValue::Var(size, x) => match *size {
                16 => {
                    let value = self.val_u64(x);
                    v.put_u16_le(value as u16);
                }
                32 => {
                    let value = self.val_u64(x);
                    v.put_u32_le(value as u32);
                }
                64 => {
                    let value = self.val_u64(x);
                    v.put_u64_le(value);
                }
                _ => panic!(format!("{}", size)),
            },
            ByteValue::Expr(pos, size, x) => {
                let value = x(self, *pos as u64);
                match *size {
                    16 => {
                        let value = self.as_u64(&value);
                        v.put_u16_le(value as u16);
                    }
                    64 => {
                        let value = self.as_u64(&value);
                        v.put_u64_le(value);
                    }
                    _ => panic!(""),
                }
            }
        }
    }

    pub fn as_vec(&mut self) -> Vec<u8> {
        let dd = self.val_u64("$$");
        self.vars.insert("$".to_string(), ByteValue::U64(dd));

        let mut v: Vec<u8> = Vec::new();
        for item in &self.data {
            // println!("{:?}", item);
            let dd = self.val_u64("$$");
            let dollar = self.vars.get_mut("$").unwrap();
            *dollar = ByteValue::U64(dd + v.len() as u64);

            self.write_to(&item, &mut v);
        }
        v
    }
}

pub enum ByteValue {
    U8(u8),
    U8Slice(Vec<u8>),
    U16(u16),
    U32(u32),
    U64(u64),

    UTF8(String),
    Var(usize, String),
    Expr(usize, usize, Box<dyn Fn(&BinaryWriter, u64) -> ByteValue>),
}
impl Debug for ByteValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        match &self {
            ByteValue::U8(x) => f.write_fmt(format_args!("U8({})", x)),
            ByteValue::U8Slice(_x) => f.write_fmt(format_args!("U8Slice()")),
            ByteValue::U16(x) => f.write_fmt(format_args!("U16({})", x)),
            ByteValue::U32(x) => f.write_fmt(format_args!("U32({})", x)),
            ByteValue::U64(x) => f.write_fmt(format_args!("U64({})", x)),
            ByteValue::UTF8(x) => f.write_fmt(format_args!("UTF8({})", x)),
            ByteValue::Var(_size, x) => f.write_fmt(format_args!("Var({})", x)),
            ByteValue::Expr(pos, size, _x) => {
                f.write_fmt(format_args!("Expr({:?},{:?})", pos, size))
            }
        }
    }
}
