#![allow(dead_code)]
use crate::checksum_mapping::ChecksumMap;
use glam::{quat, vec3, Quat, Vec3};
use log::debug;

// use crate::checksum_mapping::ChecksumMap;

#[inline(always)]
fn parse_n_bytes(input: &[u8], n: usize) -> nom::IResult<&[u8], &[u8]> {
    nom::bytes::complete::take(n)(input)
}

#[inline(always)]
pub fn parse_u8(input: &[u8]) -> nom::IResult<&[u8], u8> {
    nom::number::complete::u8(input)
}

#[inline(always)]
#[allow(dead_code)]
fn parse_i8(input: &[u8]) -> nom::IResult<&[u8], i8> {
    nom::number::complete::i8(input)
}

#[inline(always)]
pub fn parse_le_u16(input: &[u8]) -> nom::IResult<&[u8], u16> {
    nom::number::complete::le_u16(input)
}

#[inline(always)]
#[allow(dead_code)]
pub fn parse_le_u16_as_f32(input: &[u8], min: f32, max: f32) -> nom::IResult<&[u8], f32> {
    let (input, v) = nom::number::complete::le_u16(input)?;
    Ok((input, ((v as f32 / u16::MAX as f32) * (max - min)) + min))
}

#[inline(always)]
#[allow(dead_code)]
fn parse_le_i16(input: &[u8]) -> nom::IResult<&[u8], i16> {
    nom::number::complete::le_i16(input)
}

#[inline(always)]
pub fn parse_le_u32(input: &[u8]) -> nom::IResult<&[u8], u32> {
    nom::number::complete::le_u32(input)
}

#[inline(always)]
#[allow(dead_code)]
pub fn parse_be_u32(input: &[u8]) -> nom::IResult<&[u8], u32> {
    nom::number::complete::be_u32(input)
}

#[inline(always)]
#[allow(dead_code)]
fn parse_le_i32(input: &[u8]) -> nom::IResult<&[u8], i32> {
    nom::number::complete::le_i32(input)
}

#[inline(always)]
pub fn parse_le_u64(input: &[u8]) -> nom::IResult<&[u8], u64> {
    nom::number::complete::le_u64(input)
}

#[inline(always)]
pub fn parse_le_f32(input: &[u8]) -> nom::IResult<&[u8], f32> {
    nom::number::complete::le_f32(input)
}

#[inline(always)]
pub fn parse_be_f32(input: &[u8]) -> nom::IResult<&[u8], f32> {
    nom::number::complete::be_f32(input)
}

#[inline(always)]
fn parse_length_string(input: &[u8]) -> nom::IResult<&[u8], &str> {
    let (input, data) = nom::multi::length_data(nom::number::complete::le_u32)(input)?;
    let s = if let Some((i, _)) = data.iter().enumerate().find(|x| *x.1 == 0) {
        let s = std::ffi::CStr::from_bytes_with_nul(&data[0..=i]).unwrap();
        s.to_str().unwrap()
    } else {
        unsafe { std::str::from_utf8_unchecked(data) }
    };
    Ok((input, s))
}

#[inline(always)]
#[allow(dead_code)]
fn parse_string(input: &[u8], size: usize) -> nom::IResult<&[u8], &str> {
    let (input, data) = nom::bytes::complete::take(size)(input)?;
    let s = std::str::from_utf8(data).unwrap();
    Ok((input, s))
}

#[inline(always)]
#[allow(dead_code)]
fn parse_u8_slice(input: &[u8], size: usize) -> nom::IResult<&[u8], &[u8]> {
    let (input, data) = nom::bytes::complete::take(size)(input)?;
    Ok((input, data))
}

pub fn whats_next(input: &[u8]) {
    let ahead = 128.min(input.len() - 4);

    struct Values {
        i: usize,
        a: u8,
        b: u16,
        c: u32,
        d: f32,
        // e: f32,
        s: String,
    }

    let mut out = std::io::stdout();
    let mut stream = tablestream::Stream::new(
        &mut out,
        vec![
            tablestream::col!(Values: .i).header("#"),
            tablestream::col!(Values: .a).header("U8"),
            tablestream::col!(Values: .b).header("U16"),
            tablestream::col!(Values: .c).header("U32"),
            tablestream::col!(Values: .d).header("F32 le"),
            // tablestream::col!(Values: .e).header("F32 be"),
            tablestream::col!(Values: .s).header("String"),
        ],
    );

    for i in 0..ahead {
        let (_, a) = parse_u8(&input[i..]).unwrap();
        let (_, b) = parse_le_u16(&input[i..]).unwrap();
        let (_, c) = parse_le_u32(&input[i..]).unwrap();
        let (_, d) = parse_le_f32(&input[i..]).unwrap();
        // let (_, e) = parse_be_f32(&input[i..]).unwrap();

        let mut s = match std::str::from_utf8(&input[i..]) {
            Ok(s) => s.to_string(),
            Err(e) => std::str::from_utf8(&input[i..i + e.valid_up_to()])
                .unwrap()
                .to_string(),
        };

        for (i, c) in s.char_indices() {
            if c.is_ascii_control() {
                s = s.as_str()[..i].to_string();
                break;
            }
        }

        if s.len() > 0 {
            s.push_str(&format!(" (len:{})", s.len()));
        }

        stream
            .row(Values {
                i,
                a,
                b,
                c,
                d,
                // e,
                s,
            })
            .unwrap();
    }

    stream.finish().unwrap();
}

#[allow(dead_code)]
pub fn find_str(input: &[u8], size: usize) {
    println!("Trying to find a str:");
    for i in 0..128 {
        let input = &input[i..(i + size)];
        let s = std::str::from_utf8(input);
        match s {
            Ok(s) => println!("    {}: {:?}", i, s),
            Err(_) => println!("    {}: <ERROR>", i),
        }
    }
}

#[allow(dead_code)]
fn find_u16(input: &[u8]) {
    println!("Trying to find a u16:");
    for i in 0..128 {
        let input = &input[i..];
        let (input, a) = parse_le_u16(input).unwrap();
        let (input, b) = parse_le_u16(input).unwrap();
        let (input, c) = parse_le_u16(input).unwrap();
        let (_, d) = parse_le_u16(input).unwrap();
        println!("    {}: {:40},{:40},{:40},{:40},", i, a, b, c, d);
    }
}

#[allow(dead_code)]
pub fn find_u32(input: &[u8]) {
    println!("Trying to find a u32:");
    for i in 0..128 {
        let input = &input[i..];
        let (input, a) = parse_le_u32(input).unwrap();
        let (input, b) = parse_le_u32(input).unwrap();
        let (input, c) = parse_le_u32(input).unwrap();
        let (_, d) = parse_le_u32(input).unwrap();
        println!("    {}: {:40},{:40},{:40},{:40},", i, a, b, c, d);
    }
}

#[allow(dead_code)]
pub fn find_u64(input: &[u8]) {
    println!("Trying to find a u64:");
    for i in 0..(100 * 1024) {
        let input = &input[i..];
        let (input, a) = parse_le_u64(input).unwrap();
        let (input, b) = parse_le_u64(input).unwrap();
        let (input, c) = parse_le_u64(input).unwrap();
        let (_, d) = parse_le_u64(input).unwrap();
        println!("    {}: {:40},{:40},{:40},{:40},", i, a, b, c, d);
    }
}

#[allow(dead_code)]
pub fn find_f32(input: &[u8]) {
    fn bounds(f: f32) -> f32 {
        if f > -1000.0 && f < 1000.0 {
            f
        } else {
            f32::NAN
        }
    }
    println!("Trying to find a f32:");
    for i in 0..128 {
        let input = &input[i..];
        let (input, a) = parse_le_f32(input).unwrap();
        let (input, b) = parse_le_f32(input).unwrap();
        let (input, c) = parse_le_f32(input).unwrap();
        let (_, d) = parse_le_f32(input).unwrap();
        println!(
            "    {}: {:20.3},{:20.3},{:20.3},{:20.3},",
            i,
            bounds(a),
            bounds(b),
            bounds(c),
            bounds(d)
        );
    }
}

#[allow(dead_code)]
fn find_f32_min_max(input: &[u8], min: f32, max: f32) {
    println!("Trying to find a f32:");
    for i in 0..128 {
        let input = &input[i..];
        let (input, a) = parse_le_f32(input).unwrap();
        if a < min || a > max {
            continue;
        }
        let (input, b) = parse_le_f32(input).unwrap();
        if b < min || b > max {
            continue;
        }
        let (input, c) = parse_le_f32(input).unwrap();
        if c < min || c > max {
            continue;
        }
        let (_, d) = parse_le_f32(input).unwrap();
        if d < min || d > max {
            continue;
        }
        println!("    {}: {:10.3},{:10.3},{:10.3},{:10.3},", i, a, b, c, d);
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum ParseD3dMeshError {
    Unkown,
}

#[allow(dead_code)]
fn parse_map(i: &[u8]) -> nom::IResult<&[u8], &str> {
    let (i, header_length) = parse_le_u32(i)?;
    let (i, _) = parse_n_bytes(i, header_length as usize).unwrap();

    let (i, name) = parse_length_string(i)?;
    //println!(
    //     "\theader_length: {}, name: [{}] ({})",
    //     header_length,
    //     name,
    //     name.len()
    // );

    Ok((i, name))
}

#[allow(dead_code)]
fn println_f32(value: f32) {
    println!("{:?}: {:X?}", value, value.to_le_bytes());
}

struct DebugU32(u32);
impl std::fmt::Debug for DebugU32 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.0.to_le_bytes();
        let asf32 = f32::from_le_bytes(bytes);
        f.write_fmt(format_args!("{} ({:X?}) (f32: {})", self.0, bytes, asf32))
    }
}

struct DebugU64(u64);
impl std::fmt::Debug for DebugU64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} ({:X?})", self.0, self.0.to_le_bytes()))
    }
}

pub struct NomSlice<'a> {
    pub slice: &'a [u8],
    pub qty: usize,
}

impl<'a> NomSlice<'a> {
    pub fn new(slice: &'a [u8]) -> Self {
        Self { slice, qty: 0 }
    }

    pub fn assert_eof(&self) {
        debug!("Missing: {}", self.slice.len());
        assert!(self.slice.len() == 0);
    }

    #[inline(always)]
    pub fn parse_n_bytes(&mut self, n: usize, name: &str) -> &[u8] {
        match parse_n_bytes(self.slice, n) {
            Ok((i, data)) => {
                self.slice = i;
                self.qty += n;

                let _d: Vec<_> = data.iter().collect();
                if n > 100 {
                    debug!("read {} bytes {}", n, name);
                } else {
                    debug!("read {} bytes {}: {:?}", n, name, data);
                }
                data
            }
            Err(e) => {
                panic!("parse_n_bytes: {:?}", e);
            }
        }
    }

    #[inline(always)]
    pub fn parse_le_u16(&mut self, name: &str) -> u16 {
        let (i, data) = parse_le_u16(self.slice).unwrap();
        self.slice = i;
        self.qty += 2;

        debug!("[{}] u16: {}", name, data);
        data
    }

    #[inline(always)]
    pub fn parse_flag_le_u32<T: std::fmt::Debug + From<u32>>(&mut self, name: &str) -> T {
        let (i, data) = parse_le_u32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        debug!("[{}] as u32: {:?} [{}]", name, T::from(data), data);
        T::from(data)
    }

    #[inline(always)]
    pub fn parse_le_u32(&mut self, name: &str) -> u32 {
        let (i, data) = parse_le_u32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        debug!("[{}] as u32: {}", name, data);
        data
    }

    #[inline(always)]
    pub fn parse_le_i32(&mut self, name: &str) -> i32 {
        let (i, data) = parse_le_i32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        debug!("[{}] as i32: {}", name, data);
        data
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub fn parse_be_u32(&mut self, name: &str) -> u32 {
        let (i, data) = parse_be_u32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        debug!("[{}] as u32: {}", name, data);
        data
    }

    #[inline(always)]
    pub fn parse_le_u64(&mut self, name: &str) -> u64 {
        let (i, data) = parse_le_u64(self.slice).unwrap();
        self.slice = i;
        self.qty += 8;

        debug!("[{}] as u64: {} 0x{:X?}", name, data, data);
        data
    }

    #[inline(always)]
    pub fn parse_le_u64_with_debug<F>(&mut self, name: &str, f: F) -> u64
    where
        F: FnOnce(u64) -> String,
    {
        let (i, data) = parse_le_u64(self.slice).unwrap();
        self.slice = i;
        self.qty += 8;

        let msg = f(data);
        debug!("[{}] as u64: {} 0x{:X?} - {}", name, data, data, msg);
        data
    }

    #[inline(always)]
    pub fn parse_le_f32(&mut self, name: &str) -> f32 {
        let (i, data) = parse_le_f32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        if name != "" {
            debug!("[{}] as f32: {}", name, data);
        }
        data
    }

    #[inline(always)]
    pub fn parse_length_string(&mut self, name: &str) -> &str {
        let (i, data) = parse_length_string(self.slice).unwrap();
        self.slice = i;
        self.qty += 4 + data.len();

        debug!("[{}] as string: {} {}", name, data.len(), data);
        data
    }

    #[inline(always)]
    pub fn parse_string(&mut self, size: usize, name: &str) -> &str {
        let (i, data) = parse_n_bytes(self.slice, size).unwrap();
        let data = std::str::from_utf8(data).unwrap();
        self.slice = i;
        self.qty += size;

        debug!("read {name}: {} {}", size, data);
        data
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub fn parse_length_buffer(&mut self, name: &str) -> &[u8] {
        let n = self.parse_le_u32(name);
        self.parse_n_bytes(n as usize, name)
    }

    #[inline(always)]
    pub fn parse_length1_buffer(&mut self, name: &str) -> &[u8] {
        let n = self.parse_n_bytes(1, &format!("{} length", name))[0];
        self.parse_n_bytes(n as usize, name)
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub fn parse_u16_slice(&mut self, n: usize) -> &[u16] {
        let bytes_size = std::mem::size_of::<u16>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;

        let _d: Vec<_> = data.iter().take(10).collect();
        debug!("read {} u16: {:?}", n, bytes_size);

        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u16, n) }
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub fn parse_u32_slice(&mut self, n: usize) -> &[u32] {
        let bytes_size = std::mem::size_of::<u32>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;

        let _d: Vec<_> = data.iter().take(10).collect();
        debug!("read {} u32: {:?}", n, bytes_size);

        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u32, n) }
    }

    #[inline(always)]
    pub fn parse_f32_slice(&mut self, n: usize) -> &[f32] {
        let bytes_size = std::mem::size_of::<f32>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;

        let _d: Vec<_> = data.iter().take(10).collect();
        debug!("read {} f32: {:?}", n, bytes_size);

        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const f32, n) }
    }

    // Telltale specifics
    pub fn read_ertm_magic_number(&mut self) -> bool {
        let header_magic = self.parse_string(4, "header_magic");
        header_magic == "ERTM"
    }

    pub fn read_properties(&mut self, mapping: &ChecksumMap) -> Vec<(String, u32)> {
        let qty = self.parse_le_u32("qty of sections");
        self.read_n_sections(qty as usize, mapping)
    }

    pub fn read_n_sections(&mut self, n: usize, mapping: &ChecksumMap) -> Vec<(String, u32)> {
        let mut sections = vec![];

        for _ in 0..n {
            let k = self.parse_le_u64_with_debug("Section Hash", |k| {
                mapping.get_mapping(k).unwrap_or_else(|| "?".to_string())
            });
            let k = if let Some(name) = mapping.get_mapping(k) {
                name
            } else {
                format!("{}", k)
            };
            let v = self.parse_le_u32("Section flags?");

            sections.push((k, v));
        }

        sections
    }

    pub fn read_m33<S: AsRef<str>>(&mut self, name: S) -> [f32; 9] {
        let mut m = [0.0; 9];
        for i in 0..9 {
            let v = self.parse_le_f32("");
            m[i] = v;
        }
        debug!("read matrix {}: {:?}", name.as_ref(), m);
        m
    }

    #[allow(dead_code)]
    pub fn read_m44<S: AsRef<str>>(&mut self, name: S) {
        let mut m = [0.0; 16];
        for i in 0..16 {
            m[i] = self.parse_le_f32("");
        }
        debug!("read matrix {}: {:?}", name.as_ref(), m);
    }

    pub fn read_quat<S: std::fmt::Debug>(&mut self, name: S) -> Quat {
        let mut q = [0.0; 4];
        for i in 0..4 {
            q[i] = self.parse_le_f32("");
        }

        let q = quat(q[0], q[1], q[2], q[3]);

        debug!("read {:?} as {:?} length: {}", name, q, q.length());

        q
    }

    pub fn read_vec3<S: std::fmt::Debug>(&mut self, name: S) -> Vec3 {
        let mut v = [0.0; 3];
        for i in 0..3 {
            v[i] = self.parse_le_f32("");
        }

        let v = vec3(v[0], v[1], v[2]);
        debug!("read {:?} as {:?}", name, v);
        v
    }

    pub fn read_length_transform<S: std::fmt::Debug>(&mut self, _name: S) -> (Quat, Vec3) {
        let length = self.parse_le_u32("transform length");
        assert!(length == 32);

        let q = self.read_quat("quaternion");
        let t = self.read_vec3("translation");

        (q, t)
    }
}
