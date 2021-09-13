#![allow(dead_code)]
use std::io::Read;

use json::JsonValue;
use structopt::StructOpt;

#[derive(StructOpt, Debug)]
#[structopt(about = "Telltale Games D3DMesh converter")]
enum Args {
    Convert {
        #[structopt(short, long)]
        path: String,
        #[structopt(long)]
        pretty_print: bool,
        #[structopt(long)]
        buffer_as_base64: bool,
    },
}

#[inline(always)]
fn parse_n_bytes(input: &[u8], n: usize) -> nom::IResult<&[u8], &[u8]> {
    nom::bytes::complete::take(n)(input)
}

#[inline(always)]
fn parse_le_u16(input: &[u8]) -> nom::IResult<&[u8], u16> {
    nom::number::complete::le_u16(input)
}

#[inline(always)]
fn parse_le_u32(input: &[u8]) -> nom::IResult<&[u8], u32> {
    nom::number::complete::le_u32(input)
}

#[inline(always)]
fn parse_le_u64(input: &[u8]) -> nom::IResult<&[u8], u64> {
    nom::number::complete::le_u64(input)
}

#[inline(always)]
fn parse_le_f32(input: &[u8]) -> nom::IResult<&[u8], f32> {
    nom::number::complete::le_f32(input)
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
fn parse_string(input: &[u8], size: usize) -> nom::IResult<&[u8], &str> {
    let (input, data) = nom::bytes::complete::take(size)(input)?;
    let s = std::str::from_utf8(data).unwrap();
    Ok((input, s))
}

#[inline(always)]
fn parse_u8_slice(input: &[u8], size: usize) -> nom::IResult<&[u8], &[u8]> {
    let (input, data) = nom::bytes::complete::take(size)(input)?;
    Ok((input, data))
}

fn whats_next(input: &[u8]) {
    println!("Options:");
    println!(
        "    parse_le_u16: {:?} or {:?} or {:?} or {:?} or {:?}",
        parse_le_u16(input).unwrap().1,
        parse_le_u16(&input[1..]).unwrap().1,
        parse_le_u16(&input[2..]).unwrap().1,
        parse_le_u16(&input[3..]).unwrap().1,
        parse_le_u16(&input[4..]).unwrap().1,
    );
    println!(
        "    parse_le_u32: {:?} or {:?} or {:?} or {:?} or {:?}",
        parse_le_u32(input).unwrap().1,
        parse_le_u32(&input[1..]).unwrap().1,
        parse_le_u32(&input[2..]).unwrap().1,
        parse_le_u32(&input[3..]).unwrap().1,
        parse_le_u32(&input[4..]).unwrap().1,
    );
    println!(
        "    parse_le_f32: {:?} or {:?} or {:?} or {:?} or {:?}",
        parse_le_f32(input).unwrap().1,
        parse_le_f32(&input[1..]).unwrap().1,
        parse_le_f32(&input[2..]).unwrap().1,
        parse_le_f32(&input[3..]).unwrap().1,
        parse_le_f32(&input[4..]).unwrap().1,
    );
}

fn find_str(input: &[u8], size: usize) {
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

fn find_u32(input: &[u8]) {
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

fn find_f32(input: &[u8]) {
    println!("Trying to find a f32:");
    for i in 0..128 {
        let input = &input[i..];
        let (input, a) = parse_le_f32(input).unwrap();
        let (input, b) = parse_le_f32(input).unwrap();
        let (input, c) = parse_le_f32(input).unwrap();
        let (_, d) = parse_le_f32(input).unwrap();
        println!("    {}: {:40.3},{:40.3},{:40.3},{:40.3},", i, a, b, c, d);
    }
}

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
pub enum ParseD3dMeshError {
    Unkown,
}

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
    slice: &'a [u8],
    qty: usize,
}

impl<'a> NomSlice<'a> {
    pub fn new(slice: &'a [u8]) -> Self {
        Self { slice, qty: 0 }
    }

    #[inline(always)]
    pub fn parse_n_bytes(&mut self, n: usize) -> &[u8] {
        let (i, data) = parse_n_bytes(self.slice, n).unwrap();
        self.slice = i;
        self.qty += n;
        data
    }

    #[inline(always)]
    pub fn parse_le_u16(&mut self) -> u16 {
        let (i, data) = parse_le_u16(self.slice).unwrap();
        self.slice = i;
        self.qty += 2;
        data
    }

    #[inline(always)]
    pub fn parse_le_u32(&mut self) -> u32 {
        let (i, data) = parse_le_u32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;
        data
    }

    #[inline(always)]
    pub fn parse_le_u64(&mut self) -> u64 {
        let (i, data) = parse_le_u64(self.slice).unwrap();
        self.slice = i;
        self.qty += 8;
        data
    }

    #[inline(always)]
    pub fn parse_le_f32(&mut self) -> f32 {
        let (i, data) = parse_le_f32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;
        data
    }

    #[inline(always)]
    pub fn parse_length_string(&mut self) -> &str {
        let (i, data) = parse_length_string(self.slice).unwrap();
        self.slice = i;
        self.qty += 4 + data.len();
        data
    }

    #[inline(always)]
    pub fn parse_string(&mut self, size: usize) -> &str {
        let (i, data) = parse_n_bytes(self.slice, size).unwrap();
        let data = std::str::from_utf8(data).unwrap();
        self.slice = i;
        self.qty += size;
        data
    }

    #[inline(always)]
    pub fn parse_u16_slice(&mut self, n: usize) -> &[u16] {
        let bytes_size = std::mem::size_of::<u16>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;
        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u16, n) }
    }

    #[inline(always)]
    pub fn parse_f32_slice(&mut self, n: usize) -> &[f32] {
        let bytes_size = std::mem::size_of::<f32>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;
        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const f32, n) }
    }
}

fn parse_d3dmesh_buffer(input: &mut NomSlice, buffer_as_base64: bool) -> json::JsonValue {
    input.qty = 0;

    let qty = input.parse_le_u32();
    let stride = input.parse_le_u32();
    let t = input.parse_le_u32();
    let _ = input.parse_le_u32();
    let data = input.parse_f32_slice(((stride * qty) / 4) as usize);

    let data = if buffer_as_base64 {
        let data =
            unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u8, data.len() * 4) };
        JsonValue::String(base64::encode(data))
    } else {
        let data: Vec<_> = data
            .iter()
            .map(|x| JsonValue::Number(json::number::Number::from(*x)))
            .collect();
        JsonValue::Array(data)
    };

    json::object! {
        type: match t {
            1 => "position".to_string(),
            2 => "normal".to_string(),
            _ => format!("{}", t)
        },
        qty: qty,
        stride: stride,
        data: data
    }
}

fn parse_d3dmesh<S: AsRef<str>>(
    path: S,
    pretty_print: bool,
    buffer_as_base64: bool,
) -> std::result::Result<(), ParseD3dMeshError> {
    let mut json = json::object! {};

    let mut f = std::fs::File::open(path.as_ref()).unwrap();
    let bytes = {
        let mut bytes = vec![];
        f.read_to_end(&mut bytes).unwrap();
        bytes
    };
    let mut input = NomSlice::new(bytes.as_slice());

    let header_magic = input.parse_string(4);
    if header_magic != "ERTM" {
        panic!()
    }

    let param_count = input.parse_le_u32();
    let _param_hash = input.parse_n_bytes(8);

    for _ in 0..(param_count - 1) {
        let _ = input.parse_le_u32();
        let _ = input.parse_n_bytes(8);
    }

    let _ = input.parse_le_u32();
    let _header_len = input.parse_le_u32();
    let d3d_name = input.parse_length_string();

    json["name"] = JsonValue::String(d3d_name.to_string());

    let _ = input.parse_length_string();

    let minx = input.parse_le_f32();
    let miny = input.parse_le_f32();
    let minz = input.parse_le_f32();
    let maxx = input.parse_le_f32();
    let maxy = input.parse_le_f32();
    let maxz = input.parse_le_f32();
    json["bbox"] = json::object! {
        minx: minx,
        miny: miny,
        minz: minz,
        maxx: maxx,
        maxy: maxy,
        maxz: maxz,
    };

    json["meshes"] = JsonValue::Array(vec![]);

    let _header_size = input.parse_le_u32();
    let qty_meshes = input.parse_le_u32();

    for _ in 0..qty_meshes {
        let mut mesh = json::object! {};

        input.qty = 0;

        let _header_hash = input.parse_le_u64();

        let _ = input.parse_le_u32();
        let _ = input.parse_le_u64();

        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u64();
        let _ = input.parse_le_u32();

        let minx = input.parse_le_f32();
        let miny = input.parse_le_f32();
        let minz = input.parse_le_f32();
        let maxx = input.parse_le_f32();
        let maxy = input.parse_le_f32();
        let maxz = input.parse_le_f32();
        mesh["bbox"] = json::object! {
            minx: minx,
            miny: miny,
            minz: minz,
            maxx: maxx,
            maxy: maxy,
            maxz: maxz,
        };

        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();

        mesh["maps"] = JsonValue::Array(vec![]);

        let _ = input.parse_le_u32();
        let name = input.parse_length_string();
        let _ = mesh["maps"].push(json::object! {
            type: "diffuse",
            name: name
        });

        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();

        let _ = input.parse_le_u32();
        let _ = input.parse_length_string();
        let _ = input.parse_le_u32();
        let _ = input.parse_length_string();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();

        let _ = input.parse_le_u32();
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_le_u32();
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_le_u32();

        let _ = input.parse_le_u32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_u32();
        let _ = input.parse_le_f32();
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32();
        let _ = input.parse_le_f32();
        let _ = input.parse_le_u32();
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32();
        let _ = input.parse_le_f32();
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_length_string();
        let _ = input.parse_length_string();

        let _ = json["meshes"].push(mesh);
    }

    let _ = input.parse_le_u32();
    let _ = input.parse_le_u32();

    let t = input.parse_le_u32();
    if t == 60 {
        let _ = input.parse_le_u32();

        let mut qty = input.parse_le_u32();
        if qty > 0 {
            loop {
                if qty > 0 {
                    let _ = input.parse_le_u64();
                    let _ = input.parse_le_u32();
                } else {
                    break;
                }
                qty -= 1;
            }

            let _ = input.parse_le_u32();
            let _ = input.parse_le_u32();
        }
    } else {
        let _ = input.parse_le_u32();

        let _ = input.parse_le_u32();
        let _ = input.parse_le_u32();
    }

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);

    let _ = input.parse_le_u32();

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_le_u32();
    let _ = input.parse_le_u32();

    let _ = input.parse_le_u32();
    let _ = input.parse_le_u32();

    let _ = input.parse_le_u32();
    let _ = input.parse_le_u32();

    let _ = input.parse_n_bytes(1);

    let _ = input.parse_le_u32();
    let qty_indices = input.parse_le_u32();
    let _ = input.parse_le_u32();
    let _ = input.parse_le_u16();

    json["buffers"] = JsonValue::Array(vec![]);

    let buffer_size = input.parse_le_u32();
    let index_buffer = input.parse_n_bytes(buffer_size as usize);
    let index_buffer = if buffer_as_base64 {
        JsonValue::String(base64::encode(index_buffer))
    } else {
        let index_buffer = unsafe {
            std::slice::from_raw_parts(
                index_buffer.as_ptr() as *const u16,
                (buffer_size / 2) as usize,
            )
        };
        let index_buffer: Vec<_> = index_buffer
            .iter()
            .map(|x| JsonValue::Number(json::number::Number::from(*x)))
            .collect();
        JsonValue::Array(index_buffer)
    };
    let _ = json["buffers"].push(json::object! {
        type: "index",
        qty: qty_indices,
        data: index_buffer
    });

    while input.slice.len() != 0 {
        let buffer = parse_d3dmesh_buffer(&mut input, buffer_as_base64);
        let _ = json["buffers"].push(buffer);
    }

    if !pretty_print {
        println!("{}", json);
    } else {
        println!("{}", json.pretty(4));
    }

    Ok(())
}

fn main() {
    let args = Args::from_args();
    //println!("{:?}", args);

    match args {
        Args::Convert {
            path,
            pretty_print,
            buffer_as_base64,
        } => {
            parse_d3dmesh(path, pretty_print, buffer_as_base64).unwrap();
        }
    }
}
