use crate::indexbuffer;
use json::JsonValue;
use std::{
    io::{Read, Write},
    path::PathBuf,
    str::FromStr,
};

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

pub enum D3DBufferData {
    U16(Vec<u16>),
    F32(Vec<f32>),
}

pub struct D3DBuffer {
    r#type: String,
    qty: u32,
    stride: u32,
    data: D3DBufferData,
}

impl D3DBuffer {
    pub fn as_u16(&self) -> &Vec<u16> {
        match &self.data {
            D3DBufferData::U16(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_f32(&self) -> &Vec<f32> {
        match &self.data {
            D3DBufferData::F32(v) => v,
            _ => panic!(),
        }
    }
}

pub struct D3DMesh {
    buffers: Vec<D3DBuffer>,
}

impl D3DMesh {
    pub fn new() -> Self {
        Self { buffers: vec![] }
    }
}

fn parse_d3dmesh_buffer(
    input: &mut NomSlice,
    buffer_as_base64: bool,
) -> (D3DBuffer, json::JsonValue) {
    input.qty = 0;

    let qty = input.parse_le_u32();
    let stride = input.parse_le_u32();
    let t = input.parse_le_u32();
    let _ = input.parse_le_u32();
    let data = input.parse_f32_slice(((stride * qty) / 4) as usize);

    let t = match t {
        1 => "position".to_string(),
        2 => "normal".to_string(),
        _ => format!("{}", t),
    };
    (
        D3DBuffer {
            r#type: t.clone(),
            qty,
            stride,
            data: D3DBufferData::F32(data.to_vec()),
        },
        json::object! {
            type: t,
            qty: qty,
            stride: stride,
            data: if buffer_as_base64 {
                let data =
                    unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u8, data.len() * 4) };
                JsonValue::String(base64::encode(data))
            } else {
                let data: Vec<_> = data
                    .iter()
                    .map(|x| JsonValue::Number(json::number::Number::from(*x)))
                    .collect();
                JsonValue::Array(data)
            }
        },
    )
}

pub fn parse_d3dmesh<S: AsRef<str>>(
    path: S,
    output: Option<String>,
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
    let a = input.parse_le_u32(); //              ESP+18
    let first_index = input.parse_le_u16(); // first index? ESP+14

    json["buffers"] = JsonValue::Array(vec![]);

    let buffer_size = input.parse_le_u32();
    let index_buffer: Vec<_> = input
        .parse_n_bytes(buffer_size as usize)
        .iter()
        .map(|x| *x)
        .collect();

    let mut esp14 = first_index as u32;
    let mut esp18 = a;
    let mut esp20 = 0;
    let mut esp40 = 0;
    let mut esi1c = qty_indices;
    let mut esi20 = 2;
    let index_buffer = indexbuffer::get_index_buffer(
        index_buffer,
        &mut esp14,
        &mut esp18,
        &mut esp20,
        &mut esp40,
        &mut esi1c,
        &mut esi20,
    );
    let index_buffer = unsafe {
        std::slice::from_raw_parts(index_buffer.as_ptr() as *const u16, qty_indices as usize)
    };

    let index_buffer_json = if buffer_as_base64 {
        let bytes = unsafe {
            std::slice::from_raw_parts(index_buffer.as_ptr() as *const u8, index_buffer.len() * 2)
        };
        JsonValue::String(base64::encode(bytes))
    } else {
        let index_buffer: Vec<_> = index_buffer
            .iter()
            .map(|x| JsonValue::Number(json::number::Number::from(*x)))
            .collect();
        JsonValue::Array(index_buffer)
    };
    let _ = json["buffers"].push(json::object! {
        type: "index",
        qty: qty_indices,
        data: index_buffer_json
    });

    let mut mesh = D3DMesh::new();
    mesh.buffers.push(D3DBuffer {
        r#type: "index".to_string(),
        qty: qty_indices,
        stride: 2,
        data: D3DBufferData::U16(index_buffer.to_vec()),
    });

    while input.slice.len() != 0 {
        let (buffer, buffer_json) = parse_d3dmesh_buffer(&mut input, buffer_as_base64);
        mesh.buffers.push(buffer);
        let _ = json["buffers"].push(buffer_json);
    }

    match output {
        Some(output) => save_to(mesh, json, output, pretty_print),
        None => {
            if !pretty_print {
                println!("{}", json);
            } else {
                println!("{}", json.pretty(4));
            }
        }
    }

    Ok(())
}

fn save_to(mesh: D3DMesh, json: JsonValue, output: String, pretty_print: bool) {
    let path = PathBuf::from_str(output.as_str()).unwrap();
    let ext = path.extension().unwrap();
    if ext == "json" {
        let json = if !pretty_print {
            format!("{}", json)
        } else {
            format!("{}", json.pretty(4))
        };
        let _ = std::fs::write(path, json);
    } else if ext == "obj" {
        save_to_obj(mesh, path);
    } else {
        todo!();
    }
}

#[inline(always)]
fn obj_v(f: &mut std::fs::File, x: f32, y: f32, z: f32) {
    let _ = write!(f, "v {} {} {}\n", x, y, z);
}

#[inline(always)]
fn obj_f(f: &mut std::fs::File, a: u16, b: u16, c: u16) {
    let _ = write!(f, "f {} {} {}\n", a, b, c);
}

fn save_to_obj(mesh: D3DMesh, path: PathBuf) {
    let mut f = std::fs::File::with_options()
        .write(true)
        .create(true)
        .truncate(true)
        .open(path)
        .unwrap();

    let vertices = mesh
        .buffers
        .iter()
        .find(|x| x.r#type == "position")
        .unwrap();
    let vertices = vertices.as_f32();
    for v in vertices.chunks(3) {
        obj_v(&mut f, v[0], v[1], v[2]);
    }

    let indices = mesh.buffers.iter().find(|x| x.r#type == "index").unwrap();
    let indices = indices.as_u16();
    for i in indices.chunks(3) {
        obj_f(&mut f, i[0] + 1, i[1] + 1, i[2] + 1);
    }
}
