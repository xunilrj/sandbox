use crate::{attributes::read_att, indexbuffer};
use json::JsonValue;
use log::debug;
use std::{io::Read, str::FromStr};

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

#[allow(dead_code)]
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

#[allow(dead_code)]
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

#[allow(dead_code)]
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
    slice: &'a [u8],
    qty: usize,
}

impl<'a> NomSlice<'a> {
    pub fn new(slice: &'a [u8]) -> Self {
        Self { slice, qty: 0 }
    }

    #[inline(always)]
    pub fn parse_n_bytes(&mut self, n: usize) -> &[u8] {
        if let Ok((i, data)) = parse_n_bytes(self.slice, n) {
            self.slice = i;
            self.qty += n;

            let d: Vec<_> = data.iter().take(10).collect();
            debug!("read {} bytes: {:?}", n, d);
            data
        } else {
            panic!("parse_n_bytes");
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
    pub fn parse_le_u32(&mut self, name: &str) -> u32 {
        let (i, data) = parse_le_u32(self.slice).unwrap();
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
    pub fn parse_le_f32(&mut self, name: &str) -> f32 {
        let (i, data) = parse_le_f32(self.slice).unwrap();
        self.slice = i;
        self.qty += 4;

        debug!("[{}] as f32: {}", name, data);
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
    pub fn parse_string(&mut self, size: usize) -> &str {
        let (i, data) = parse_n_bytes(self.slice, size).unwrap();
        let data = std::str::from_utf8(data).unwrap();
        self.slice = i;
        self.qty += size;

        debug!("read string: {} {}", size, data);
        data
    }

    #[inline(always)]
    #[allow(dead_code)]
    pub fn parse_u16_slice(&mut self, n: usize) -> &[u16] {
        let bytes_size = std::mem::size_of::<u16>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;

        let d: Vec<_> = data.iter().take(10).collect();
        debug!("read {} u16: {:?}", n, bytes_size);

        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const u16, n) }
    }

    #[inline(always)]
    pub fn parse_f32_slice(&mut self, n: usize) -> &[f32] {
        let bytes_size = std::mem::size_of::<f32>() * n;
        let (i, data) = parse_n_bytes(self.slice, bytes_size).unwrap();
        self.slice = i;
        self.qty += bytes_size;

        let d: Vec<_> = data.iter().take(10).collect();
        debug!("read {} f32: {:?}", n, bytes_size);

        unsafe { std::slice::from_raw_parts(data.as_ptr() as *const f32, n) }
    }
}

pub enum D3DBufferData {
    U16(Vec<u16>),
    F32(Vec<f32>),
}

pub struct D3DBuffer {
    pub r#type: String,
    pub qty: u32,
    pub stride: u32,
    pub data: D3DBufferData,
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

    pub fn as_bytes(&self) -> &[u8] {
        match &self.data {
            D3DBufferData::U16(v) => unsafe {
                std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() / 2)
            },
            D3DBufferData::F32(v) => unsafe {
                std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() / 4)
            },
        }
    }

    pub fn to_json_number(&self) -> Vec<JsonValue> {
        match &self.data {
            D3DBufferData::U16(v) => {
                let mut n = vec![];
                for v in v {
                    let number = json::number::Number::from(*v);
                    n.push(JsonValue::Number(number));
                }
                n
            }
            D3DBufferData::F32(v) => {
                let mut n = vec![];
                for v in v {
                    let number = json::number::Number::from(*v);
                    n.push(JsonValue::Number(number));
                }
                n
            }
        }
    }
}

#[derive(Default)]
pub struct D3DBoundingBox {
    pub minx: f32,
    pub miny: f32,
    pub minz: f32,
    pub maxx: f32,
    pub maxy: f32,
    pub maxz: f32,
}

pub struct D3DMap {
    pub r#type: String,
    pub name: String,
}

pub struct D3DMesh {
    pub bbox: D3DBoundingBox,
    pub maps: Vec<D3DMap>,
    pub vertices: [usize; 2],
    pub indices: [usize; 2],
}

impl D3DMesh {
    pub fn new() -> Self {
        Self {
            bbox: D3DBoundingBox::default(),
            maps: Vec::with_capacity(16),
            vertices: [0; 2],
            indices: [0; 2],
        }
    }
}

pub struct D3DFile {
    pub name: String,
    pub bbox: D3DBoundingBox,
    pub meshes: Vec<D3DMesh>,
    pub buffers: Vec<D3DBuffer>,
}

impl D3DFile {
    pub fn new() -> Self {
        Self {
            name: "".to_string(),
            meshes: Vec::with_capacity(10),
            buffers: Vec::with_capacity(10),
            bbox: D3DBoundingBox::default(),
        }
    }
}

fn parse_d3dmesh_buffer(input: &mut NomSlice) -> D3DBuffer {
    input.qty = 0;

    let qty = input.parse_le_u32("Buffer Qty");
    let stride = input.parse_le_u32("Buffer Stride");
    let t = input.parse_le_u32("Buffer Type");
    let _ = input.parse_le_u32("?");
    let data = input.parse_f32_slice(((stride * qty) / 4) as usize);

    let t = match t {
        1 => "position".to_string(),
        2 => "normal".to_string(),
        _ => format!("{}", t),
    };

    D3DBuffer {
        r#type: t,
        qty,
        stride,
        data: D3DBufferData::F32(data.to_vec()),
    }
}

pub fn parse_d3dfile<S: AsRef<str>>(
    path: S,
    output: Option<String>,
    pretty_print: bool,
    buffer_as_base64: bool,
    detach_index_buffer: bool,
) -> std::result::Result<(), ParseD3dMeshError> {
    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

    let mut d3dfile = D3DFile::new();

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

    let param_count = input.parse_le_u32("?");
    let _param_hash = input.parse_n_bytes(8);

    for _ in 0..(param_count - 1) {
        let _ = input.parse_le_u32("?");
        let _ = input.parse_n_bytes(8);
    }

    let _ = input.parse_le_u32("?");
    let _header_len = input.parse_le_u32("?");
    let d3d_name = input.parse_length_string("?");

    d3dfile.name = d3d_name.to_string();

    let _ = input.parse_length_string("?");

    let minx = input.parse_le_f32("bbox.minx");
    let miny = input.parse_le_f32("bbox.miny");
    let minz = input.parse_le_f32("bbox.minz");
    let maxx = input.parse_le_f32("bbox.maxx");
    let maxy = input.parse_le_f32("bbox.maxy");
    let maxz = input.parse_le_f32("bbox.maxz");
    d3dfile.bbox = D3DBoundingBox {
        minx: minx,
        miny: miny,
        minz: minz,
        maxx: maxx,
        maxy: maxy,
        maxz: maxz,
    };

    let _header_size = input.parse_le_u32("?");
    let qty_meshes = input.parse_le_u32("?");

    for i in 0..qty_meshes {
        debug!("Mesh: {}", i);
        bar.reach_percent(25);
        let mut mesh = D3DMesh::new();

        input.qty = 0;

        let _header_hash = input.parse_le_u64("Mesh header hash");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u64("?");

        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_u32("?");

        mesh.vertices[0] = input.parse_le_u32("Vertex Start") as usize;
        mesh.vertices[1] = input.parse_le_u32("Vertex End") as usize;

        mesh.indices[0] = input.parse_le_u32("Index Start") as usize;
        mesh.indices[1] = 0;

        let _ = input.parse_le_u32("polygon count?");
        let _ = input.parse_le_u64("section header");

        const ATT_BOUNDING_BOX: u32 = 0;
        const ATT_DIFFUSE_MAP: u32 = 25;
        for _ in 0..9 {
            let att = crate::attributes::read_att(&mut input);
            match att {
                crate::attributes::Attribute::BoundingBox(minx, miny, minz, maxx, maxy, maxz) => {
                    mesh.bbox = D3DBoundingBox {
                        minx,
                        miny,
                        minz,
                        maxx,
                        maxy,
                        maxz,
                    }
                }
                _ => {}
            }
        }

        let _ = input.parse_n_bytes(1);
        let _ = read_att(&mut input);
        let _ = input.parse_le_u32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32("?");

        // material and parameters?
        let _ = input.parse_n_bytes(1);
        let _ = read_att(&mut input);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_u32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);
        let _ = input.parse_le_u32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.parse_n_bytes(1);
        let _ = input.parse_n_bytes(1);

        let _ = input.parse_length_string("?");
        let _ = input.parse_length_string("?");

        d3dfile.meshes.push(mesh);
    }

    for _ in 0..3 {
        let _ = crate::attributes::read_att2(&mut input);
    }

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);
    let _ = input.parse_n_bytes(1);

    let _ = input.parse_le_u32("?");

    let _ = input.parse_n_bytes(1);
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");

    for _ in 0..2 {
        let _ = crate::attributes::read_att(&mut input);
    }

    let _ = input.parse_n_bytes(1);

    // Buffers

    bar.reach_percent(50);

    let _ = input.parse_le_u32("?");
    let qty_indices = input.parse_le_u32("IB Qty Indices");
    let a = input.parse_le_u32("?"); //              ESP+18
    let first_index = input.parse_le_u16("IB First Index"); // first index? ESP+14

    let buffer_size = input.parse_le_u32("IB Buffer Size");
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
    let index_buffer_bytes = indexbuffer::get_index_buffer(
        index_buffer,
        &mut esp14,
        &mut esp18,
        &mut esp20,
        &mut esp40,
        &mut esi1c,
        &mut esi20,
    );
    debug!("converting index buffer");
    let index_buffer = unsafe {
        std::slice::from_raw_parts(
            index_buffer_bytes.as_ptr() as *const u16,
            index_buffer_bytes.len() / 2,
        )
    };

    debug!("writing index buffer");
    d3dfile.buffers.push(D3DBuffer {
        r#type: "index".to_string(),
        qty: qty_indices,
        stride: 2,
        data: D3DBufferData::U16(index_buffer.to_vec()),
    });
    debug!("done.");

    // Fix indices
    for i in 0..d3dfile.meshes.len() {
        let next = d3dfile
            .meshes
            .get(i + 1)
            .map(|x| x.indices[0] - 1)
            .unwrap_or(qty_indices as usize);
        d3dfile.meshes[i].indices[1] = next;
    }

    // Read buffers

    while input.slice.len() != 0 {
        bar.reach_percent(75);
        let buffer = parse_d3dmesh_buffer(&mut input);
        d3dfile.buffers.push(buffer);
    }

    bar.reach_percent(100);
    let mut bar = progress::Bar::new();
    bar.set_job_title("Saving...");
    match output {
        Some(output) => {
            crate::outputs::save_to(&d3dfile, output.as_str(), buffer_as_base64, pretty_print);

            let output = std::path::PathBuf::from_str(output.as_str()).unwrap();
            let output = output.with_extension("ib");

            // if detach_index_buffer {
            //     use std::str::FromStr;
            //     std::fs::write(output, index_buffer_bytes);
            // }
        }
        None => {
            todo!("print to stdout")
            // if !pretty_print {
            //     println!("{}", json);
            // } else {
            //     println!("{}", json.pretty(4));
            // }
        }
    }

    bar.set_job_title("Done");
    bar.reach_percent(100);

    Ok(())
}
