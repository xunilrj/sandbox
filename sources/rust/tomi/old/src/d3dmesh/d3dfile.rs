use json::JsonValue;

use crate::skl::SklFile;

#[derive(Debug)]
pub enum D3DBufferData {
    U8(Vec<u8>),
    U16(Vec<u16>),
    U32(Vec<u32>),
    F32(Vec<f32>),
}

pub struct D3DBuffer {
    pub r#type: String,
    pub qty: u32,
    pub stride: u32,
    pub data: D3DBufferData,
}

impl D3DBuffer {
    pub fn as_u8(&self) -> &Vec<u8> {
        match &self.data {
            D3DBufferData::U8(v) => v,
            _ => panic!("buffer is {:?}", self.data),
        }
    }

    pub fn as_u8_mut(&mut self) -> &mut Vec<u8> {
        match &mut self.data {
            D3DBufferData::U8(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_u16(&self) -> &Vec<u16> {
        match &self.data {
            D3DBufferData::U16(v) => v,
            _ => panic!("buffer is {:?}", self.data),
        }
    }

    pub fn as_f32(&self) -> &Vec<f32> {
        match &self.data {
            D3DBufferData::F32(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_f32_mut(&mut self) -> &mut Vec<f32> {
        match &mut self.data {
            D3DBufferData::F32(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_u32(&self) -> &Vec<u32> {
        match &self.data {
            D3DBufferData::U32(v) => v,
            _ => panic!(),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        match &self.data {
            D3DBufferData::U8(v) => v.as_slice(),
            D3DBufferData::U16(v) => unsafe {
                std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() / 2)
            },
            D3DBufferData::U32(v) => unsafe {
                std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() / 2)
            },
            D3DBufferData::F32(v) => unsafe {
                std::slice::from_raw_parts(v.as_ptr() as *const u8, v.len() / 4)
            },
        }
    }

    pub fn to_json_number(&self) -> Vec<JsonValue> {
        match &self.data {
            D3DBufferData::U8(v) => {
                let mut n = vec![];
                for v in v {
                    let number = json::number::Number::from(*v);
                    n.push(JsonValue::Number(number));
                }
                n
            }
            D3DBufferData::U16(v) => {
                let mut n = vec![];
                for v in v {
                    let number = json::number::Number::from(*v);
                    n.push(JsonValue::Number(number));
                }
                n
            }
            D3DBufferData::U32(v) => {
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

#[derive(Debug)]
pub struct D3DMap {
    pub r#type: String,
    pub name: String,
}

pub struct D3DMesh {
    pub bbox: D3DBoundingBox,
    pub maps: Vec<D3DMap>,
    pub vertices: [usize; 2],
    pub index_start: usize,
    pub tri_count: usize,
    pub bone_pallete: usize,
}

impl D3DMesh {
    pub fn new() -> Self {
        Self {
            bbox: D3DBoundingBox::default(),
            maps: Vec::with_capacity(16),
            vertices: [0; 2],
            index_start: 0,
            tri_count: 0,
            bone_pallete: 0,
        }
    }
}

#[derive(Debug)]
pub struct BonePallete {
    pub bones: Vec<usize>,
}

pub struct D3DFile {
    pub name: String,
    pub bbox: D3DBoundingBox,
    pub meshes: Vec<D3DMesh>,
    pub buffers: Vec<D3DBuffer>,
    pub palletes: Vec<BonePallete>,
    pub skl: Option<SklFile>,
}

impl D3DFile {
    pub fn new() -> Self {
        Self {
            name: "".to_string(),
            meshes: Vec::with_capacity(10),
            buffers: Vec::with_capacity(10),
            bbox: D3DBoundingBox::default(),
            palletes: vec![],
            skl: None,
        }
    }

    pub fn get_buffer(&self, name: &str) -> Option<&D3DBuffer> {
        self.buffers.iter().find(|x| x.r#type == name)
    }

    // Returns the nth buffer with a given name
    pub fn nth_buffer(&self, n: usize, name: &str) -> Option<&D3DBuffer> {
        self.buffers.iter().filter(|x| x.r#type == name).nth(n)
    }

    pub fn nth_buffer_mut(&mut self, n: usize, name: &str) -> Option<&mut D3DBuffer> {
        self.buffers.iter_mut().filter(|x| x.r#type == name).nth(n)
    }
}
