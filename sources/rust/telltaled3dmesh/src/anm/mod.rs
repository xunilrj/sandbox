use std::io::Read;

use crate::parser::{self, NomSlice};

pub struct AnmFile {}

impl AnmFile {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct Buffer {
    hash: u64,
    a: u32,
    qty: u16,
    b: u16,
}

pub fn convert<P: AsRef<str>>(path: P) {
    let mut bar = progress::Bar::new();
    bar.set_job_title("Parsing...");

    let mut f = std::fs::File::open(path.as_ref()).unwrap();
    let bytes = {
        let mut bytes = vec![];
        f.read_to_end(&mut bytes).unwrap();
        bytes
    };

    let mut input = NomSlice::new(bytes.as_slice());

    if !input.read_ertm_magic_number() {
        panic!("ERTM not found")
    }

    let mut anm = AnmFile::new();

    let header = input.read_properties();
    println!("{:?}", header);

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u64("?");

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_f32("duration?");

    let anim_length = input.parse_le_u32("animation length in bytes?");

    let qty_values = input.parse_le_u32("qtd values") as usize;
    let length = input.parse_le_u32("length?");
    let qty_buffers = input.parse_le_u32("qty");

    let mut buffers = vec![];
    for _ in 0..qty_buffers {
        let hash = input.parse_le_u64("?");
        let hbytes = hash.to_le_bytes();
        let (hbytes, a) = parser::parse_le_u32(&hbytes[..]).unwrap();
        let (_, b) = parser::parse_le_u32(hbytes).unwrap();
        println!("{} {}", a, b);
        let a = input.parse_le_u32("?");
        let qty = input.parse_le_u16("?");
        let b = input.parse_le_u16("?");

        let i = Buffer { hash, qty, a, b };
        buffers.push(i);
    }

    
    for buffer in buffers {
        println!("buffer: {:?}", buffer.hash);
        match buffer.hash {
            //CompressedTransformKeys
            0xFC6597EB1FE5458E => {
                for i in 0..buffer.qty {
                    let length = input.parse_n_bytes(1);
                    let length = if length[0] == 255 {
                        input.parse_le_u16("length")
                    } else {
                        length[0] as u16
                    };

                    let mut header = input.parse_n_bytes(length as usize);
                    let data = input.parse_length1_buffer("?");
                }
            }
            0x6B77C806C0E23EA1 => {
                for _ in 0..buffer.qty {
                    let v = input.read_vec3("vec3");
                }
            }
            // SingleValue<Quaternion>
            0xCECACE3A835CB7EE => {
                for _ in 0..buffer.qty {
                    let q = input.read_quat("quat");
                }
            }
            // SingleValue<Transform>
            0xC1E84D6FF72CE80 => {
                for _ in 0..buffer.qty {
                    let _ = input.read_quat("quat");
                    let _ = input.read_vec3("vec3");
                }
            }
            _ => todo!("{:?}", buffer.hash)
        }
    }
    
    let _ = input.parse_u32_slice(qty_values);
    let skipSymbols = input.parse_le_u16("?");
    if skipSymbols == 0 {
        let _ = input.read_n_properties(qty_values);
    }
}
