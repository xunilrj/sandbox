use std::io::Read;

use crate::parser::{self, NomSlice};

pub struct AnmFile {}

impl AnmFile {
    pub fn new() -> Self {
        Self {}
    }
}

pub struct Something {
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

    let _ = input.read_properties();

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_u64("?");

    let _ = input.parse_le_u32("?");
    let _ = input.parse_le_f32("duration?");

    let anim_length = input.parse_le_u32("animation length in bytes?");

    let qty_bones = input.parse_le_u32("qtd bones?") as usize;
    let length = input.parse_le_u32("length?");
    let something_qty = input.parse_le_u32("qty");

    let mut somethings = vec![];
    for _ in 0..something_qty {
        let hash = input.parse_le_u64("?");
        let a = input.parse_le_u32("?");
        let qty = input.parse_le_u16("?");
        let b = input.parse_le_u16("?");

        let i = Something { hash, qty, a, b };
        somethings.push(i);
    }

    // 0
    if let Some(s) = somethings.get(0) {
        let buffers = s.qty;
        for _ in 0..buffers {
            let buffer = input.parse_length1_buffer("?");
            println!("{} {:?}", buffer.len(), buffer);
            let buffer = input.parse_length1_buffer("?");
            println!("{} {:?}", buffer.len(), buffer);
            println!("");
        }
    }

    // 1
    if let Some(s) = somethings.get(1) {
        let floats = s.qty;
        for _ in 0..floats {
            let a = input.read_quat("quat");
        }
    }

    // 2
    if let Some(s) = somethings.get(2) {
        let floats = s.qty;
        for _ in 0..floats {
            let _ = input.read_quat("quat");
            let _ = input.read_vec3("vec3");
        }
    }

    let a = input.parse_f32_slice(qty_bones);
    println!("{} {:?} {}", a.len(), a, a.iter().sum::<f32>());
    println!("");

    let _ = input.parse_le_u16("?");

    let _ = input.read_n_properties(qty_bones);
}
