mod gltf;

use std::{io::Read, path::PathBuf, str::FromStr};

use crate::parser::NomSlice;

pub struct Bone {
    parent: usize,
    children: Vec<usize>,
    translation: [f32; 3],
    rotation: [f32; 4],
}

pub struct SklFile {
    pub bones: Vec<Bone>,
}

impl SklFile {
    pub fn new() -> Self {
        Self { bones: Vec::new() }
    }
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

    let mut skl = SklFile::new();

    let _ = input.read_properties();

    let _ = input.parse_le_u32("?");
    let qty = input.parse_le_u32("?") as usize;

    for i in 0..qty {
        let _ = input.read_n_properties(2);

        let parent = input.parse_le_u32("parent?") as usize;

        let translation = input.read_vec3("translation");
        let rotation = input.read_quat("rotation");

        let l = input.parse_le_u32("Bone Q Section Length");
        assert!(l == 32);
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("Bone Q");

        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");
        let _ = input.parse_le_f32("?");

        let _ = input.read_m33("Bone Basis");

        let _ = input.parse_le_u32("IK Section Length");
        let qty = input.parse_le_u32("IK qty");
        for _ in 0..qty {
            let _ = input.parse_length_string("Bone Name");
            let _ = input.parse_le_f32("?");
        }

        let _ = input.parse_le_f32("?");

        let bone = Bone {
            parent,
            children: Vec::new(),
            translation,
            rotation,
        };
        skl.bones.push(bone);
        if parent != (u32::MAX as usize) {
            skl.bones[parent].children.push(i);
        }
    }

    let out = PathBuf::from_str(path.as_ref()).unwrap();
    let out = out.with_extension("gltf");
    gltf::save(&out, skl);
}
